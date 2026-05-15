{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-
 - Handlers for authentication related stuff
-}
module Codex.AuthHandlers (
  handleLogin,
  handleLogout,
  handleRegister,
  getRemainingTime,
  ) where

import           Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Map.Syntax ( (##) )
import qualified Data.HashMap.Strict as HM
import           Data.Aeson.Types (typeMismatch)
import           Data.Aeson (Value(..), Result(..),
                              fromJSON, toJSON, parseJSON,
                              FromJSON)
import           Data.Maybe (fromMaybe)

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import Control.Monad.Trans.Except
    ( ExceptT(ExceptT), runExceptT, throwE, catchE )

import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Router

import qualified Heist.Interpreted as I

import qualified Data.Configurator.Types as Configurator
import qualified Data.Configurator as Configurator

import           Data.Time.Clock(getCurrentTime, diffUTCTime, UTCTime, NominalDiffTime)
import           Data.Time.Format(readSTime, defaultTimeLocale)

import           Codex.Types
import           Codex.Utils
import           Codex.Application
import           Codex.LdapAuth


------------------------------------------------------------------------------
-- | Handle login requests
handleLogin :: Codex ()
handleLogin = do
  opt <- with auth currentUser
  case opt of
    Just _ -> redirectURL home
    Nothing -> method GET (loginForm "_login" Nothing) <|>
               method POST handleLoginSubmit


-- | Render login and register form
loginForm :: ByteString -> Maybe AuthFailure -> Codex ()
loginForm form authFail = heistLocal (I.bindSplices errs) $ render form
  where
    errs = "loginError" ## maybe (return []) (I.textSplice . T.pack .show) authFail


getLdap :: Codex (Maybe LdapConf)
getLdap = do
  conf <- getSnapletUserConfig
  liftIO $ getLdapConf (Configurator.subconfig "users.ldap" conf)


-- | handler for login attempts
-- first try local user DB, then LDAP (if enabled)
handleLoginSubmit :: Codex ()
handleLoginSubmit = do
  user  <-  require (getParam "login")
  passwd <- require (getParam "password")
  ldap <- getLdap
  r <- with auth $ loginByUsername (T.decodeUtf8 user) (ClearText passwd) False
  case r of
    Right au -> do
      addr <- getsRequest rqClientAddr
      logMsg (user <> " logged in from " <> addr)
      registerTimer au
      redirectURL home
    Left err -> case ldap of
      Nothing ->  loginForm "_login" (Just err)
      Just cfg -> loginLdapUser cfg user passwd


loginLdapUser :: LdapConf -> ByteString -> ByteString -> Codex ()
loginLdapUser ldapConf user passwd = do
  r <- with auth $ withBackend (\r -> liftIO $ ldapAuth r ldapConf user passwd)
  case r of
    Left err -> 
      loginForm "_login" (Just err)
    Right au -> do
      addr <- getsRequest rqClientAddr
      logMsg (user <> " logged in from " <> addr)
      with auth (forceLogin au)
      registerTimer au
      redirectURL home


-- register the exam timer start (if not set already)
registerTimer :: AuthUser -> Codex ()
registerTimer au@AuthUser{..}
  | HM.member "timerStart" userMeta = return ()
  | otherwise = do
      time <- liftIO getCurrentTime
      let au' = au { userMeta = HM.insert "timerStart" (toJSON time) userMeta }
      result <- with auth (saveUser au')
      case result of
        Right _ -> 
          logMsg (T.encodeUtf8 userLogin <> " started exam")
        Left _ -> 
          logMsg (T.encodeUtf8 userLogin <> " update failed")


{-
-- get the elapsed time for the currently logged user
getElapsedTime :: Codex (Maybe NominalDiffTime)
getElapsedTime = do
  au <- require (with auth currentUser) <|> unauthorized
  now <- liftIO getCurrentTime
  return $ case getUserTimer au of
    Nothing -> Nothing
    Just Timer{..} ->
      Just (diffUTCTime now timerStart)
-}
  
-- get the remaining time for the currently logged user
getRemainingTime :: Codex (Maybe NominalDiffTime)
getRemainingTime = do
  au <- require (with auth currentUser) <|> unauthorized
  now <- liftIO getCurrentTime
  conf <- getSnapletUserConfig
  opt <- liftIO $ Configurator.lookup conf "timer.duration"
  return $ case opt of
    Nothing -> Nothing
    Just (DiffTime duration) -> 
      case getUserTimer au of
        Nothing -> Nothing
        Just Timer{..} ->
          Just (duration + timerBonus - diffUTCTime now timerStart)


-- | a newtype for converting time differences
newtype DiffTime = DiffTime NominalDiffTime

instance Configurator.Configured DiffTime where
  convert (Configurator.String txt)
    = DiffTime <$> parseDiffTime (T.unpack txt)
  convert _ = Nothing

instance FromJSON DiffTime where
  parseJSON (String txt)
    = case parseDiffTime (T.unpack txt) of
        Nothing -> fail "invalid time diff"
        Just diff -> return (DiffTime diff)
  parseJSON v = typeMismatch "String" v
                             

parseDiffTime :: String -> Maybe NominalDiffTime
parseDiffTime txt
  = case [ t | (t,"")<-readSTime True defaultTimeLocale "%H:%M" txt ] of
      (t:_) -> Just t
      _ -> Nothing

data Timer = Timer { timerStart :: UTCTime
                   , timerBonus :: NominalDiffTime 
                   }
 

getUserTimer :: AuthUser -> Maybe Timer
getUserTimer au = do
  start <- getUserMeta "timerStart" au
  let DiffTime bonus = fromMaybe (DiffTime 0) (getUserMeta "timerBonus" au)
  return (Timer start bonus)

getUserMeta :: FromJSON a => Text -> AuthUser -> Maybe a
getUserMeta key au = do
  val <- HM.lookup key (userMeta au)
  resultToMaybe (fromJSON val)


resultToMaybe :: Result a -> Maybe a
resultToMaybe (Success x) = Just x
resultToMaybe (Error _) = Nothing


------------------------------------------------------------------------------

-- | Handle sign-in requests
handleRegister :: Codex ()
handleRegister = do
  conf <- getSnapletUserConfig
  c <- liftIO $ Configurator.require conf "users.register"
  unless c unauthorized
  method GET (loginForm "_register" Nothing) <|>
    method POST handleSubmit
  where
    handleSubmit = do
      r <- with auth newUser
      case r of
        Left err -> loginForm "_register" (Just err)
        Right au -> with auth (forceLogin au) >> redirectURL home


------------------------------------------------------------------------------
-- | Register a new user
--
newUser :: Handler b (AuthManager b) (Either AuthFailure AuthUser)
newUser = do
    l <- fmap T.decodeUtf8 <$> getParam "login"
    p <- getParam "password"
    p2<- getParam "password2"
    n <- fmap T.decodeUtf8 <$> getParam "fullname"
    e <- fmap T.decodeUtf8 <$> getParam "email"
    runExceptT $ do
      login  <- maybe (throwE UsernameMissing) return l
      passwd <- maybe (throwE PasswordMissing) return p
      passwd2<- maybe (throwE PasswordMissing) return p2
      name <- maybe (throwE (AuthError "Missing full user name")) return n
      email <- maybe (throwE (AuthError "Missing user email")) return e
      when (passwd /= passwd2) $
        throwE (AuthError "Passwords fields do not match")
      au <- ExceptT (createUser login passwd)
      let meta' = HM.fromList [("fullname", String name)]
      let au' = au { userEmail = Just email, userMeta = meta'}
      ExceptT (saveUser au') `catchE` (\err -> case err of
                                          DuplicateLogin -> return au'
                                          err -> throwE err)




------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Codex ()
handleLogout = method GET $ do
  uid <- require getUserLogin <|> unauthorized
  with auth logout
  let user = B.fromString $ T.unpack $ fromLogin uid
  addr <- getsRequest rqClientAddr
  logMsg (user <> " logged out from " <> addr)
  redirectURL Login


home :: AppUrl
home = Page ["index.md"]


