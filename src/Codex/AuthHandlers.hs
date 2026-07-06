{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-
 - Handlers for authentication related stuff
-}
module Codex.AuthHandlers (
  handleLogin,
  handleLogout,
  handleRegister,
  getTimeRemaining,
  getUserTranslate,
  checkTimeRemaining,
  getUserMeta,
  setUserMeta
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
                              FromJSON, ToJSON)
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

-- | a newtype for converting time differences into json/configuration text
newtype DiffTime = DiffTime NominalDiffTime

instance Configurator.Configured DiffTime where
  convert (Configurator.String txt)
    = DiffTime <$> parseDiffTime (T.unpack txt)
  convert _ = Nothing

instance FromJSON DiffTime where
  parseJSON (String txt)
    = case parseDiffTime (T.unpack txt) of
        Just diff -> return (DiffTime diff)
        Nothing -> fail "invalid time diff"
  parseJSON v = typeMismatch "String" v
                             
parseDiffTime :: String -> Maybe NominalDiffTime
parseDiffTime txt
  = case ([t | (t,"")<-readSTime True defaultTimeLocale "%H:%M" txt] ++
          [t | (t,"")<-readSTime True defaultTimeLocale "%ss" txt]) of
      (diff:_) -> Just diff
      _ -> Nothing


-- | the exam time information for a particular user
data Timer = Timer { firstLogin  :: UTCTime
                   , timeDiffExtra :: NominalDiffTime 
                   }

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


-- | get the desired translation language from the login page
getTranslate :: Codex (Maybe String)
getTranslate = 
  fmap
    (\bs -> if bs == "" then Nothing else Just (B.toString bs))
    (require $ getParam "translate")

    

-- | handler for login attempts
-- first try local user DB, then LDAP (if enabled)
handleLoginSubmit :: Codex ()
handleLoginSubmit = do
  user  <-  require (getParam "login")
  passwd <- require (getParam "password")
  translate <- getTranslate
  ldap <- getLdap
  r <- with auth $ loginByUsername (T.decodeUtf8 user) (ClearText passwd) False
  case r of
    Right au -> do
      addr <- getsRequest rqClientAddr
      logMsg (user <> " logged in from " <> addr)
      au' <- registerFirstLogin au
      _ <- setUserMeta "translate" translate au'
      redirectURL home
    Left err -> case ldap of
      Nothing ->  loginForm "_login" (Just err)
      Just cfg -> loginLdapUser cfg user passwd translate


loginLdapUser :: LdapConf -> ByteString -> ByteString -> Maybe String
              -> Codex ()
loginLdapUser ldapConf user passwd translate = do
  r <- with auth $ withBackend (\r -> liftIO $ ldapAuth r ldapConf user passwd)
  case r of
    Left err -> 
      loginForm "_login" (Just err)
    Right au -> do
      addr <- getsRequest rqClientAddr
      logMsg (user <> " logged in from " <> addr)
      with auth (forceLogin au)
      au' <- registerFirstLogin au
      _ <- setUserMeta "translate" translate au'
      redirectURL home


-- register the first login (if not set already)
registerFirstLogin :: AuthUser -> Codex AuthUser
registerFirstLogin au@AuthUser{..}
  | HM.member "firstLogin" userMeta = return au
  | otherwise = do
      time <- liftIO getCurrentTime
      result <- setUserMeta "firstLogin" time au
      case result of
        Right au' -> do
          logMsg (T.encodeUtf8 userLogin <> " first login updated")
          return au'
        Left _ -> do
          logMsg (T.encodeUtf8 userLogin <> " first login update FAILED")
          return au


  
-- get the remaining time for the currently logged user
getTimeRemaining :: Codex (Maybe NominalDiffTime)
getTimeRemaining = do
  au <- require (with auth currentUser) <|> unauthorized
  now <- liftIO getCurrentTime
  conf <- getSnapletUserConfig
  optDuration <- liftIO $ Configurator.lookup conf "timer.duration"
  return $ do
    DiffTime duration <- optDuration
    Timer start extra <- getUserTimer au
    return (duration + extra - diffUTCTime now start)


getUserTranslate :: Codex (Maybe String)
getUserTranslate =  do
  au <- require (with auth currentUser) <|> unauthorized
  return (getUserMeta "translate" au)


-- | check that the current use stil has time remaining
checkTimeRemaining :: Codex Bool
checkTimeRemaining = do
  optDiff <- getTimeRemaining
  return $ case optDiff of
    Nothing -> True
    Just diff -> diff>0


getUserTimer :: AuthUser -> Maybe Timer
getUserTimer au = do
  start <- getUserMeta "firstLogin" au
  let DiffTime extra = fromMaybe (DiffTime 0) (getUserMeta "timeDiffExtra" au)
  return Timer { firstLogin = start, timeDiffExtra = extra }

getUserMeta :: FromJSON a => Text -> AuthUser -> Maybe a
getUserMeta key au = do
  val <- HM.lookup key (userMeta au)
  resultToMaybe (fromJSON val)

setUserMeta :: ToJSON a
            => Text -> a -> AuthUser -> Codex (Either AuthFailure AuthUser)
setUserMeta key val au = do
  let au' = au { userMeta = HM.insert key (toJSON val) (userMeta au) }
  with auth (saveUser au')

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


