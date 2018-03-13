{-# LANGUAGE OverloadedStrings #-}

module Codex.AuthHandlers (
  handleLogin,
  handleLogout,
  handleRegister
  ) where

import           Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Map.Syntax
import qualified Data.HashMap.Strict as HM
import           Data.Aeson (Value(..))

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Except

import           Data.Monoid

import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Router

import qualified Heist.Interpreted as I

import qualified Data.Configurator as Configurator

import           Codex.Types
import           Codex.Utils
import           Codex.Application
import           Codex.LdapAuth

------------------------------------------------------------------------------
-- | Handle login requests
handleLogin :: Codex ()
handleLogin =
  method GET (loginForm "_login" Nothing) <|>
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
      redirectURL home
    Left err -> case ldap of
      Nothing ->  loginForm "_login" (Just err)
      Just cfg -> loginLdapUser cfg user passwd


loginLdapUser :: LdapConf -> ByteString -> ByteString -> Codex ()
loginLdapUser ldapConf user passwd = do
  r <- with auth $  withBackend (\r -> liftIO $ ldapAuth r ldapConf user passwd)
  case r of
    Left err -> 
      loginForm "_login" (Just err)
    Right au -> do
      addr <- getsRequest rqClientAddr
      logMsg (user <> " logged in from " <> addr)
      with auth (forceLogin au)
      redirectURL home



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
