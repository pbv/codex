{-# LANGUAGE OverloadedStrings #-}

module Codex.AuthHandlers (
  handleLogin,
  handleLogout,
  handleRegister
  ) where

import           Data.ByteString.UTF8 (ByteString)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Map.Syntax
import qualified Data.HashMap.Strict as HM
import           Data.Aeson (Value(..))

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Except

import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist

import qualified Heist.Interpreted as I

import qualified Data.Configurator as Configurator

import           Codex.Types
import           Codex.Config
import           Codex.Utils
import           Codex.Application
import           Codex.LdapAuth

------------------------------------------------------------------------------
-- | Handle login requests
handleLogin :: Codex ()
handleLogin =
  method GET (handleLoginForm "login" Nothing) <|>
  method POST handleLoginSubmit


-- | Render login and signup forms
handleLoginForm ::  ByteString -> Maybe AuthFailure -> Codex ()
handleLoginForm file authFail = heistLocal (I.bindSplices errs) $ render file
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
  login <-  require (getParam "login")
  passwd <- require (getParam "password")
  ldap <- getLdap
  r <- with auth $ loginByUsername (T.decodeUtf8 login) (ClearText passwd) False
  case r of
    Right au -> redirect "/"
    Left err -> case ldap of
      Nothing -> handleLoginForm "login" (Just err)
      Just cfg -> loginLdapUser cfg login passwd


loginLdapUser :: LdapConf -> ByteString -> ByteString -> Codex ()
loginLdapUser ldapConf login passwd = do
  r <- with auth $ withBackend (\r -> liftIO $ ldapAuth r ldapConf login passwd)
  case r of
    Left err -> handleLoginForm "login" (Just err)
    Right au -> with auth (forceLogin au) >> redirect "/"



------------------------------------------------------------------------------

-- | Handle sign-in requests
handleRegister :: Codex ()
handleRegister =
  method GET (handleLoginForm "register" Nothing) <|>
  method POST handleSubmit
  where
    handleSubmit = do
      r <- with auth newUser
      case r of
        Left err -> handleLoginForm "register" (Just err)
        Right au -> with auth (forceLogin au) >> redirect "/pub/index.md"


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
-- in exam mode procedeed to printout
handleLogout :: Codex ()
handleLogout = method GET $ do
  uid <- require getUserID <|> unauthorized
  with auth logout
  redirect "/"
