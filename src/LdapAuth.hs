{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module LdapAuth 
       (ldapAuth, 
        dummyAuth,
        userName,
        adminRole
       ) where

import           Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as B

import           Data.Text(Text)
import qualified Data.Text as T

import qualified Data.HashMap.Strict as HM

import           Data.Char (isAlphaNum,toLower)

import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans
import           Control.Monad

import           Data.Aeson.Types 

import           LDAP

import           Data.Time.Clock

import           Snap.Snaplet.Auth

import           Types


type MaybeIO = MaybeT IO 

-- LDAP atributes list
type Attrs = [(String, [String])]

-- attempt LDAP bind to check user password and search for LDAP attributes
ldapBindSearch :: LDAP -> String -> String -> MaybeIO Attrs
ldapBindSearch con dn passwd 
  = MaybeT $ catchLDAP bind (\_ -> return Nothing)
  where bind = 
          do ldapSimpleBind con dn passwd
             lst <- ldapSearch con (Just dn) LdapScopeSubtree Nothing LDAPAllUserAttrs False
             case lst of
               [] -> return Nothing
               (le:_) -> return (Just $ leattrs le)


-- attempt to authenticate using LDAP
ldapAuth :: IAuthBackend r => 
            r -> LdapConf -> ByteString -> ByteString -> IO (Maybe AuthUser)
ldapAuth r conf user passwd 
  = runMaybeT (ldapAuth' r conf (sanitize user) passwd)
  
    
-- keep only alphanumeric chars and lowercase all letters
sanitize :: ByteString -> ByteString 
sanitize  = B.fromString . map toLower . filter isAlphaNum . B.toString

ldapAuth' :: IAuthBackend r => 
            r -> LdapConf -> ByteString -> ByteString -> MaybeIO AuthUser
ldapAuth' r LdapConf{..} user passwd 
  = do now <- liftIO getCurrentTime
       con <- liftIO (ldapInitialize ldapURI)
       attrs <- msum [ldapBindSearch con dn (B.toString passwd) | dn<-dns]
       MaybeT (lookupByLogin r login) 
         `mplus` do au <- liftIO (save r (newUser now attrs))
                    either (const $ fail "no login") return au
  where 
        dns = ["uid=" ++ B.toString user ++ "," ++ base | base<-ldapBases]
        login = T.pack $ B.toString user 
        newUser time attrs = 
          defAuthUser { userLogin = login
                      , userPassword = Nothing
                      , userCreatedAt = Just time
                      , userUpdatedAt = Just time
                      , userCurrentLoginAt = Just time                   
                      , userMeta = HM.fromList [(T.pack k, String $ T.pack v) 
                                               | (k,v:_)<-attrs, keepAttrs k]
                      , userRoles = if login `elem` ldapAdmins
                                    then [adminRole] else []
                      }  

-- | Administrator role
adminRole :: Role
adminRole = Role "admin"


-- which LDAP attributes to keep?
keepAttrs :: String -> Bool
keepAttrs "gecos" = True
keepAttrs "cn"    = True
keepAttrs "sn"    = True
keepAttrs "givenName" = True
keepAttrs _        = False

  

-- fetch the user name of an authenticated user
userName :: AuthUser -> Text
userName au 
  = case HM.lookup (T.pack "gecos") (userMeta au) of
    Just (String name) -> name
    _                  -> userLogin au  -- fallback: give the user login

-----------------------------------------------------------
-- dummy password-less login
-- for development only!

dummyAuth :: IAuthBackend r => 
             r -> LdapConf -> ByteString -> ByteString -> IO (Maybe AuthUser)
dummyAuth r LdapConf{..} username passwd
  = do now <- getCurrentTime
       let login = T.pack $ B.toString username
       optAu <- lookupByLogin r login 
       let newuser = defAuthUser {
               userId = optAu >>= userId
             , userLogin = login
             , userPassword = Nothing
             , userCreatedAt = (optAu >>= userCreatedAt) `mplus` Just now
             , userUpdatedAt = Just now
             , userRoles = if login `elem` ldapAdmins
                           then [adminRole] else []
             }
       eithAu <- save r newuser
       return (either (const Nothing) Just eithAu)
       

