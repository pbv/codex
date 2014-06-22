{-# LANGUAGE RecordWildCards #-}
module LdapAuth 
       (ldapAuth, 
        dummyAuth,
        userName
       ) where

import           Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as B

import           Data.Text(Text)
import qualified Data.Text as T
-- import qualified Data.Text.Encoding as T
-- import qualified Data.Text.Encoding.Error as T

import qualified Data.HashMap.Strict as HM

import           Data.Maybe

import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans
import           Control.Monad

import           Data.Aeson.Types 

import           LDAP
-- import           LDAP.Init
-- import           LDAP.Exceptions

import           Data.Time.Clock

import           Snap.Snaplet.Auth

import           Types


type MaybeIO = MaybeT IO 

-- LDAP atributes list
type Attrs = [(String,[String])]

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
ldapAuth r conf user passwd = runMaybeT (ldapAuth' r conf user passwd)

ldapAuth' :: IAuthBackend r => 
            r -> LdapConf -> ByteString -> ByteString -> MaybeIO AuthUser
ldapAuth' r LdapConf{..} user passwd 
  = do con <- liftIO $ ldapInit ldapHost ldapPort
       attrs <- msum [ldapBindSearch con dn (B.toString passwd) | dn<-dns]
       now <- liftIO getCurrentTime
       let au = newUser now attrs
       MaybeT (lookupByLogin r login) 
         `mplus` do au' <- liftIO (save r au)
                    either (const $ fail "no login") return au'
  where 
        dns = ["uid=" ++ B.toString user ++ "," ++ base | base<-ldapBases]
        login = T.pack $ B.toString user 
        newUser time attrs = 
          defAuthUser { userLogin = login'
                      , userPassword = Nothing
                      , userCreatedAt = Just time
                      , userUpdatedAt = Just time
                      , userMeta = HM.fromList [(T.pack k, String $ T.pack v) 
                                               | (k,v:_)<-attrs, keepAttrs k]
                      }  
          -- lookup cannonical user login
          where login' = maybe login T.pack $ 
                         (lookup "uid" attrs >>= listToMaybe)


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
dummyAuth r _ login _
  = do now <- getCurrentTime
       let login' = T.pack $ B.toString login
       let newuser = defAuthUser { userLogin = login'
                                 , userPassword = Nothing
                                 , userCreatedAt = Just now
                                 , userUpdatedAt = Just now
                                 }
       opt <- lookupByLogin r login'
       case opt of 
         Nothing -> do opt' <- save r newuser
                       return (either (\_ -> Nothing) Just opt')
         Just au -> return (Just au)
