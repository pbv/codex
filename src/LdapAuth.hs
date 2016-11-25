{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module LdapAuth 
       (ldapAuth
       -- , 
        -- dummyAuth,
        -- userName,
        -- adminRole
       ) where

import           Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as B

import           Data.Text(Text)
import qualified Data.Text as T

import           Data.HashMap.Strict(HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Set (Set)
import qualified Data.Set as Set

import           Data.Char (isSpace,toLower)

-- import           Control.Monad.Trans.Maybe
-- import           Control.Monad.Trans
import           Control.Monad

import           Data.Aeson.Types 

import           LDAP

import           Data.Time.Clock

import           Snap.Snaplet.Auth

import           Types


-- user meta attributes
type Attrs = HashMap Text Value

-- attempt LDAP bind to check user password and search for LDAP attributes
ldapBindSearch :: LdapConf -> String -> String -> IO (Maybe Attrs)
ldapBindSearch LdapConf{..} user passwd = catchLDAP search (\_ -> return Nothing) 
    where
        dn = "uid=" ++ user ++ "," ++ ldapBase
        search = do 
            con <- ldapInitialize ldapURI
            ldapSimpleBind con dn passwd
            lst <- ldapSearch con (Just dn) LdapScopeSubtree Nothing LDAPAllUserAttrs False
            return $ case lst of
                       [] -> Nothing
                       (le:_) -> Just $ HM.fromList [ (T.pack k, String $ T.pack v) 
                                                      | (k, v:_) <- leattrs le,
                                                        k`Set.member`keepAttrs
                                                    ]


-- set of LDAP attibutes to keep
keepAttrs :: Set String
keepAttrs = Set.fromList ["gecos", "cn", "sn", "givenName"]


ldapAuth :: IAuthBackend r => 
            r -> LdapConf -> ByteString -> ByteString -> IO (Maybe AuthUser)
ldapAuth r ldapConf user passwd 
    = do optMeta <- ldapBindSearch ldapConf userStr (B.toString passwd)
         case optMeta of
           Just meta -> updateUser r ldapConf login meta
           Nothing -> return Nothing

  where userStr = sanitize (B.toString user)
        login = T.pack userStr
  
      
-- keep only non space chars and lowercase all letters
sanitize :: String -> String 
sanitize  = map toLower . filter (not . isSpace)


-- | update or create a user with given attributes
updateUser :: IAuthBackend r =>
              r -> LdapConf -> Text -> Attrs -> IO (Maybe AuthUser)
updateUser r LdapConf{..} login attrs = do
  now <- getCurrentTime
  optAu <- lookupByLogin r login
  let newuser = defAuthUser { 
                     userId = optAu >>= userId
                   , userLogin = login
                   , userPassword = Nothing
                   , userCreatedAt = (optAu >>= userCreatedAt) `mplus` Just now
                   , userUpdatedAt = Just now
                   , userCurrentLoginAt = Just now                   
                   , userMeta = attrs
                   -- , userRoles = if login `elem` ldapAdmins then [adminRole] else []
                   }
  result <- save r newuser
  return $ case result of 
    Left err -> Nothing
    Right au -> Just au


-- | Administrator role
adminRole :: Role
adminRole = Role "admin"



-----------------------------------------------------------
-- dummy password-less login
-- for development only!
{-
dummyAuth :: IAuthBackend r => 
             r -> LdapConf -> ByteString -> ByteString -> IO (Maybe AuthUser)
dummyAuth r ldapConf username passwd
    = updateUser r ldapConf (T.pack $ sanitize $ B.toString username) HM.empty
-}

