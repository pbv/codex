{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module LdapAuth 
       (ldapAuth
       ) where

import           Control.Monad
import           Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as B

import           Data.Text(Text)
import qualified Data.Text as T

import           Data.HashMap.Strict(HashMap)
import qualified Data.HashMap.Strict as HM

import           Data.Char (isSpace,toLower)
import           Data.Aeson.Types 

import           Data.Time.Clock
import           Snap.Snaplet.Auth
import           LDAP
import           Types


-- user meta attributes
type Attrs = HashMap Text Value

ldapAuth ::
  IAuthBackend r =>
  r -> LdapConf -> ByteString -> ByteString -> IO (Either AuthFailure AuthUser)
ldapAuth r ldapConf user passwd 
    = do entries <- ldapBindSearch ldapConf userStr (B.toString passwd)
         case entries of
           (entry:_) -> let attrs = convertEntry ldapConf entry
                        in updateUserAttrs r login attrs 
           [] -> return (Left (AuthError "LDAP authentication failed"))

  where userStr = sanitize (B.toString user)
        login = T.pack userStr
        
-- keep only non space chars and lowercase all letters
sanitize :: String -> String 
sanitize  = map toLower . filter (not . isSpace)



-- attempt LDAP bind, check user password and search LDAP entries
ldapBindSearch :: LdapConf -> String -> String -> IO [LDAPEntry]
ldapBindSearch LdapConf{..} login passwd
  = catchLDAP (do con <- ldapInitialize ldapURI
                  ldapSimpleBind con dn passwd
                  search con) (\_ -> return []) 
  where
      dn = "uid=" ++ login ++ "," ++ ldapBase
      search con =
        ldapSearch con (Just dn) LdapScopeSubtree Nothing LDAPAllUserAttrs False



-- convert LDAP entries into user attributes
convertEntry :: LdapConf ->  LDAPEntry -> Attrs
convertEntry LdapConf{..} LDAPEntry{..} =
  HM.fromList [ (mkey, String entry)
                   | (key,vals) <- leattrs,
                     let key' = T.pack key,
                     let entry = T.concat (map T.pack vals),
                     let mkey = HM.lookupDefault key' key' ldapMap
                   ]
  

-- update or create a user with given attributes
updateUserAttrs :: IAuthBackend r =>
                  r -> Text -> Attrs -> IO (Either AuthFailure AuthUser)
updateUserAttrs r login attrs = do
  now <- getCurrentTime
  mbAu <- lookupByLogin r login
  let attrs' = maybe HM.empty userMeta mbAu
  let user = defAuthUser { userId = mbAu >>= userId
                         , userLogin = login
                         , userPassword = mbAu >>= userPassword 
                         , userCreatedAt = (mbAu >>= userCreatedAt)
                                           `mplus` Just now
                         , userUpdatedAt = Just now
                         , userCurrentLoginAt = Just now        
                         , userMeta = HM.union attrs' attrs
                         }
  save r user



