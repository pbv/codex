{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Codex.LdapAuth (
  LdapConf,
  getLdapConf,
  ldapAuth
  ) where

import           Control.Monad
import           Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as B

import           Data.Text(Text)
import qualified Data.Text as T


import           Data.HashMap.Strict(HashMap)
import qualified Data.HashMap.Strict as HM

import           Data.Maybe (maybeToList, fromMaybe)

import           Data.Char (isAlphaNum,toLower)
import           Data.Aeson.Types

import           Data.Time.Clock
import           Snap.Snaplet.Auth

import qualified Ldap.Client as Ldap

import qualified Data.Configurator as Conf
import           Data.Configurator.Types (Config)
import           Network.URI

-- | LDAP configuration
data LdapConf = LdapConf { -- ldapURI :: String             -- ^ LDAP server URI
                           ldapHost :: Ldap.Host
                         , ldapPort :: Ldap.PortNumber
                         , ldapBase :: Text              -- ^ DN search base
                         , ldapMap :: HashMap Text Text  -- ^ attributes to keep
                         }
                deriving Show


-- user meta attributes
type Attrs = HashMap Text Value

ldapAuth ::
  IAuthBackend r =>
  r -> LdapConf -> ByteString -> ByteString -> IO (Either AuthFailure AuthUser)
ldapAuth r ldapConf user passwd 
  | validLogin userS
    = do entries <- ldapBindSearch ldapConf userS passwdS
         case entries of
           Right (entry:_) -> let attrs = convertEntry ldapConf entry
                              in updateUserAttrs r (T.pack userS) attrs
           _ -> return (Left (AuthError "LDAP authentication failed"))
  | otherwise = return (Left (AuthError "Invalid user login"))

  where userS   = map toLower (B.toString user)
        passwdS = B.toString passwd


validLogin :: String -> Bool
validLogin = all (\x -> isAlphaNum x || x=='_' || x=='-' || x=='.' || x=='@')



-- | attempt LDAP bind, check user password and search LDAP entries
ldapBindSearch :: LdapConf -> String -> String
               -> IO (Either Ldap.LdapError [Ldap.SearchEntry])
ldapBindSearch LdapConf{..} uid passwd
  = Ldap.with ldapHost ldapPort $ \ldap -> do
          Ldap.bind ldap ldapDn ldapPasswd
          Ldap.search ldap ldapDn (Ldap.scope Ldap.BaseObject) ldapFilter []
  where
    ldapDn = Ldap.Dn ("uid=" <> T.pack uid <> "," <> ldapBase)
    ldapFilter = Ldap.Present (Ldap.Attr "gecos")
    ldapPasswd = Ldap.Password (B.fromString passwd)


-- | convert LDAP entries into user attributes
convertEntry :: LdapConf ->  Ldap.SearchEntry -> Attrs
convertEntry LdapConf{..} (Ldap.SearchEntry _ attrlist) =
  HM.fromList [ (key', String entry)
                   | (Ldap.Attr key, vals) <- attrlist,
                     key' <- maybeToList (HM.lookup key ldapMap),
                     let entry = T.concat (map (T.pack . B.toString) vals)
                   ]


-- | update or create a user with given attributes
updateUserAttrs :: IAuthBackend r =>
                  r -> Text -> Attrs -> IO (Either AuthFailure AuthUser)
updateUserAttrs r login attrs = do
  now <- getCurrentTime
  mbAu <- lookupByLogin r login
  let attrs' = maybe HM.empty userMeta mbAu
  let user = defAuthUser { userId = mbAu >>= userId
                         , userLogin = login
                         , userCreatedAt = (mbAu >>= userCreatedAt)
                                           `mplus` Just now
                         , userUpdatedAt = Just now
                         , userCurrentLoginAt = Just now
                         , userMeta = HM.union attrs' attrs
                         }
  save r user



getLdapConf ::  Config -> IO (Maybe LdapConf)
getLdapConf conf = do
  enabled <- Conf.lookupDefault False conf "enabled"
  if enabled then
    do uri <- Conf.require conf "uri"
       base <- Conf.require conf "base"
       attrs <- HM.fromList <$> Conf.require conf "attrs"
       return (makeLdapConf base attrs =<< parseAbsoluteURI uri)
    else return Nothing


makeLdapConf :: Text -> HashMap Text Text -> URI -> Maybe LdapConf
makeLdapConf base attrs URI{..} =
  do host <- fmap uriRegName uriAuthority
     let optPort = readMaybe =<< fmap uriPort uriAuthority
     case uriScheme of
      "ldap:" ->
        return LdapConf { ldapHost = Ldap.Plain host
                        , ldapPort = fromMaybe defaultLdapPort optPort
                        , ldapBase = base
                        , ldapMap = attrs
                        }
      "ldaps:" ->
        -- allow insecureTlsSettings to disregard LDAP server name
        return LdapConf { ldapHost = Ldap.Tls host Ldap.insecureTlsSettings
                        , ldapPort = fromMaybe defaultLdapsPort optPort
                        , ldapBase = base
                        , ldapMap = attrs
                        }
      _ -> Nothing



defaultLdapPort, defaultLdapsPort :: Ldap.PortNumber
defaultLdapPort  = 389
defaultLdapsPort = 636


readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of ((a, "") : _) -> return a
                              _ -> Nothing
