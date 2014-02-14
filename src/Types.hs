{-# LANGUAGE FlexibleInstances #-}
{-
  Types of identifiers for various entities
-}

module Types where

-- import           System.FilePath
import           Data.ByteString.UTF8(ByteString)
import qualified Data.ByteString.UTF8 as B

import           Data.Text (Text)
import qualified Data.Text as T

-- import           Data.Configurator
import           Data.Configurator.Types


-- users; obtained from an auth session
newtype UID = UID { fromUID :: ByteString } deriving (Eq,Ord)

-- problems; parsed from URLs
newtype PID = PID { fromPID :: ByteString } deriving (Eq,Ord)

-- submission; obtained from URLs 
newtype SID = SID { fromSID :: Int } deriving (Eq,Ord)

--
-- read and show instances
--
instance Show UID where
  showsPrec _ (UID s) = ((B.toString s)++)
  
instance Read UID where
  readsPrec _ xs = [(UID $ B.fromString xs, "")]

instance Show PID where
  showsPrec _ (PID s) = ((B.toString s)++)
  
instance Read PID where
  readsPrec _ xs = [(PID $ B.fromString xs, "")]

instance Show SID where
  showsPrec _ (SID n) = ('S':).shows n

instance Read SID where
  readsPrec _ ('S':xs) = [(SID n,xs')| (n,xs')<-reads xs]
  readsPrec _ _        = []
  
  
-- | safeexec parameters
data SafeExec = SafeExec { safeExec :: FilePath
                         , pythonExec :: FilePath
                         , maxCpuTime :: Int
                         , maxClockTime :: Int
                         , maxMemory :: Int 
                         } 
                deriving (Eq, Show)

  
-- | LDAP configuration
data LdapConf = LdapConf { ldapHost :: String 
                         , ldapBases :: [String]
                         } 
                deriving (Eq,Show)
                         
-- | printout configuration
data Printout = Printout { printEnabled :: Bool 
                         , printHeader :: Text
                         , printOptions :: [String]
                         } 
                deriving (Eq,Show)


-- | allow lists of strings in configurator data
-- silently ignores non-string values
instance Configured [String] where
  convert (List vs) = Just [T.unpack s | String s <- vs]
  convert _         = Nothing

