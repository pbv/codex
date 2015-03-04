{-
  Types of identifiers for various entities
-}

module Types where

import           Data.ByteString.UTF8(ByteString)
import qualified Data.ByteString.UTF8 as B

import           Data.Text (Text)
import qualified Data.Text as T

import           Data.Configurator.Types

import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.FromField


-- | user identifiers (login)
newtype UID = UID ByteString deriving (Eq,Ord)

-- | problem identifiers 
newtype PID = PID ByteString deriving (Eq,Ord)

-- | submission identifiers
newtype SID = SID Int deriving (Eq,Ord)

-- | read and show instances
instance Show UID where
  showsPrec _ (UID s) = ((B.toString s)++)
  
instance Read UID where
  readsPrec _ xs = [(UID $ B.fromString xs, "")]

instance Show PID where
  showsPrec _ (PID s) = ((B.toString s)++)
  
instance Read PID where
  readsPrec _ xs = [(PID $ B.fromString xs, "")]

instance Show SID where
  showsPrec _ (SID n) = shows n

instance Read SID where
  readsPrec _ xs = [(SID n,xs')| (n,xs')<-reads xs]
  
  
-- | convertion to/from SQLite fields
instance ToField UID where  
  toField (UID uid) = toField (B.toString uid)
  
instance ToField PID where
  toField (PID pid) = toField (B.toString pid)
  
instance ToField SID where
  toField (SID sid) = toField sid
  
instance FromField UID where
  fromField f = fmap (UID . B.fromString) (fromField f)
  
instance FromField PID where
  fromField f = fmap (PID . B.fromString) (fromField f)

instance FromField SID where
  fromField f = fmap SID (fromField f)
  
  
-- | safeexec parameters
data SafeExec = SafeExec { safeExec :: FilePath
                         , pythonExec :: FilePath
                         , maxCpuTime :: Int
                         , maxClockTime :: Int
                         , maxMemory :: Int 
                         } 
                deriving (Eq, Show)

  
-- | LDAP configuration
data LdapConf = LdapConf { ldapURI :: String
                         , ldapBases :: [String]
                         } 
                deriving (Eq,Show)
                         
-- | printout configuration
data Printout = Printout { printEnabled :: Bool 
                         , printHeader :: Text
                         , printOptions :: [String]
                         } 
                deriving (Eq,Show)



-- | problem tags
type ProblemTag = Text
