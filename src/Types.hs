{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveFunctor, EmptyDataDecls #-}
{-
  Types of various entities
-}

module Types where

import           Control.Applicative
import           Data.ByteString.UTF8(ByteString)
import qualified Data.ByteString.UTF8 as B

import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as T

import           Data.Typeable
-- import           Data.Configurator.Types

import           Data.Time.Clock


import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.FromRow

import           Text.Pandoc.Builder hiding (Code)

-- identifiers
newtype ID a = ID ByteString deriving (Eq, Ord, Show, Read)

instance IsString (ID a) where
  fromString s = ID (B.fromString s)
  
fromID :: ID a -> ByteString
fromID (ID bs) = bs 

data User  -- phantom

-- | Code fragments tagged with language  
newtype Code lang = Code { fromCode :: Text } deriving (Eq,Show)

instance IsString (Code lang) where
  fromString s = Code (T.pack s)
  
toCode :: Text -> Code lang
toCode = Code

-- | convertion to/from SQL fields
instance ToField (Code lang) where
  toField (Code txt) = toField txt

instance FromField (Code lang) where
  fromField f = toCode <$> fromField f


-- phantom types for tagging Code 
data Python
data Doctest

  
-- individual problems
data Problem = Problem {
  probID       :: ID Problem,              -- unique identifier
  probHeader   :: Block,            -- header and description 
  probDescr    :: Blocks,
  probCode  :: Maybe (Code Python),    -- default submission
  probSpec  :: Maybe (Code Doctest),   -- doctest script
  probAttrs    :: [(Text, Text)],   -- attributes (key-value pairs)
  probLimit    :: Maybe UTCTime     -- optional deadline
  } deriving Show


-- worksheets
data Worksheet a = Worksheet { worksheetMeta :: Meta
                             , worksheetItems :: [Either Blocks a]
                             }
                 deriving (Show, Functor)

-- | single row in the submission DB
data Submission = Submission {
  submitID   :: ID Submission,
  submitUID  :: ID User,
  submitPID  :: ID Problem,           -- problems id
  submitIPAddr :: Text,        -- client IP address
  submitTime :: UTCTime,       -- submit time  
  submitCode :: Code Python,   -- program code
  submitStatus :: Status,       -- accepted/wrong answer/etc
  submitReport :: Text
  }
  
data Status = Accepted
            | WrongAnswer
            | CompileError
            | RuntimeError
            | TimeLimitExceeded
            | MemoryLimitExceeded
            | MiscError
              deriving (Eq, Read, Show, Typeable)

-- | convertion to/from SQL fields
instance ToField (ID a) where
  toField (ID id) = toField id
  
instance FromField (ID a) where
  fromField f = fmap ID (fromField f)

instance FromRow (ID a) where
    fromRow = field

instance ToField Status where
  toField s = toField (show s)

instance FromField Status where
  fromField f = do s <- fromField f 
                   parse (reads s)
    where 
      parse ((s,""):_) = return s
      parse _  = returnError ConversionFailed f "couldn't parse status field" 

instance FromRow Submission where
  fromRow = Submission <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field




{-
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
-}

-- | Python interpreter configuration
data PythonConf = PythonConf { pythonExec :: !FilePath
                             , pythonScript :: !FilePath
                             } deriving (Eq, Show)

  
-- | LDAP configuration
data LdapConf = LdapConf { ldapURI :: String
                         , ldapBase :: String
                         , ldapAdmins :: [Text]
                         } 
                deriving (Eq,Show)
                         
-- | Printout configuration
data PrintConf = PrintConf { printEnabled :: Bool 
                           , printHeader :: Text
                           , printOptions :: [String]
                           } 
                deriving (Eq,Show)


{-
-- | problem tags
type Tag = Text


-- | a type class for collecting tags from problems, etc.
class Tagged a where
    taglist  :: a -> [Tag]

-- check if something is tagged with a single tag
isTagged :: Tagged a => Tag -> a -> Bool
isTagged tag a = tag `elem` taglist a

-- check if something is tagged with a list of tags
hasTags :: Tagged a => [Tag] -> a -> Bool
hasTags tags a = tags `isSublistOf` taglist a
    where isSublistOf xs ys = all (`elem`ys) xs
-}
