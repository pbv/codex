{-# LANGUAGE RecordWildCards, OverloadedStrings, DeriveDataTypeable #-}
module Tester where

import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.State
import           Control.Exception
import           Data.Typeable

import           Data.Monoid
import           Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.Exit
import           System.FilePath
import           System.IO 
import           System.Directory

import qualified Data.ByteString.Char8 as B

import           Types

-- | tester in some monad m
type Tester m = Code -> m (Result,Text)

data Result = Accepted
            | WrongAnswer
            | CompileError
            | RuntimeError
            | TimeLimitExceeded
            | MemoryLimitExceeded
            | MiscError
              deriving (Eq, Read, Show, Typeable)


match :: Text -> Text -> Bool
match = T.isInfixOf

-- trim a text to a maximum length
trim :: Int -> Text -> Text
trim size txt
  | T.length txt <= size = txt
  | otherwise = T.append (T.take size txt) "\n**Output too long (truncated)***\n"


-- aquire and release a text temporary file
withTextTemp :: String -> Text -> (FilePath -> IO a) -> IO a
withTextTemp name txt cont = withTempFile name cont'
   where cont' (f,h) = T.hPutStr h txt >> hClose h >> cont f


withTempFile :: String -> ((FilePath,Handle) -> IO a) -> IO a
withTempFile name k = bracket createTemp (\(f,_)->removeFile f) k
  where createTemp = do
          tempDir <- getTemporaryDirectory
          openTempFileWithDefaultPermissions tempDir name


  
