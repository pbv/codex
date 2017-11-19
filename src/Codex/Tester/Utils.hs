{-# LANGUAGE OverloadedStrings #-}

module Codex.Tester.Utils where


import           Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Control.Exception
import           Control.Monad       (when)
import           Control.Monad.Trans (liftIO)
import           System.IO
import           System.IO.Temp
import           System.Exit
import           System.Directory
import           System.Posix.Files
import           System.Posix.Types (FileMode)
import           System.Process.Text

import           Data.Bits

import           Codex.Tester.Result 

-- | match a piece of text
match :: Text -> Text -> Bool
match = T.isInfixOf

-- | aquire and release temporary files
withTextTemp :: FilePath -> Text -> (FilePath -> IO a) -> IO a
withTextTemp name contents k
  = withSystemTempFile name (\f h -> T.hPutStr h contents >> hClose h >> k f)

{-
withTempFile :: FilePath -> ((FilePath, Handle) -> IO a) -> IO a
withTempFile name = bracket create (\(f,_) -> removeFile f)
  where create = do
          tmpDir <- getTemporaryDirectory
          openTempFileWithDefaultPermissions tmpDir name
-}

-- | remove files if they exist, silently ignore otherwise
removeFileIfExists :: FilePath -> IO ()
removeFileIfExists f = do b<-doesFileExist f; when b (removeFile f)

cleanupFiles :: [FilePath] -> IO ()
cleanupFiles = mapM_ removeFileIfExists

-- | ensure a file is readable by all users
ensureFileReadable :: FilePath -> IO ()
ensureFileReadable 
  = ensureFileMode (ownerReadMode .|.  groupReadMode .|. otherReadMode )

ensureFileExecutable :: FilePath -> IO ()
ensureFileExecutable 
  = ensureFileMode (ownerExecuteMode .|. groupExecuteMode .|. otherExecuteMode)

ensureFileMode :: FileMode -> FilePath  -> IO ()
ensureFileMode flags path = do
  mode <- fileMode <$> getFileStatus path
  when (mode .&. flags /= flags) $
    setFileMode path (mode .|. flags)

runCompiler :: FilePath -> [String] -> IO ()
runCompiler cmd args = do
  (exitCode, _, err) <- readProcessWithExitCode cmd args ""
  case exitCode of
    ExitFailure _ ->
      throw (compileError err)
    ExitSuccess ->
      return ()

