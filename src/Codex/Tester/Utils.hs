{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Codex.Tester.Utils
  ( RunProcessErr(..)
  , compileErrorHandler
  , match
  , withTemp, withTempDir
  , assert
  , fileExists
  , removeFileIfExists
  , cleanupFiles
  , chmod
  , readable, executable, writeable
  , runProcess
  , safeExec
  , unsafeExec
  , getMetaArgs
  , parseArgs
  , globPatterns
  ) where


import           Data.Text(Text)
import qualified Data.Text.IO as T

import           Control.Exception hiding (assert)
import           Control.Monad       (when, unless)
import           Control.Monad.Trans
import           System.IO
import           System.IO.Temp
import           System.Exit
import           System.Directory (doesFileExist, removeFile)
import           System.Posix.Files
import           System.Posix.Types (FileMode)
import           System.FilePath.Glob(glob)
import           System.FilePath ((</>))

import           Control.Concurrent (forkIO)
import           Control.Concurrent.Async (concurrently)
import           System.Process (readProcessWithExitCode,
                                 waitForProcess)
import           System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams

import           Data.ByteString.Char8    (ByteString)
import qualified Data.ByteString.Char8          as B
import qualified Data.ByteString.Builder        as B
import qualified Data.ByteString.Lazy           as LB

import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import qualified Data.Text.Encoding.Error       as T

import           Data.Int (Int64)
import           Data.List (sort)
import           Data.Bits
import           Data.Maybe(catMaybes)


import           Text.Pandoc(Meta)
import qualified Text.Pandoc.Builder as P

import           Codex.Page(lookupFromMeta)
import           Codex.Tester.Result
import           Codex.Tester.Limits

import qualified ShellWords


data RunProcessErr = RunProcessErr !Text !Text -- stdout, stderr
  deriving (Read, Show)

instance Exception RunProcessErr  -- default instance


-- | match a piece of text
match :: Text -> Text -> Bool
match = T.isInfixOf

-- | aquire and release temporary files and directories
withTemp :: MonadIO m => FilePath -> Text -> (FilePath -> IO a) -> m a
withTemp name contents k
  = liftIO $ withSystemTempFile name (\f h -> T.hPutStr h contents >> hClose h >> k f)


withTempDir :: MonadIO m => FilePath -> (FilePath -> IO a) -> m a
withTempDir name k
  = liftIO $ withSystemTempDirectory name k


-- | ensure an test assertion; throws MiscError otherwise
assert :: MonadIO m => IO Bool -> String -> m ()
assert chk msg = liftIO $ do
  c <- chk
  unless c $ throwIO (AssertionFailed msg)

fileExists :: FilePath -> IO Bool
fileExists = doesFileExist


-- | remove files if they exist, silently ignore otherwise
removeFileIfExists :: FilePath -> IO ()
removeFileIfExists f = do b<-doesFileExist f; when b (removeFile f)

cleanupFiles :: [FilePath] -> IO ()
cleanupFiles = mapM_ removeFileIfExists


chmod :: MonadIO m => (FileMode -> FileMode) -> FilePath -> m ()
chmod chg filepath = liftIO $ do
  mode <- fileMode <$> getFileStatus filepath
  setFileMode filepath (chg mode)


readable :: FileMode -> FileMode
readable mode =
  mode .|. ownerReadMode .|. groupReadMode .|. otherReadMode

executable :: FileMode -> FileMode
executable mode =
  mode .|. ownerExecuteMode .|. groupExecuteMode .|. otherExecuteMode

writeable :: FileMode -> FileMode
writeable mode =
  mode .|. ownerWriteMode .|. groupWriteMode .|. otherWriteMode


-- | run an external process (e.g.compiler) possibly under a sandbox
runProcess :: Maybe Limits -> FilePath -> [String] -> IO ()
runProcess optLimits cmd args = do
  (exitCode, stdout, stderr) <- case optLimits of
    Nothing -> readTextProcessWithExitCode cmd args ""
    Just limits -> safeExec limits cmd Nothing args ""
  case exitCode of
    ExitFailure _ -> 
      throwIO (RunProcessErr stdout stderr)
    ExitSuccess ->
      return ()

compileErrorHandler :: RunProcessErr -> IO Result
compileErrorHandler (RunProcessErr stdout stderr)
  = return $ compileError (P.codeBlock (stdout<>stderr))


readTextProcessWithExitCode cmd args stdin = do
  (exitCode, out, err) <- readProcessWithExitCode cmd args stdin
  return (exitCode, T.pack out, T.pack err)


-- | safeExec with text input/output
safeExec :: Limits
         -> FilePath           -- ^ command
         -> Maybe FilePath     -- ^ optional working directory
         -> [String]           -- ^ comand line arguments
         -> Text               -- ^ stdin
         -> IO (ExitCode, Text, Text)
         --               ^ stdout, stderr
safeExec limits exec dir args stdin = do
  (code, out, err) <- safeExecBS limits exec dir args (T.encodeUtf8 stdin)
  return (code,
           T.decodeUtf8With T.lenientDecode out,
           T.decodeUtf8With T.lenientDecode err)

--
-- | safeExec using streaming I/O to handle output limits
--
safeExecBS :: Limits
           -> FilePath           -- ^ command
           -> Maybe FilePath     -- ^ working directory
           -> [String]           -- ^ arguments
           -> ByteString         -- ^ stdin
           -> IO (ExitCode, ByteString, ByteString)
           -- ^ code, stdout, stderr
safeExecBS Limits{..} exec dir args inbs = do
  (inp, outstream, errstream, pid) <-
    Streams.runInteractiveProcess "safeexec" (args' ++ args) dir Nothing
  (do forkIO (produceStream inp inbs)
      (outbs, errbs) <- concurrently
                        (consumeStream outputLimit outstream)
                        (consumeStream outputLimit errstream) 
      code <- waitForProcess pid
      return (code, outbs, errbs)
    ) `catch` (\(e :: SomeException) ->
                  do code <- waitForProcess pid
                     return (code, "", B.pack $ show e)
              ) 
  where
    -- default output limit: 50K bytes
    outputLimit = maybe 50000 fromIntegral maxFSize
    mkArg opt = maybe [] (\c -> [opt, show c])
    args' = mkArg "--cpu" maxCpuTime
            ++
            mkArg "--clock" maxClockTime
            ++
            mkArg "--mem" maxMemory
            ++
            mkArg "--stack" maxStack
            ++
            mkArg "--fsize" maxFSize
            ++
            mkArg "--core" maxCore
            ++
            mkArg "--nproc" numProc
            ++
            ["--exec", exec] 



produceStream :: OutputStream ByteString -> ByteString -> IO ()
produceStream out bs
  = Streams.fromByteString bs >>= Streams.connectTo out


-- | concatenate bytes from an input stream upto a specified limit
-- NB: this function consumes the input stream until the end
--
consumeStream :: Int64 -> InputStream ByteString -> IO ByteString
consumeStream limit stream = do
  (stream, getCount) <- Streams.countInput stream
  LB.toStrict . B.toLazyByteString <$>  
    Streams.foldM (\builder bs -> do
                      c <- getCount
                      if c > limit
                        then return builder
                        else return (builder <> B.byteString bs)) mempty stream
  
unsafeExec :: FilePath                 -- ^ command
          -> [String]                  -- ^ arguments
          -> Text                      -- ^ stdin
          -> IO (ExitCode, Text, Text) -- ^ code, stdout, stderr
unsafeExec exec args inp = do
  (code, out, err) <-
    readProcessWithExitCode exec args (T.unpack inp)
  return (code, T.pack out, T.pack err)


        
--
-- get comand line arguments from metadata field values
-- first list are keys that take arguments;
-- second list are keys that control on/off switches
--
getMetaArgs :: [Text] -> [Text] -> Meta -> [Text]
getMetaArgs argKeys switchKeys meta
  = catMaybes ([ do str <- lookupFromMeta key meta
                    Just ("--" <> key <> "=" <> str)
               | key <- argKeys ]
                ++
               [ do bool <- lookupFromMeta key meta
                    if bool then Just ("--" <> key) else Nothing
                | key <- switchKeys ]
              )

                 

-- | parse a command line string
parseArgs :: MonadIO m => String -> m [String]
parseArgs ""
  = return [] -- special case to signal empty arguments
parseArgs txt = case ShellWords.parse txt of
  Left msg ->
    liftIO $ throwIO $ userError ("parse error in command-line arguments: " <> msg)
  Right args ->
    return args


globPattern :: MonadIO m => String -> m [FilePath]
globPattern patt = do                        
  files <- sort <$> liftIO (glob patt)
  when (null files) $
    liftIO $ throwIO $ userError ("no files match " ++ show patt)
  return files

globPatterns :: MonadIO m => FilePath -> [String] -> m [FilePath]
globPatterns dir patts = concat <$> mapM (globPattern . (dir</>)) patts


       
    
