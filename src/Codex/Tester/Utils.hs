{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Codex.Tester.Utils
  ( ProcessRun(..)
  , compileErrorHandler
  , match
  , withTemp, withTempFile, withTempDir
  , assert
  , fileExists
  , removeFileIfExists
  , cleanupFiles
  , runProcess
  , sandboxExec
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
import           System.IO.Temp (withSystemTempFile, withSystemTempDirectory)
import           System.Exit
import           System.Directory (doesFileExist, copyFile, removeFile)
-- import           System.Posix.Files
-- import           System.Posix.Types (FileMode)
import           System.FilePath.Glob(glob)
import           System.FilePath ((</>))

import           Control.Concurrent (forkIO)
import           Control.Concurrent.Async (concurrently)
import           System.Process ( -- readProcessWithExitCode,
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
-- import           Data.Bits
import           Data.Maybe(catMaybes)


import           Text.Pandoc(Meta)
import qualified Text.Pandoc.Builder as P

import           Codex.Page(lookupFromMeta)
import           Codex.Tester.Result

import qualified ShellWords

-- | the output of running a process
data ProcessRun
  = ProcessRun { procExitCode :: !ExitCode
               , procStdout :: !Text
               , procStderr :: !Text
               }
  deriving (Read, Show)

-- | allow throwing as an exception; default instance
instance Exception ProcessRun


-- | match a piece of text
match :: Text -> Text -> Bool
match = T.isInfixOf

-- | aquire and release temporary files and directories
withTemp :: MonadIO m => FilePath -> Text -> (FilePath -> IO a) -> m a
withTemp name contents k
  = liftIO $
    withSystemTempFile name
        (\f h -> T.hPutStr h contents >> hClose h >> k f)


-- | aquire a temporary file initialized with the contents of another file
withTempFile :: MonadIO m
  => FilePath -> FilePath -> (FilePath -> IO a) -> m a 
withTempFile name orig k
  = liftIO $
    withSystemTempFile name
       (\f h -> hClose h >> copyFile orig f >> k f)

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

-- | run an external process (e.g.compiler) under a sandbox
-- and throw an exception if it fails
runProcess :: FilePath -> FilePath -> [String] -> IO ()
runProcess profile exec args = do
  out <- sandboxExec profile exec args ""
  case procExitCode out of
    ExitFailure _ ->
      throwIO out
    ExitSuccess ->
      return ()

compileErrorHandler :: ProcessRun -> IO Result
compileErrorHandler (ProcessRun (ExitFailure 137) stdout stderr)
  = return $ timeLimitExceeded (P.codeBlock stderr)
compileErrorHandler (ProcessRun _ stdout stderr)
  = return $ compileError (P.codeBlock (stdout<>stderr))
-- | sandbox execution of a subprocess
sandboxExec :: FilePath           -- ^ path to profile
            -> FilePath           -- ^ path to executable
            -> [String]           -- ^ comand line arguments
            -> Text               -- ^ stdin
            -> IO ProcessRun
sandboxExec profile exec args stdin = do
  (code, out, err) <- sandboxExecBS profile exec args (T.encodeUtf8 stdin)
  return ProcessRun { procExitCode = code
                    , procStdout = T.decodeUtf8With T.lenientDecode out
                    , procStderr =
                        if code == ExitFailure 137 then
                          "TimeLimitExceeded\n"
                        else
                          ""
                          <>
                          T.decodeUtf8With T.lenientDecode err
                    }

sandboxExecBS :: FilePath           -- ^ profile
              -> FilePath           -- ^ command
              -> [String]           -- ^ arguments
              -> ByteString         -- ^ stdin
              -> IO (ExitCode, ByteString, ByteString)
              -- ^ code, stdout, stderr
sandboxExecBS profile exec args inbs = do
  let args' = ["--profile="++profile, exec] ++ args
  let outputLimit = 50000 
  (inp, outstream, errstream, pid) <-
    Streams.runInteractiveProcess "firejail" args'  Nothing Nothing
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

{-
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
    Streams.runInteractiveProcess "firejail" ([ "--profile=/home/pbv/codex/codex.profile",
                                                exec ] ++ args) dir Nothing
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
            mkArg "--minuid" minUID
            ++
            mkArg "--maxuid" maxUID
            ++
            ["--exec", exec] 
-}


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
  
{-
unsafeExec :: FilePath                 -- ^ command
          -> [String]                  -- ^ arguments
          -> Text                      -- ^ stdin
          -> IO (ExitCode, Text, Text) -- ^ code, stdout, stderr
unsafeExec exec args inp = do
  (code, out, err) <-
    readProcessWithExitCode exec args (T.unpack inp)
  return (code, T.pack out, T.pack err)
-}

        
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


       
    
