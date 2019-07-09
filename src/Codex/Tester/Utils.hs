{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Codex.Tester.Utils
  ( match
  , withTemp, withTempDir
  , assert
  , fileExists
  , removeFileIfExists
  , cleanupFiles
  , chmod
  , readable, executable, writeable
  , runCompiler
  , safeExec
  , unsafeExec
  , getQuickCheckArgs
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


import           Control.Concurrent (forkIO)
import           Control.Concurrent.Async (async, wait)
import           System.Process (readProcessWithExitCode,
                                  terminateProcess, waitForProcess)
import           System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams

import           Data.ByteString.Char8    (ByteString)
import qualified Data.ByteString.Char8          as B
import qualified Data.ByteString.Builder        as B
import qualified Data.ByteString.Lazy           as LB

import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import qualified Data.Text.Encoding.Error       as T

  
import           Data.Bits
import           Data.Maybe(catMaybes)

import           Text.Pandoc(Meta)
import           Codex.Page(lookupFromMeta)
import           Codex.Tester.Result
import           Codex.Tester.Limits

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




runCompiler :: FilePath -> [String] -> IO ()
runCompiler cmd args = do
  (exitCode, out, err) <- readProcessWithExitCode cmd args ""
  case exitCode of
    ExitFailure _ ->
      throwIO (compileError $ T.pack out <> T.pack err)
    ExitSuccess ->
      return ()


-- | safeExec with text input/output
safeExec :: Limits
          -> FilePath           -- ^ command
          -> Maybe FilePath     -- ^ optional working directory
          -> [String]           -- ^ arguments
          -> Text               -- ^ stdin
          -> IO (ExitCode, Text, Text)
             -- ^ code, stdout, stderr
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
           -> IO (ExitCode, ByteString,ByteString)
           --    ^ code, stdout, stderr
safeExecBS Limits{..} exec dir args inbs = do
  (inp, out, err, pid) <-
    Streams.runInteractiveProcess "safeexec" (args' ++ args) dir Nothing
  (do forkIO (produceStream inp inbs)
      a1 <- async (consumeStream outputLimit out)
      a2 <- async (consumeStream outputLimit err)
      outbs <- wait a1
      errbs <- wait a2
      code <- waitForProcess pid
      return (code, outbs, errbs)
    ) `catch` (\(e ::SomeException) ->
                  do terminateProcess pid
                     code <- waitForProcess pid
                     return (code, "", B.pack $ show e)
              )
  where
    outputLimit = maybe 30000 fromIntegral maxFSize
    -- default output limit: 30K bytes
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



produceStream :: OutputStream a -> a -> IO ()
produceStream s v = do
  Streams.write (Just v) s
  Streams.write Nothing s


-- | consume a stream and accumulate output
-- throws an exception if maximum size exceeded
consumeStream :: Int -> InputStream ByteString -> IO ByteString
consumeStream maxSize inp = do
  buf <- worker maxSize inp mempty
  return (LB.toStrict . B.toLazyByteString $ buf)
  where
    worker :: Int -> InputStream ByteString -> B.Builder -> IO B.Builder
    worker size inp buf = do
      response <- Streams.read inp
      case response of
        Nothing -> return buf
        Just bytes ->
          let k = B.length bytes
          in if k > size then
               ioError $ userError "Output Limit Exceeded"
             else
               worker (size - k) inp (buf <> B.byteString bytes)

  
unsafeExec :: FilePath                 -- ^ command
          -> [String]                  -- ^ arguments
          -> Text                      -- ^ stdin
          -> IO (ExitCode, Text, Text) -- ^ code, stdout, stderr
unsafeExec exec args inp = do
  (code, out, err) <-
    readProcessWithExitCode exec args (T.unpack inp)
  return (code, T.pack out, T.pack err)


-- get QuickCheck runner command line arguments
getQuickCheckArgs :: Meta -> [String]
getQuickCheckArgs meta
  = catMaybes
    [ lookupArg "maxSuccess"
    , lookupArg "maxSize"
    , lookupArg "maxDiscardRatio"
    , lookupArg "randSeed"
    ]
  where
    lookupArg key
      = fmap (\val -> "--" ++ key ++ "=" ++ val) (lookupFromMeta key meta)


