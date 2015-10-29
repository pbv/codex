--
-- | A monad combining catching IO errors and logging
--
module LogIO ( LogIO
             , runLogIO
             , logString
             , logPrefix
             , safeIO
             ) where

import           Data.Text(Text)
import qualified Data.Text as T
import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.Writer
import           System.IO.Error

-- the reader environment provides a prefix for the logging messages
type LogIO a = ReaderT Text (WriterT [Text] IO) a 

runLogIO :: LogIO a -> IO (a, [Text])
runLogIO m = runWriterT (runReaderT m T.empty)

-- | log a string
logString :: String -> LogIO ()
logString s = do { p <- ask; tell [T.append p (T.pack s)] }

-- | set the logging messages prefix 
logPrefix :: String -> LogIO a -> LogIO a
logPrefix s = local (const (T.pack $ s ++ ": ")) 

-- | lift an IO action to the logging monad
-- | logs a message on IO error and returns a default value
safeIO :: a -> IO a -> LogIO a
safeIO def m = do
  r <- liftIO (catchIOError (Right <$> m) (return . Left))
  case r of
    Left err -> tell [T.pack (show err)] >> return def
    Right v -> return v



