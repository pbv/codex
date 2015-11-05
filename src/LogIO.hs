--
-- | A monad combining catching and logging IO errors
--
module LogIO ( LogIO
             , runLogIO
             , logString
             , logText
             -- , logPrefix
             , safeIO
             ) where

import           Data.Text(Text)
import qualified Data.Text as T
import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.Writer
import           System.IO.Error

type LogIO a = WriterT [Text] IO a 

runLogIO :: LogIO a -> IO (a, [Text])
runLogIO = runWriterT  

-- | log a string
logString :: String -> LogIO ()
logString = logText . T.pack

logText :: Text -> LogIO ()
logText t = tell [t]

-- | set the logging messages prefix 
-- logPrefix :: String -> LogIO a -> LogIO a
-- logPrefix s = local (const (T.pack $ s ++ ": ")) 

-- | lift an IO action to the logging monad
-- | logs a message on IO error and returns a default value
safeIO :: a -> IO a -> LogIO a
safeIO def m = do
  r <- liftIO (catchIOError (Right <$> m) (return . Left))
  case r of
    Left err -> tell [T.pack (show err)] >> return def
    Right v -> return v



