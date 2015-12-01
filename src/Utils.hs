{-# LANGUAGE OverloadedStrings #-}
{-
  Some helper handlers for our application monad
-}
module Utils where

import           Control.Monad.State
import           Control.Exception
import           Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as B
import           Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.IO as T
import           Data.String

import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Auth

import           Heist
import qualified Heist.Interpreted as I

import qualified Text.XmlHtml as X


import qualified Data.Configurator as Configurator
import           Data.Configurator.Types

import           Control.Applicative 
import           Control.Exception (SomeException)

import           System.Remote.Monitoring
import           System.Remote.Counter as Counter

import           System.Directory
import           System.IO

import           Types
import           SafeExec
import           Language
import           Application
import           LdapAuth


import           Data.Time.Clock


-- | read configuration parameters
configPython :: Config -> IO PythonConf
configPython conf = do
  exec <- Configurator.lookup conf "python.exec"
  script <- Configurator.lookup conf "python.script"
  return PythonConf {
     pythonExec = maybe "python" id exec,
     pythonScript = maybe "python/pytest.py" id script
    }

configHaskell :: Config -> IO HaskellConf
configHaskell conf = do
  exec <- Configurator.lookup conf "haskell.exec"
  return HaskellConf { haskellExec = maybe "runhaskell" id exec }


configSafeExec :: Config -> IO SafeExecConf
configSafeExec conf = do
  -- python   <- Configurator.require conf "submissions.python"
  path <-Configurator.lookup conf "safeexec.path"
  cpu   <- Configurator.lookup conf "safeexec.max_cpu"
  clock <- Configurator.lookup conf "safeexec.max_clock"
  mem   <- Configurator.lookup conf "safeexec.max_memory"
  nproc <- Configurator.lookup conf "safexec.num_proc"
  return defaultConf {
    safeExecPath = maybe (safeExecPath defaultConf) id path
    , maxCpuTime = maybe (maxCpuTime defaultConf) id cpu 
    , maxClockTime = maybe (maxClockTime defaultConf) id clock
    , maxMemory = maybe (maxMemory defaultConf) id mem
    , numProc = maybe (numProc defaultConf) id nproc
    }

configPrintConf :: Config -> IO PrintConf
configPrintConf conf = do
  enabled <- Configurator.lookupDefault False conf "printouts.enabled"
  header <- Configurator.lookupDefault defaultHeader conf "printouts.header"
  opts <- Configurator.lookupDefault [] conf "printouts.options"
  return (PrintConf enabled header opts)
  where defaultHeader = "Pythondo"

configLdapConf :: Config -> IO LdapConf
configLdapConf conf = do
  uri <- Configurator.require conf "ldap.uri"
  base <- Configurator.require conf "ldap.base"
  admins <- Configurator.require conf "ldap.admins"
  return (LdapConf uri base admins)


-- | increment an EKG counter
incrCounter :: Text -> AppHandler ()
incrCounter name 
  = gets ekg >>= 
    maybe (return ())  (\ekg -> liftIO $ getCounter name ekg >>= Counter.inc) 



-- | Get current logged in user; fail with unauthorized error if missing
-- getRequiredUserID :: Pythondo UID
-- getRequiredUserID = require getUserID <|> unauthorized

-- | Get current logged in user ID (if any)
--   from the authentication snaplet  
getUserID :: AppHandler (Maybe UserID)
getUserID = do 
  opt <- with auth currentUser
  return $ fmap (fromString . T.unpack . userLogin) opt


getFullName :: AppHandler (Maybe Text)
getFullName = do
  opt <- with auth currentUser
  return (fmap userName opt)

-- | Get user id and roles
getUserRoles :: AppHandler (Maybe [Role])
getUserRoles = do
  opt <- with auth currentUser
  return (fmap userRoles opt)

getProblemID :: AppHandler (Maybe ProblemID)
getProblemID = fmap ProblemID <$> getParam "problem"

getSubmitID :: AppHandler (Maybe SubmitID)
getSubmitID = do opt <- getParam "submit"
                 return $ do bs <- opt
                             case reads (B.toString bs) of
                               [(i,"")] -> Just (SubmitID i)
                               _ -> Nothing



-- | try a Maybe-handler and "pass" if it yields Nothing 
require :: AppHandler (Maybe a) -> AppHandler a
require handler = do
  opt <- handler
  case opt of
    Nothing -> pass
    Just v -> return v


-- | get a text parameter from a POST request
getTextPost :: MonadSnap m => ByteString -> m (Maybe Text)
getTextPost name =
  do opt <- getPostParam name
     return ((T.filter (/='\r') . T.decodeUtf8With T.ignore) <$> opt)


---------------------------------------------------------------------
-- | error handlers
---------------------------------------------------------------------    
unauthorized, badRequest, notFound :: AppHandler a
unauthorized = render "_unauthorized" >> finishError 401 "Unauthorized"
badRequest   = render "_badrequest" >> finishError 400 "Bad request"
notFound     = render "_notfound" >> finishError 404 "Not found"


internalError :: SomeException -> AppHandler a
internalError e
  = do renderWithSplices  "_internalerror" 
             ("errorMsg" ## I.textSplice (T.pack $ show e))
       finishError 500 "Internal Server Error"


-- | finish with an http error
finishError :: MonadSnap m => Int -> ByteString -> m a
finishError code msg = do
  modifyResponse (setResponseStatus code msg)
  r <- getResponse
  finishWith r
  


-- if/then/else conditional splice
-- split children around the <else/> element; removes the <else/> element
ifElseISplice :: Monad m => Bool -> I.Splice m
ifElseISplice cond = getParamNode >>= (rewrite . X.childNodes)
  where rewrite nodes = 
          let (ns, ns') = break (\n -> X.tagName n==Just "else") nodes
          in I.runNodeList $ if cond then ns else (drop 1 ns') 

conditionalSplice :: Monad m => Bool -> I.Splice m
conditionalSplice = ifElseISplice
    


-- | make a checkbox input for a tag filter
checkboxInput :: Text -> Bool -> Bool -> [X.Node]
checkboxInput value checked disabled
  = [X.Element "label" attrs' [X.Element "input" attrs [], X.TextNode value]]
  where attrs = [ ("checked", "checked") | checked ] ++
                [ ("disabled", "disabled") | disabled ] ++
                [ ("type", "checkbox"), 
                  ("name", "tag"), 
                  ("value", value), 
                  ("onclick", "this.form.submit();")
                ]
        attrs' = [ ("class","disabled") | disabled ]
                


jsTimer :: String -> NominalDiffTime -> [X.Node]
jsTimer id secs
  = [X.Element "span" [("id",T.pack id),
                       ("class", "js-timer")] [],
     javascript $ T.pack $
     "start_countdown(" ++ show id ++ "," ++ show (floor secs :: Int) ++ ");"]

javascript :: Text -> X.Node
javascript txt = X.Element "script" [("type","text/javascript")] [X.TextNode txt]




-- aquire and release a text temporary file
withTextTemp :: String -> Text -> (FilePath -> IO a) -> IO a
withTextTemp name txt cont = withTempFile name cont'
   where cont' (f,h) = T.hPutStr h txt >> hClose h >> cont f


withTempFile :: String -> ((FilePath,Handle) -> IO a) -> IO a
withTempFile name k = bracket createTemp (\(f,_)->removeFile f) k
  where createTemp = do
          tempDir <- getTemporaryDirectory
          openTempFileWithDefaultPermissions tempDir name

                               

    
