{-# LANGUAGE OverloadedStrings #-}
{-
  Some helper handlers for our application monad
-}
module Utils where

import           Control.Monad.State
import           Data.Text(Text)
import           Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as B
import qualified Data.Text as T

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

import           Application
import           Types
import           LdapAuth

import           Data.Time.Clock


-- | read configuration parameters 
configSandbox :: Config -> IO Sandbox
configSandbox conf = do
    python   <- Configurator.require conf "submissions.python"
    safeexec <-Configurator.require conf "submissions.safeexec"
    cpu   <- Configurator.require conf "submissions.max_cpu"
    clock <- Configurator.require conf "submissions.max_clock"
    mem   <- Configurator.require conf "submissions.max_memory"
    return Sandbox { safeExec = safeexec
                   , pythonExec= python
                   , maxCpuTime = cpu 
                   , maxClockTime = clock
                   , maxMemory = mem 
                   }

configPrintout :: Config -> IO Printout
configPrintout conf = do
  enabled <- Configurator.lookupDefault False conf "printouts.enabled"
  header <- Configurator.lookupDefault defaultHeader conf "printouts.header"
  opts <- Configurator.lookupDefault [] conf "printouts.options"
  return (Printout enabled header opts)
  where defaultHeader = "Pythondo"

configLdapConf :: Config -> IO LdapConf
configLdapConf conf = do
  uri <- Configurator.require conf "ldap.uri"
  bases <- Configurator.require conf "ldap.bases"
  return (LdapConf uri bases)



-- | get SafeExec configuration
getSandbox :: AppHandler Sandbox
getSandbox = gets _sandbox


-- | get LDAP configuration parameters
getLdapConf :: AppHandler LdapConf
getLdapConf = gets _ldapConf


-- | get printout configuration parameters
getPrintout :: AppHandler Printout              
getPrintout = gets _printout


-- | increment an EKG counter
incrCounter :: Text -> AppHandler ()
incrCounter name 
  = gets _ekg >>= 
    maybe (return ())  (\ekg -> liftIO $ getCounter name ekg >>= Counter.inc) 



-- | Get current logged in user; fail with unauthorized error if missing
-- getRequiredUserID :: AppHandler UID
-- getRequiredUserID = require getUserID <|> unauthorized

-- | Get current logged in user ID (if any)
--   from the authentication snaplet  
getUserID :: AppHandler (Maybe UID)
getUserID = do 
  opt <- with auth currentUser
  return $ fmap (UID . B.fromString . T.unpack . userLogin) opt


getFullName :: AppHandler (Maybe Text)
getFullName = do 
  opt <- with auth currentUser
  return (fmap userName opt)


getProblemID :: AppHandler (Maybe PID)
getProblemID = fmap PID <$> getParam "pid"

getSubmissionID :: AppHandler (Maybe SID)
getSubmissionID = fmap (SID . read . B.toString) <$> getParam "sid"



-- | try a Maybe-handler and "pass" if it yields Nothing 
require :: AppHandler (Maybe a) -> AppHandler a
require handler = do
  opt <- handler
  case opt of
    Nothing -> pass
    Just v -> return v


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
  
{-
-- simple conditional splice
conditionalSplice :: Monad m => Bool -> I.Splice m
conditionalSplice cond = if cond then I.runChildren else return []
-}


-- if/then/else conditional splice
-- split children around the <else/> element; removes the <else/> element
ifElseISplice :: Monad m => Bool -> I.Splice m
ifElseISplice cond = getParamNode >>= (rewrite . X.childNodes)
  where rewrite nodes = 
          let (ns, ns') = break (\n -> X.tagName n==Just "else") nodes
          in I.runNodeList $ if cond then ns else (drop 1 ns') 

conditionalSplice :: Monad m => Bool -> I.Splice m
conditionalSplice = ifElseISplice
    

{-
-- | a splice for conditionals with a nested <else>...</else>
conditionalSplice ::  Bool -> I.Splice AppHandler
conditionalSplice cond = localParamNode rewrite I.runChildren
  where rewrite 
          = if cond then 
              -- filter out any <else>... </else> node
               filterChildren (\n -> X.tagName n/=Just "else")  
            else
              -- locate first <else>.. </else> child; 
              -- return empty element if not found
              \node -> case X.childElementTag "else" node of
                Just node' -> node'   
                Nothing -> X.Element "else" [] []

        
-- | filter children of a node          
filterChildren :: (X.Node -> Bool) -> X.Node -> X.Node
filterChildren f (X.Element tag attrs children) 
  = X.Element tag attrs (filter f children)
filterChildren _ node = node    
-}


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
                


jsTimer :: PID -> NominalDiffTime -> [X.Node]
jsTimer pid secs
  = [X.Element "span" [("id",T.pack id),
                       ("class", "js-timer")] [],
     javascript $ T.pack $
     "start_countdown(" ++ show id ++ "," ++ show (floor secs :: Int) ++ ");"]
  where id = show pid ++ "-js-timer"

javascript txt = X.Element "script" [("type","text/javascript")] [X.TextNode txt]
