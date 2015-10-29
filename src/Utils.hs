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
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T

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



-- | get SafeExec configuration
getSandbox :: Pythondo Sandbox
getSandbox = gets _sandbox


-- | get LDAP configuration parameters
getLdapConf :: Pythondo LdapConf
getLdapConf = gets _ldapConf


-- | get printout configuration parameters
getPrintConf :: Pythondo PrintConf
getPrintConf = gets _printConf


-- | increment an EKG counter
incrCounter :: Text -> Pythondo ()
incrCounter name 
  = gets _ekg >>= 
    maybe (return ())  (\ekg -> liftIO $ getCounter name ekg >>= Counter.inc) 



-- | Get current logged in user; fail with unauthorized error if missing
-- getRequiredUserID :: Pythondo UID
-- getRequiredUserID = require getUserID <|> unauthorized

-- | Get current logged in user ID (if any)
--   from the authentication snaplet  
getUserID :: Pythondo (Maybe UID)
getUserID = do 
  opt <- with auth currentUser
  return $ fmap (UID . B.fromString . T.unpack . userLogin) opt


getFullName :: Pythondo (Maybe Text)
getFullName = do
  opt <- with auth currentUser
  return (fmap userName opt)

-- | Get user id and roles
getUserRoles :: Pythondo (Maybe [Role])
getUserRoles = do
  opt <- with auth currentUser
  return (fmap userRoles opt)

getProblemID :: Pythondo (Maybe PID)
getProblemID = fmap PID <$> getParam "pid"

getSubmissionID :: Pythondo (Maybe SID)
getSubmissionID = fmap (SID . read . B.toString) <$> getParam "sid"



-- | try a Maybe-handler and "pass" if it yields Nothing 
require :: Pythondo (Maybe a) -> Pythondo a
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
unauthorized, badRequest, notFound :: Pythondo a
unauthorized = render "_unauthorized" >> finishError 401 "Unauthorized"
badRequest   = render "_badrequest" >> finishError 400 "Bad request"
notFound     = render "_notfound" >> finishError 404 "Not found"


internalError :: SomeException -> Pythondo a
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
conditionalSplice ::  Bool -> I.Splice Pythondo
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

                               

    
