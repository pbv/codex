{-# LANGUAGE OverloadedStrings #-}
{-
  Some helper handlers for our application monad
-}
module Utils where

-- import           Control.Concurrent.MVar
import           Control.Monad.State
import           Data.Text(Text)
-- import qualified Data.Text
import           Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as B

-- import           Data.Text (Text)
import qualified Data.Text as T

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
-- import Snap.Snaplet.Session

import           Heist
-- import           Heist.SpliceAPI
import qualified Heist.Interpreted as I

import qualified Text.XmlHtml as X


import Data.Configurator
import Data.Configurator.Types


import Control.Exception (SomeException)

-- import System.Process
-- import System.Exit (ExitCode)
import System.Remote.Monitoring
import System.Remote.Counter as Counter

-- import Paths_pythondo(version)

import Application
import Types
import LdapAuth

-- sublist checking
isSublistOf :: Eq a => [a] -> [a] -> Bool
isSublistOf xs ys = all (`elem`ys) xs


getConfigured :: Configured a => Name -> a -> AppHandler a
getConfigured key def = do 
  conf <- gets config
  liftIO $ lookupDefault def conf key 


ifConfigured :: Name -> AppHandler Bool
ifConfigured key = do
  conf <- gets config
  liftIO $ lookupDefault False conf key


-- | get current SafeExec parameters from configurator
getSafeExec :: AppHandler SafeExec
getSafeExec = do 
  conf<-gets config
  liftIO $ getSafeExec' conf

getSafeExec' :: Config -> IO SafeExec
getSafeExec' conf = do 
  python <- require conf "submissions.python"
  safeexec<-require conf "submissions.safeexec"
  cpu <- require conf "submissions.max_cpu"
  clock <- require conf "submissions.max_clock"
  mem <- require conf "submissions.max_memory"
  return SafeExec { safeExec = safeexec
                  , pythonExec= python
                  , maxCpuTime = cpu 
                  , maxClockTime = clock
                  , maxMemory = mem 
                  }


-- | get LDAP configuration parameters
getLdapConf :: AppHandler LdapConf
getLdapConf = do 
  conf <- gets config
  liftIO $ do uri <- require conf "ldap.uri"
              bases <- require conf "ldap.bases"
              return (LdapConf uri bases)

-- | get printout configuration parameters
getPrintout :: AppHandler Printout              
getPrintout = do
  conf <- gets config
  liftIO $ do enabled <- lookupDefault False conf "printouts.enabled"
              header <- lookupDefault defaultHeader conf "printouts.header"
              opts <- lookupDefault [] conf "printouts.options"
              return (Printout enabled header opts)
  where
    defaultHeader = "Pythondo"

-- | increment an EKG counter
incrCounter :: Text -> AppHandler ()
incrCounter name 
  = gets ekgServer >>= 
    maybe (return ())  (\ekg -> liftIO $ getCounter name ekg >>= Counter.inc) 


-- | Get current logged in user; fail with unauthorized error if missing
getLoggedUser :: AppHandler UID
getLoggedUser = do 
  opt <- with auth currentUser
  case opt of
    Nothing -> unauthorized
    Just au -> return (UID . B.fromString $ T.unpack $ userLogin au)



getFullName :: AppHandler Text
getFullName = do 
  opt <- with auth currentUser
  case opt of 
    Nothing -> unauthorized
    Just au -> return (userName au)



-- | Get a required parameter; fails if missing
getRequiredParam :: ByteString -> AppHandler ByteString    
getRequiredParam p = do 
  opt <- getParam p
  case opt of 
    Nothing -> badRequest
    Just v -> return v



-- | error handlers
unauthorized, badRequest, notFound :: AppHandler a
unauthorized = render "_unauthorized" >> finishError 401 "Unauthorized"
badRequest = render "_badrequest" >> finishError 400 "Bad request"
notFound = render "_notfound" >> finishError 404 "Not found"


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
                
