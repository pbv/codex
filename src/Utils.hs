{-# LANGUAGE OverloadedStrings #-}
{-
  Some helper handlers for our application monad
-}
module Utils where

import           Control.Monad.State
import           Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as B
import           Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
-- import qualified Data.Text.IO as T
import           Data.String
import           Data.Maybe (maybeToList)
import qualified Data.Map as Map

import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Auth

import           Heist
import qualified Heist.Interpreted as I

import qualified Text.XmlHtml as X



import           Control.Applicative 
import           Control.Exception (SomeException)

import           System.Remote.Monitoring
import           System.Remote.Counter as Counter

-- import           System.Directory
-- import           System.IO

import           Types
import           Interval
-- import           SafeExec
-- import           Language
import           Application
import           LdapAuth


import           Data.Time.Clock




-- | increment an EKG counter
incrCounter :: Text -> Codex ()
incrCounter name 
  = gets ekg >>= 
    maybe (return ())  (\ekg -> liftIO $ getCounter name ekg >>= Counter.inc) 



-- | Get current logged in user ID (if any)
--   from the authentication snaplet  
getUserID :: Codex (Maybe UserID)
getUserID = do 
  opt <- with auth currentUser
  return $ fmap (fromString . T.unpack . userLogin) opt


getFullName :: Codex (Maybe Text)
getFullName = do
  opt <- with auth currentUser
  return (fmap userName opt)

-- | Get user id and roles
getUserRoles :: Codex (Maybe [Role])
getUserRoles = do
  opt <- with auth currentUser
  return (fmap userRoles opt)


getUserEvents :: Codex (Maybe Events)
getUserEvents = fmap userEvents <$> with auth currentUser

-- | events associated with a user account
userEvents :: AuthUser -> Events
userEvents au = [(n, t) | (n,f)<-fields, t <- maybeToList (f au)]
  where fields =  [("activation", userActivatedAt),
                   ("creation", userCreatedAt),
                   ("update", userUpdatedAt),
                   ("login", userCurrentLoginAt)]
 


getSubmitID :: Codex (Maybe SubmitID)
getSubmitID = do opt <- getParam "sid"
                 return $ do bs <- opt
                             case reads (B.toString bs) of
                               [(i,"")] -> Just (SubmitID i)
                               _ -> Nothing



-- get tag list from query string
getQueryTags :: Codex [Text]
getQueryTags = do
  params <- getParams
  return (map (T.pack . B.toString) $ Map.findWithDefault [] "tag" params)



-- | try a Maybe-handler and "pass" if it yields Nothing 
require :: Codex (Maybe a) -> Codex a
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
unauthorized, badRequest, notFound :: Codex a
unauthorized = render "_unauthorized" >> finishError 401 "Unauthorized"
badRequest   = render "_badrequest" >> finishError 400 "Bad request"
notFound     = render "_notfound" >> finishError 404 "Not found"


internalError :: SomeException -> Codex a
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


tagCaseSplice :: Monad m => Text -> I.Splice m
tagCaseSplice tag = getParamNode >>= (I.runNodeList . select . X.childNodes)
   where
     select nodes = case filter (\n -> X.tagName n==Just tag ||
                                       X.tagName n==Just "default") nodes of
       [] -> []
       (n:_) -> X.childNodes n

caseSplice :: (Monad m, Show a) => a -> I.Splice m
caseSplice v = tagCaseSplice (T.pack $ show v)



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



                               

    
