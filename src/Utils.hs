{-# LANGUAGE OverloadedStrings #-}
{-
  Some helper handlers for our application monad
-}
module Utils where

-- import           Control.Monad.State
import           Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as B
import qualified Data.ByteString      as B
import           Data.Char(toUpper)
import           Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
-- import qualified Data.Text.IO as T
import           Data.List (intersperse)
import           Data.Maybe (fromMaybe, listToMaybe, maybeToList)
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HM
import           Data.Map.Syntax

import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Session

import           Data.Aeson.Types 

import           Heist
-- import           Heist.Splices
import qualified Heist.Interpreted as I

import qualified Text.XmlHtml as X



-- import           Control.Applicative 
import           Control.Exception (SomeException)

import           Types
import           Interval
import           Application

import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.Format

import           System.FilePath



authUserID :: AuthUser -> UserID
authUserID = UserID . T.encodeUtf8 . userLogin

authFullname :: AuthUser -> Maybe Text
authFullname au 
  = case HM.lookup "fullname" (userMeta au) of
    Just (String name) -> Just name
    _                  -> Nothing


isAdmin :: AuthUser -> Bool
isAdmin au = Role "admin" `elem` userRoles au


-- | Get current logged in user ID (if any)
--   from the authentication snaplet  
getUserID :: Codex (Maybe UserID)
getUserID = do 
  mAu <- with auth currentUser
  return (fmap authUserID mAu)


getFullname :: Codex (Maybe Text)
getFullname = do
  mAu <- with auth currentUser
  return (mAu >>=  authFullname)

-- | Get user id and roles
getUserRoles :: Codex (Maybe [Role])
getUserRoles = do
  mAu <- with auth currentUser
  return (fmap userRoles mAu)


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
getSubmitID = fmap SubmitID <$> readParam "sid"

readParam :: (Read a, MonadSnap m) => ByteString -> m (Maybe a)
readParam name = do
  opt <- getParam name
  return $ do bs <- opt
              case reads (B.toString bs) of
                [(x, "")] -> Just x
                _   -> Nothing


-- get tag list from query string
getQueryTags :: Codex [Text]
getQueryTags = do
  params <- getParams
  return (map T.decodeUtf8 $ Map.findWithDefault [] "tag" params)



-- | try a Maybe-handler and "pass" if it yields Nothing 
require :: Handler m m' (Maybe a) -> Handler m m' a
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
  


-- | splice an UTC time as a local time string
utcTimeSplice :: Monad m => TimeZone -> UTCTime -> I.Splice m
utcTimeSplice tz t =
  I.textSplice $ T.pack $ formatTime defaultTimeLocale "%c" $ utcToLocalTime tz t


-- list of messages
messageSplices :: Monad m => [Text] -> Splices (I.Splice m)
messageSplices mesgs = do
  "message-list" ## I.mapSplices (I.runChildrenWith . splice) mesgs
  where splice msg = "message" ## I.textSplice msg



-- if/then/else conditional splice
-- split children around the <else/> element; removes the <else/> element
{-
ifElseISplice :: Monad m => Bool -> I.Splice m
ifElseISplice cond = getParamNode >>= (rewrite . X.childNodes)
  where rewrite nodes = 
          let (ns, ns') = break (\n -> X.tagName n==Just "else") nodes
          in I.runNodeList $ if cond then ns else (drop 1 ns') 
-}

-- conditionalSplice :: Monad m => Bool -> I.Splice m
-- conditionalSplice = ifElseISplice


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



-- | set list element containtment
contained :: Eq a => [a] -> [a] -> Bool
contained xs ys = all (`elem`ys) xs




-------------------------------------------------------------------------------

-- | Wrap a handler with method override support. This means that if
-- (and only if) the request is a POST, _method param is passed, and
-- it is a parsable method name, it will change the request method to
-- the supplied one. This works around some browser limitations.
-- Adapted from Snap.Extras.MethodOverride 
handleMethodOverride :: MonadSnap m =>
                        m a
                      -- ^ Internal handler to call
                      -> m a
handleMethodOverride = (modifyRequest (methodOverride "_method") >>)


-------------------------------------------------------------------------------
methodOverride :: ByteString -> Request -> Request
methodOverride param r
  | rqMethod r == POST = r { rqMethod = overridden }
  | otherwise          = r
  where
    overridden = fromMaybe POST $ do
      meth <- listToMaybe =<< rqParam param r
      case map toUpper (B.toString meth) of
       "HEAD"    -> Just HEAD
       "POST"    -> Just POST
       "PUT"     -> Just PUT
       "DELETE"  -> Just DELETE
       "TRACE"   -> Just TRACE
       "OPTIONS" -> Just OPTIONS
       "CONNECT" -> Just CONNECT
       "PATCH"   -> Just PATCH
       ""        -> Nothing
       _         -> Just (Method meth)
       


-- | encode a file path as a URL
encodePath :: FilePath -> ByteString
encodePath path = B.concat (intersperse "/" dirs')
  where dirs = map (urlEncode . B.fromString) (splitDirectories path)
        dirs'= if isAbsolute path then "":tail dirs else dirs

    
