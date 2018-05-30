{-# LANGUAGE OverloadedStrings #-}
{-
  Some helper handlers for our application monad
-}
module Codex.Utils where

import           Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as B

import           Data.Char(toUpper)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T

import           Data.Maybe (fromMaybe, listToMaybe, maybeToList)
import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.Map.Syntax

import           Snap.Core hiding (path)
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Auth
import           Snap.Snaplet.SqliteSimple
import           Snap.Snaplet.Router

import           Heist
import qualified Heist.Splices as I
import qualified Heist.Interpreted as I

import qualified Text.XmlHtml as X



import           Control.Monad.State
import           Control.Exception (SomeException)

import qualified Data.Configurator as Configurator

import           Codex.Types
import           Codex.Page
import           Codex.Interval
import           Codex.Application

import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.Format

import           System.FilePath
import qualified System.FastLogger as FastLogger

-- interpreted splices for handlers
type ISplices = Splices (I.Splice Codex)

-- | fetch document root directory from config file
getDocumentRoot :: Codex FilePath
getDocumentRoot = do
  conf <- getSnapletUserConfig
  liftIO (Configurator.require conf "system.document_root")

getStaticRoot :: Codex FilePath
getStaticRoot = do
  conf <- getSnapletUserConfig
  liftIO (Configurator.require conf "system.static_root")


-- | lookup full name for a user login in Db
queryFullname :: UserLogin -> Codex (Maybe Text)
queryFullname uid = do
  list <- query "SELECT meta_json FROM snap_auth_user WHERE login = ?" (Only uid)
  return $ case list of
    [] -> Nothing
    (txt:_) -> do
      hm <- decodeStrict (T.encodeUtf8 txt) :: Maybe (HM.HashMap Text Text)
      HM.lookup "fullname" hm


-- | Get current logged in user ID (if any)
--   from the authentication snaplet
getUserLogin :: Codex (Maybe UserLogin)
getUserLogin = do
  mAu <- with auth currentUser
  return (fmap authUserLogin mAu)

getFullname :: Codex (Maybe Text)
getFullname = do
  mAu <- with auth currentUser
  return (mAu >>=  authFullname)


authUserLogin :: AuthUser -> UserLogin
authUserLogin = UserLogin . userLogin

authFullname :: AuthUser -> Maybe Text
authFullname au
  = case HM.lookup "fullname" (userMeta au) of
    Just (String name) -> Just name
    _                  -> Nothing

-- | Get user id and roles
getUserRoles :: Codex (Maybe [Role])
getUserRoles = do
  mAu <- with auth currentUser
  return (fmap userRoles mAu)


isAdmin :: AuthUser -> Bool
isAdmin au = Role "admin" `elem` userRoles au

-- | run a handle only under administrative login
withAdmin :: Codex a -> Codex a
withAdmin action = do
  optlist <- getUserRoles
  case optlist of
    Nothing -> unauthorized
    Just roles -> if Role "admin" `elem` roles then action
                    else unauthorized


-- | get all events 
getEvents :: Codex Events
getEvents = do
  uevs <- maybe [] userEvents <$> with auth currentUser
  dbevs <- query_ "SELECT name, time FROM events"
  let evs = uevs ++ dbevs
  return (`lookup` evs)

-- | events associated with a user account
userEvents :: AuthUser -> [(Text, UTCTime)]
userEvents au = [(n, t) | (n,f)<-fields, t <- maybeToList (f au)]
  where fields =  [("activation", userActivatedAt),
                   ("creation", userCreatedAt),
                   ("update", userUpdatedAt),
                   ("login", userCurrentLoginAt)]


-- | get submission id from request parameters
getSubmitId :: Codex (Maybe SubmitId)
getSubmitId = fmap SubmitId <$> readParam "sid"


-- |  use Read instance to decode an HTTP parameter 
readParam :: (Read a, MonadSnap m) => ByteString -> m (Maybe a)
readParam name = do
  opt <- getParam name
  return $ do bs <- opt
              case reads (B.toString bs) of
                [(x, "")] -> Just x
                _   -> Nothing


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


-- | get a parameter with a default value
getParamDef :: MonadSnap m => ByteString -> ByteString -> m ByteString
getParamDef name def = fromMaybe def <$> getParam name

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


-----------------------------------------------------------------------------

-- | splices related to a page
pageSplices  :: Monad m => Page -> Splices (I.Splice m)
pageSplices page = do
  "page-description" ## return (pageToHtml page)
  "if-exercise" ## I.ifElseISplice (pageIsExercise page)


pageUrlSplices :: FilePath -> ISplices
pageUrlSplices rqpath = do
  let path = splitDirectories rqpath
  let parent= if null path then [] else init path ++ ["index.md"]
  "page-url" ## urlSplice (Page path)
  "page-parent-url" ## urlSplice (Page parent)

fileUrlSplices :: FilePath -> ISplices
fileUrlSplices rqpath = do
  let path = splitDirectories rqpath
  let parent = if null path then [] else init path
  "file-path" ## I.textSplice (T.pack rqpath)
  "file-url" ## urlSplice (Files path)
  "file-parent-url" ## urlSplice (Files parent)




-- list of messages
messageSplices :: Monad m => [Text] -> Splices (I.Splice m)
messageSplices mesgs = do
  "message-list" ## I.mapSplices (I.runChildrenWith . splice) mesgs
  where splice msg = "message" ## I.textSplice msg




tagCaseSplice :: Monad m => Text -> I.Splice m
tagCaseSplice tag = getParamNode >>= (I.runNodeList . select . X.childNodes)
   where
     select nodes = case filter (\n -> X.tagName n==Just tag ||
                                       X.tagName n==Just "default") nodes of
       [] -> []
       (n:_) -> X.childNodes n

caseSplice :: (Monad m, Show a) => a -> I.Splice m
caseSplice v = tagCaseSplice (T.pack $ show v)



--------------------------------------------------------------
 {-
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
-}

checkboxInput :: [(Text,Text)] -> [X.Node] -> X.Node
checkboxInput attrs contents = X.Element "input" attrs' contents
  where attrs' = ("type","checkbox") : attrs


jsTimer :: String -> NominalDiffTime -> [X.Node]
jsTimer id secs
  = [X.Element "span" [("id",T.pack id),
                       ("class", "js-timer")] [],
     javascript $ T.pack $
     "start_countdown(" ++ show id ++ "," ++ show (floor secs :: Int) ++ ");"]

javascript :: Text -> X.Node
javascript txt
  = X.Element "script" [("type","text/javascript")] [X.TextNode txt]

javascriptSrc :: Text -> X.Node
javascriptSrc  url
  = X.Element "script" [("type","text/javascript"), ("src",url)] []

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



-- | log an authentication message
logMsg :: ByteString -> Codex ()
logMsg msg = do
  logger <- gets _logger
  liftIO $ FastLogger.logMsg logger =<< FastLogger.timestampedLogEntry msg


