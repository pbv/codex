{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------

import           Control.Applicative
import           Control.Concurrent(forkIO, ThreadId)
import           Control.Concurrent.MVar
import           Control.Concurrent.QSem
import           Control.Exception  (SomeException, bracket_)
import           Control.Exception.Lifted  (catch)
import           Control.Lens
import           Control.Monad.State

import           Data.ByteString.UTF8                        (ByteString)

import qualified Data.HashMap.Strict                         as HM
import           Data.Map.Syntax

import qualified Data.Text                                   as T

import           Heist
import qualified Heist.Interpreted                           as I
import           Heist.Splices                               as I
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.SqliteSimple
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session                        (touchSession)
import           Snap.Snaplet.Session.Backends.CookieSession
import qualified Snap.Snaplet.SqliteSimple                   as S
import           Snap.Util.FileServe                         (fileType,
                                                              getSafePath,
                                                              serveDirectory,
                                                              serveFileAs)

import           Data.Time.Clock
import           Data.Time.LocalTime

import           System.Directory                            (doesFileExist)
import           System.FilePath


------------------------------------------------------------------------------
import           AceEditor
import           AdminHandlers
import           Application
import           AuthHandlers
import           Config
import           Db
import           Interval
import           Language
import           Page
import           Submission
import           Types
import           Tester
import           Utils

import           Data.Version                                (showVersion)
import           Paths_codex                                 (version)


import           Text.Pandoc                                 hiding (Code)
import           Text.Pandoc.Walk                            as Pandoc



-- | handle page requests
handlePage :: Codex ()
handlePage = do
  uid <- require getUserID <|> unauthorized
  with sess touchSession   -- refresh inactivity time-out
  rqpath <- getSafePath
  method GET (handleGet uid rqpath <|> handleRedir rqpath) <|>
   method POST (handlePost uid rqpath)
  where
    handleGet uid rqpath =  do
      let filepath = publicPath </> rqpath
      c <- liftIO (doesFileExist filepath)
      unless c pass
      -- serve according to mime type
      let mime = fileType mimeTypes rqpath
      if mime == "text/markdown" then
        servePage uid rqpath
        else
        serveFileAs mime filepath

    handleRedir rqpath = do
        let filepath = publicPath </> rqpath </> "index.md"
        c <- liftIO (doesFileExist filepath)
        if c then
          redirect (encodePath ("/pub" </> rqpath </> "index.md"))
          else
          notFound

    handlePost uid rqpath = do
      let filepath = publicPath </> rqpath
      c <- liftIO $ doesFileExist filepath
      guard (c && fileType mimeTypes rqpath == "text/markdown")
      page <- liftIO (readPage publicPath rqpath)
      -- ensure the request is for an exercise page
      guard (pageIsExercise page)
      text <- require (getTextPost "editform.editor")
      lang <- require (return $ pageLanguage page)
      sid <- newSubmission uid page (Code lang text)
      redirect (encodePath $ "/submited" </> show (fromSID sid))


-- | serve a markdown document
servePage :: UserID -> FilePath -> Codex ()
servePage uid rqpath = do
  page <- readPageLinks uid rqpath
  if pageIsExercise page then
    renderExercise page =<< getPageSubmissions uid rqpath
    else
    renderWithSplices "page" (pageSplices page)

--  render an exercise page
renderExercise :: Page -> [Submission] -> Codex ()
renderExercise page subs = do
  tz <- liftIO getCurrentTimeZone
  splices <- exerciseSplices page
  renderWithSplices "exercise" $ do
    splices
    pageSplices page
    submissionListSplices tz subs
    inputAceEditorSplices

-- render report for a single submission
renderReport :: Page -> Submission -> Codex ()
renderReport page sub = do
  tz <- liftIO getCurrentTimeZone
  splices <- exerciseSplices page
  renderWithSplices "report" $ do
    splices
    inputAceEditorSplices
    pageSplices page
    submitSplices tz sub




-- | read a page, collect exercise links
--  and patch with titles of linked pages
readPageLinks :: UserID -> FilePath -> Codex Page
readPageLinks uid rqpath = do
  page <- liftIO $ readPage publicPath rqpath
  let links = queryExerciseLinks page
  let dir = takeDirectory rqpath
  -- fetch linked titles; catch and ignore exceptions
  optTitles <- liftIO $
               forM links $ \url ->
               (pageTitle <$> readPage publicPath (dir</>url))
               `catch` (\(_ :: SomeException) -> return Nothing)

  let titles = HM.fromList [(url,title) | (url, Just title)<-zip links optTitles]
  --
  -- fetch submisssion count
  submissions <- HM.fromList <$>
                 forM links ( \url -> do
                     count <- length <$> getPageSubmissions uid (dir</>url)
                     return (url, count))
  --
  -- patch relevant links
  let patch elm@(Link attr@(_, classes, _) inlines target@(url, _))
        | "ex" `elem` classes =
             let title = if null inlines then HM.lookupDefault [] url titles
                         else inlines
                 count =  HM.lookupDefault 0 url submissions
             in Span nullAttr
                     [Link attr title target,
                      LineBreak,
                      Span ("",["info"],[])
                      [Str "(", Str (show count), Space,
                       Str "submissões", Str ")"]
                     ]
        | otherwise = elm
      patch elm = elm
  --
  return page { pageDescription = walk patch (pageDescription page) }




-- | collect all exercise links from a page
queryExerciseLinks :: Page -> [String]
queryExerciseLinks page = query extractURL (pageDescription page)
  where
    extractURL (Link attr@(_,classes,_) inlines target@(url,_))
      = [url | "ex" `elem` classes]
    extractURL _  = []



-- | handle requests for a single submission
handleSubmission :: Codex ()
handleSubmission = do
    au <- require (with auth currentUser) <|> unauthorized
    let uid = authUserID au
    sid <- require getSubmitID
    sub <- require (getSubmission sid) <|> notFound
    unless (isAdmin au || submitUser sub == uid)
      unauthorized
    handleMethodOverride (method GET (handleGet sub) <|>
                          method PATCH (handleReevaluate sub) <|>
                          method DELETE (handleDelete sub))
  where
    -- get report on a submission
    handleGet sub = do
      page <- liftIO $ readPage publicPath (submitPath sub)
      renderReport page sub
    -- delete a submission
    handleDelete sub = do
      deleteSubmission (submitID sub)
      redirect (encodePath ("/pub" </> submitPath sub))
    -- revaluate a submissionListSplices
    handleReevaluate sub = do
      evaluate sub
      redirect (encodePath ("/submited" </> show (fromSID $ submitID sub)))



-- | splices related to exercises
exerciseSplices :: Page -> Codex ISplices
exerciseSplices page = do
  tz <- liftIO getCurrentTimeZone
  now <- liftIO getCurrentTime
  events <- getEvents
  case evalI tz events (submitInterval page) of
    Nothing ->  error "invalid submission interval"
    Just interval -> return $ do
      "language" ##
        maybe (return []) (I.textSplice . fromLanguage) (pageLanguage page)
      "language-mode" ##
        maybe (return []) (I.textSplice . languageMode) (pageLanguage page)
      "code-text" ##
        maybe (return []) I.textSplice (pageCodeText page)
      "valid-from" ##
        I.textSplice $ maybe "N/A" (showTime tz) (Interval.lower interval)
      "valid-until" ##
        I.textSplice $ maybe "N/A" (showTime tz) (Interval.higher interval)
      "case-timing" ## caseSplice (timing now interval)




-- | splices relating to a list of submissions
submissionListSplices :: TimeZone -> [Submission] -> ISplices
submissionListSplices tz list = do
  let count = length list
  "submissions-count" ##
    I.textSplice (T.pack $ show count)
  "if-submitted" ##
    I.ifElseISplice (count > 0)
  "submissions-list" ##
    I.mapSplices (I.runChildrenWith . submitSplices tz) list

{-
-- | splice for time expressions and intervals
timeExprSplice :: TimeExpr -> I.Splice Codex
timeExprSplice e = do
  tz <- liftIO getCurrentTimeZone
  events <- lift (require getUserEvents)
  case evalT tz events e of
    Just t -> I.textSplice (showTime tz t)
    Nothing -> do logError "invalid time expression"
                  return []

timingSplice :: Interval TimeExpr -> I.Splice Codex
timingSplice interval = do
  tz <- liftIO getCurrentTimeZone
  now <- liftIO getCurrentTime
  env <- lift (require getUserEvents)
  case Interval.evalI tz env interval of
    Just i -> caseSplice (timing now i)
    Nothing -> return []
-}



------------------------------------------------------------------------------
-- | The application's routes.

routes :: [(ByteString, Codex ())]
routes = [ ("/login",    handleLogin `catch` internalError)
         , ("/logout",   handleLogout `catch` internalError)
         , ("/register", handleRegister `catch` internalError)
         , ("/pub",      handlePage `catch` internalError)
         , ("/submited/:sid", handleSubmission `catch` internalError)
         , ("/submited",  handleSubmissionList `catch` internalError)
         , ("/files",  handleBrowse publicPath `catch`  internalError)
         , ("/export", handleExport `catch` internalError)
         , ("/static",   serveDirectory staticPath <|> notFound)
         ]



-- | current logged in full user name
loggedInName :: SnapletLens b (AuthManager b) -> SnapletISplice b
loggedInName authmgr = do
    u <- lift $ withTop authmgr currentUser
    maybe (return []) I.textSplice (u >>= authFullname)


-- | splice for current date & time
nowSplice :: I.Splice Codex
nowSplice = do tz <- liftIO getCurrentTimeZone
               t <- liftIO getCurrentTime
               utcTimeSplice tz t



versionSplice :: I.Splice Codex
versionSplice = I.textSplice (T.pack (showVersion version))


-- | handle a new submission
-- insert a pending submission and start
-- run code tester in separate thread
newSubmission :: UserID -> Page -> Code -> Codex SubmitID
newSubmission uid page code = do
  now <- liftIO getCurrentTime
  sub <- insertSubmission uid (pagePath page) now code evaluating Valid
  evaluate sub
  return (submitID sub)

-- (re)evaluate a submission;
-- runs code tester in separate thread
evaluate :: Submission -> Codex ThreadId
evaluate sub = do
  let sid = submitID sub            -- ^ submission number
  let code = submitCode sub         -- ^ program code
  let received = submitTime sub     -- ^ time received
  page <- liftIO $ readPage publicPath (submitPath sub)
  evs <- getEvents
  tv <- liftIO $ evalTiming page evs received
  conf <- getSnapletUserConfig
  sqlite <- S.getSqliteState
  evQS <- gets evalQS
  liftIO $ forkIO $ bracket_ (waitQSem evQS) (signalQSem evQS) $ do
    updateSubmission sqlite sid evaluating tv
    result <- codeTester conf page code `catch`
               (\ (e::SomeException) -> return (miscError $ T.pack $ show e))
    updateSubmission sqlite sid result tv


-- | evaluate timing for a page
evalTiming :: Page -> Events -> UTCTime -> IO Timing
evalTiming page evs t = do
  tz <- getCurrentTimeZone
  case Interval.evalI tz evs (submitInterval page) of
    Nothing -> ioError (userError errMsg)
    Just int -> return (timing t int)
 where
   errMsg = show (pagePath page) ++ ": invalid submit interval"


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app =
  makeSnaplet "codex" "Web server for programming exercises." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" Nothing (Just 3600)
    d <- nestSnaplet "db" db S.sqliteInit
    a <- nestSnaplet "auth" auth $ initSqliteAuth sess d
    addAuthSplices h auth
    let sc = do
          "version" ## versionSplice
          "timeNow" ## nowSplice
          "evaluating" ## return []
          "loggedInName" ## loggedInName auth
          "ifAdmin" ## do
            mbAu <- lift (withTop auth currentUser)
            I.ifElseISplice (maybe False isAdmin mbAu)
    addConfig h (mempty & scInterpretedSplices .~ sc)
    -- Grab the DB connection pool from the sqlite snaplet and call
    -- into the Model to create all the DB tables if necessary.
    let c = S.sqliteConn $ d ^# snapletValue
    liftIO $ withMVar c $ \conn -> Db.createTables conn
    addRoutes routes
    -- get max # of concurrent evaluation threads
    conf <- getSnapletUserConfig
    evQS <- liftIO $ getEvalQS "system.workers" conf
    return App { _heist = h
               , _sess = s
               , _auth = a
               , _db   = d
               , evalQS = evQS
               }


{-
-- initialize EKG server (if configured)
initEkg :: Config -> IO (Maybe Server)
initEkg conf = do
  enabled <- Configurator.require conf "ekg.enabled"
  if enabled then do
    host <- Configurator.require conf "ekg.host"
    port <- Configurator.require conf "ekg.port"
    Just <$> forkServer host port
    else
    return Nothing
  -}
