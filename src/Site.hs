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
import           Control.Exception                           (SomeException)
import           Control.Exception.Lifted                    (catch)
import           Control.Lens
import           Control.Monad.State

import           Data.ByteString.UTF8                        (ByteString)
import           Data.Maybe                                  (fromMaybe)

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
      guard (pageIsExercise page)  -- only exercise pages
      text <- require (getTextPost "editform.editor")
      lang <- require (return $ pageLanguage page)
      sid <- newSubmission uid page (Code lang text)
      -- renderReport page sub
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
  t <- liftIO getZonedTime
  let tz = zonedTimeZone t
  renderWithSplices "exercise" $ do
    pageSplices page
    exerciseSplices t page
    submissionListSplices tz subs
    inputAceEditorSplices

-- render report for a single submission
renderReport :: Page -> Submission -> Codex ()
renderReport page sub = do
  t <- liftIO getZonedTime
  let tz = zonedTimeZone t
  renderWithSplices "report" $ do
    inputAceEditorSplices
    pageSplices page
    exerciseSplices t page
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
                       Str "submissions", Str ")"]
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




{-
renderIndex uid page links = do
  querytags <- getQueryTags
  pages <- liftIO $ mapM (Page.readPage publicPath) links
  let alltags = nub $ sort $ concatMap Page.getTags pages
  let pages' = filter (\p -> querytags `contained` Page.getTags p) pages
  let abletags = nub $ sort $ concatMap Page.getTags pages'
  subs <- mapM (Submission.getReport uid . Page.path) pages'
  tz <- liftIO getCurrentTimeZone
  renderWithSplices "indexsheet" $ do
    pageSplices page
    indexSplices tz (zip pages' subs)
    "if-tagged" ## I.ifElseISplice (not $ null alltags)
    "available" ## I.textSplice $ T.pack $ show $ length pages
    "visible" ## I.textSplice $ T.pack $ show $ length pages'
    "tag-list" ##  I.mapSplices (I.runChildrenWith .
                                 tagSplices querytags abletags) alltags

-- context-dependent splices for for a tag selection checkbox
tagSplices :: [Text] -> [Text] -> Text -> ISplices
tagSplices querytags enabled tag =
  let checked = tag `elem` querytags
      disabled= tag `notElem` enabled
       -- null (filter (isTagged tag) visible)
  in do "tag-text" ## I.textSplice tag
        "tag-checkbox" ## return (checkboxInput tag checked disabled)
-}







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
exerciseSplices :: ZonedTime -> Page -> ISplices
exerciseSplices t page = do
  -- splices related to programming languages
  "language" ##
    maybe (return []) (I.textSplice . fromLanguage) (pageLanguage page)
  "language-mode" ##
    maybe (return []) (I.textSplice . languageMode) (pageLanguage page)
  "code-text" ##
    maybe (return []) I.textSplice (pageCodeText page)
  --- splices related to submission intervals
  let valid = fromMaybe (return Always) $ pageValid t page
  case valid of
    Left err -> do
        "valid-from" ## I.textSplice err
        "valid-until" ## I.textSplice err
        "case-timing" ## return []
    Right interval -> do
        "valid-from" ##
          maybe (I.textSplice "N/A") timeExprSplice  (Interval.from interval)
        "valid-until" ##
          maybe (I.textSplice "N/A") timeExprSplice (Interval.until interval)
        "case-timing" ##
          timingSplice interval


{-
indexSplices :: TimeZone -> [(Page, [Submission])] -> ISplices
indexSplices tz pageSubs = do
  "index-list" ##
    I.mapSplices (I.runChildrenWith . auxSplices) pageSubs
  where
    auxSplices (page,subs) = pageSplices page >> submissionListSplices tz subs
-}



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


-- | splice for time expressions and intervals
timeExprSplice :: TimeExpr -> I.Splice Codex
timeExprSplice e = do
  tz <- liftIO getCurrentTimeZone
  env <- lift (require getUserEvents)
  case evalT env e of
    Right t -> I.textSplice (showTime tz t)
    Left err -> I.textSplice err


timingSplice :: Interval -> I.Splice Codex
timingSplice interval = do
  t <- liftIO getCurrentTime
  env <- lift (require getUserEvents)
  case Interval.evalI env interval t of
    Right v -> caseSplice v
    Left err -> I.textSplice err




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
  env <- require getUserEvents
  zt <- liftIO getZonedTime
  let now = zonedTimeToUTC zt
  let r = do interval <- fromMaybe (return Always) $ pageValid zt page
             Interval.evalI env interval now
  case r of
    Left msg -> error (T.unpack msg)
    Right timing -> do
      sub <- insertSubmission uid (pagePath page) now code evaluating timing
      evaluate sub
      return (submitID sub)

-- (re)evaluate a submission;
-- runs code tester in separate thread
evaluate :: Submission -> Codex ThreadId
evaluate sub = do
  let sid = submitID sub
  page <- liftIO $ readPage publicPath (submitPath sub)
  let code = submitCode sub
  conf <- getSnapletUserConfig
  sqlite <- S.getSqliteState
  liftIO $ forkIO $ do
    updateSubmission sqlite sid evaluating
    result <- codeTester conf page code `catch`
               (\ (e::SomeException) -> return (miscError $ T.pack $ show e))
    updateSubmission sqlite sid result

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
    return App { _heist = h
               , _sess = s
               , _auth = a
               , _db   = d
               -- , ekg = Nothing
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
