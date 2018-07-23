{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'codexInit' function is the initializer that combines everything
-- together and is exported by this module.
module Codex.Site
  ( codexInit
  ) where

------------------------------------------------------------------------------

import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Lens
import           Control.Monad.State
import           Control.Exception  (IOException)
import           Control.Exception.Lifted  (catch)

import           Data.ByteString.UTF8                        (ByteString)
import           Data.Map.Syntax

import qualified Data.Text                                   as T
import           Data.Maybe(fromMaybe)


import           Heist
import qualified Heist.Interpreted                           as I
import           Heist.Splices                               as I
import           Snap.Core hiding (path)
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.SqliteSimple
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import qualified Snap.Snaplet.SqliteSimple                   as S
import           Snap.Snaplet.Router
import           Snap.Util.FileServe                         (fileType,
                                                              serveDirectory,
                                                              serveFileAs)


import           Data.Time.Clock
import           Data.Time.LocalTime

import           System.Directory                            (doesFileExist)
import           System.FilePath

import           System.FastLogger                    (newLogger, stopLogger)

import qualified Data.Configurator as Conf
import           Data.Configurator.Types (Config)

------------------------------------------------------------------------------
import           Codex.AceEditor
import           Codex.AdminHandlers
import           Codex.Application
import           Codex.AuthHandlers
import           Codex.Config
import qualified Codex.Db           as  Db
import           Codex.Time
import           Codex.Page
import           Codex.Submission
import           Codex.Types
import           Codex.Utils
import           Codex.Evaluate
import           Codex.Tester
import           Codex.Tasks

import           Data.Version                                (showVersion)
import           Paths_codex                                 (version)


import           Text.Pandoc                                 hiding (Code)
import qualified Text.Pandoc as Pandoc 
import           Text.Pandoc.Walk                            as Pandoc



-- | handle page requests
handlePage :: FilePath -> Codex ()
handlePage rqpath = do
  uid <- require getUserLogin <|> unauthorized
  root <- getDocumentRoot
  let filepath = root </> rqpath
  c <- liftIO (doesFileExist filepath)
  unless c notFound
  let mime = fileType mimeTypes rqpath
  case mime of
    "text/markdown" -> (method GET (servePage uid rqpath) <|>
                        method POST (handlePost uid rqpath filepath))
    _ -> method GET (serveFileAs mime filepath)


-- | handle post request for an exercise submission
--
handlePost :: UserLogin -> FilePath -> FilePath -> Codex ()
handlePost uid rqpath filepath = do
  page <- liftIO (readMarkdownFile filepath)
  unless (pageIsExercise page) badRequest
  text <- require (getTextPost "code")
  lang <- Language <$> require (getTextPost "language")
  unless (lang `elem` pageLanguages page) badRequest
  sid <- newSubmission uid rqpath (Code lang text)
  redirectURL (Report sid)


-- | serve a markdown page
servePage :: UserLogin -> FilePath -> Codex ()
servePage uid rqpath = do
  page <- readPage uid rqpath
  if pageIsExercise page then
    renderExercise rqpath page =<< getPageSubmissions uid rqpath
    else
    renderWithSplices "_page" (pageUrlSplices rqpath >>
                               fileUrlSplices rqpath >>
                               pageSplices page)

--  render an exercise page
renderExercise :: FilePath -> Page -> [Submission] -> Codex ()
renderExercise rqpath page subs = do
  tz <- liftIO getCurrentTimeZone
  timeSplices <- timingSplices page
  renderWithSplices "_exercise" $ do
    pageUrlSplices rqpath
    fileUrlSplices rqpath
    pageSplices page
    exerciseSplices page
    timeSplices
    submissionListSplices tz subs
    textEditorSplice
    languageSplices (pageLanguages page) Nothing
    

-- render report for a single submission
renderReport :: FilePath -> Page -> Submission -> Codex ()
renderReport rqpath page sub = do
  tz <- liftIO getCurrentTimeZone
  timeSplices <- timingSplices page
  renderWithSplices "_report" $ do
    pageUrlSplices rqpath
    pageSplices page
    exerciseSplices page
    timeSplices
    submitSplices tz sub
    textEditorSplice
    languageSplices (pageLanguages page) (Just $ submitLang sub)


-- | read a page and patch exercise links
readPage :: UserLogin -> FilePath -> Codex Page
readPage uid rqpath = do
  root <- getDocumentRoot
  page <- liftIO $ readMarkdownFile (root </> rqpath)
  let rqdir = takeDirectory rqpath
  walkM (fetchLink uid root rqdir) page

-- | fetch title and submissions count for exercise links
fetchLink uid root rqdir
  elm@(Link attr@(_, classes,_) inlines target@(url,_))
  | "ex" `elem` classes = do
      title <- liftIO $ readPageTitle (root </> rqdir </> url)
      count <- countPageSubmissions uid (rqdir </> url)
      return (formatLink attr title target count)
fetchLink uid root rqdir elm
  = return elm
    
formatLink attr title target count 
  = Span nullAttr
    [Link attr title target,
     LineBreak,
      Span ("", ["info"], [])
      [Str "(",
        Str (show count), Space, Str "submissões",
        Str ")"]
    ]

readPageTitle :: FilePath -> IO [Inline]
readPageTitle path
  = fromMaybe [Pandoc.Code nullAttr path] <$> readTitle
  where
    readTitle = (pageTitle <$> readMarkdownFile path)
                `catch` (\(_ :: IOException) -> return Nothing)

{-
pageSubmissions :: UserLogin -> FilePath -> Codex Int
pageSubmissions uid path
  = length <$> getPageSubmissions uid path
-}

-- | handle GET requests for submission reports
handleReport :: SubmitId -> Codex ()
handleReport sid = do
  usr <- require (with auth currentUser) <|> unauthorized
  let uid = authUserLogin usr
  sub <- require (getSubmission sid) <|> notFound
  unless (isAdmin usr || submitUser sub == uid)
      unauthorized
  root <- getDocumentRoot
  let rqpath = submitPath sub
  method GET $ do
    page <- liftIO $ readMarkdownFile (root </> rqpath)
    renderReport rqpath page sub



-- | splices related to exercises
exerciseSplices :: Page -> ISplices
exerciseSplices page = do
  let fb = pageFeedback page
  "page-languages" ##
    I.textSplice $ T.intercalate "," $ map fromLanguage $ pageLanguages page
  "language-extensions" ##
   I.textSplice $ languageExtensions $ pageLanguages page
  "default-text" ## maybe (return []) I.textSplice (pageDefaultText page)
  "feedback-low" ## I.ifElseISplice (fb >= 25)
  "feedback-medium" ## I.ifElseISplice (fb >= 50)
  "feedback-high" ## I.ifElseISplice (fb >= 75)


-- | splices related to the submission interval for an exercise
timingSplices :: Page -> Codex ISplices
timingSplices page = do
  tz  <- liftIO getCurrentTimeZone
  now <- liftIO getCurrentTime
  events <- getEvents
  let optInt = evalInterval tz events (pageInterval page)
  case optInt of
    Left msg -> return (pure ())
    Right interval -> do
      let timeLeft = fmap (\t -> diffUTCTime t now) (higher interval)
      return $ do
        "valid-from" ##
          I.textSplice $ maybe "N/A" (showTime tz) (lower interval)
        "valid-until" ##
          I.textSplice $ maybe "N/A" (showTime tz) (higher interval)
        "current-timing" ##
          (caseSplice . timeInterval now) interval
        "time-left" ##
          I.textSplice $ maybe "N/A" (\t -> T.pack $ formatNominalDiffTime t) timeLeft
 


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



------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Codex ())]
routes =
  [ ("",        routeWith routeAppUrl)
  , ("/static", (getStaticRoot >>= serveDirectory) <|> notFound)
  ]


routeAppUrl :: AppUrl -> Codex ()
routeAppUrl appUrl =
  case appUrl of
    Login  -> handleLogin 
    Logout -> handleLogout
    Register -> handleRegister 
    Page path -> handlePage (joinPath path)
    Report sid -> handleReport sid 
    Files path -> handleBrowse (joinPath path)
    SubmissionAdmin sid -> handleSubmissionAdmin sid
    SubmissionList-> handleSubmissionList


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
newSubmission :: UserLogin -> FilePath -> Code -> Codex SubmitId
newSubmission uid rqpath code = do
  now <- liftIO getCurrentTime
  sub <- insertSubmission uid rqpath now code evaluating Valid
  evaluate sub
  return (submitId sub)



------------------------------------------------------------------------------
-- | The application initializer.
codexInit :: Tester Result -> SnapletInit App App
codexInit tester =
  makeSnaplet "codex" "Web server for programming exercises." Nothing $ do
    conf <- getSnapletUserConfig
    prefix <- liftIO $ Conf.require conf "url_prefix"
    h <- nestSnaplet "" heist $ heistInit "templates"
    r <- nestSnaplet "router" router (initRouter prefix)
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" Nothing (Just 36000)
    d <- nestSnaplet "db" db S.sqliteInit
    a <- nestSnaplet "auth" auth $ initSqliteAuth sess d
    addAuthSplices h auth
    js <- liftIO $ configSplices conf
    addConfig h (mempty & scInterpretedSplices .~ (staticSplices `mappend` js))
    -- auto-reload config file for events
    (evcfg, _) <- liftIO $ Conf.autoReload Conf.autoConfig [Conf.Required "events.cfg"]
    -- create a logger for user authentication
    logger <- liftIO $ newLogger "log/auth.log"
    onUnload (stopLogger logger)
    -- Grab the DB connection pool from the sqlite snaplet and call
    -- into the Model to create all the DB tables if necessary.
    let c = S.sqliteConn $ d ^# snapletValue
    liftIO $ withMVar c $ \conn -> Db.createTables conn
    addRoutes routes
    -- | semaphore for limiting concurrent evaluations
    ntasks <- liftIO $ Conf.require conf "system.max_concurrent"
    (semph, tasks) <- liftIO (makeTasks ntasks)
    return App { _heist = h
               , _router = r
               , _sess = s
               , _auth = a
               , _db   = d
               , _tester = tester
               , _tasks = tasks
               , _semph = semph
               , _logger  = logger
               , _eventcfg = evcfg
               }


configSplices :: Config -> IO ISplices
configSplices conf = do
  mathjax_js <- Conf.require conf "mathjax_js"
  ace_editor_js <- Conf.require conf "ace_editor_js"
  return $ do
    "mathjax-js" ## return (map javascriptSrc mathjax_js)
    "ace-editor-js" ## return (map javascriptSrc ace_editor_js)

staticSplices :: ISplices
staticSplices = do
  "login" ## urlSplice Login
  "logout" ## urlSplice Logout
  "register" ## urlSplice Register
  "home" ## urlSplice (Page ["index.md"])
  "files" ## urlSplice (Files [])
  "submissionList" ## urlSplice SubmissionList
  "version" ## versionSplice
  "timeNow" ## nowSplice
  "evaluating" ## return []
  "loggedInName" ## loggedInName auth
  "ifAdmin" ## do mbAu <- lift (withTop auth currentUser)
                  I.ifElseISplice (maybe False isAdmin mbAu)


