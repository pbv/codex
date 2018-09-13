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
import           Codex.Quiz
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

           

-- | handle file requests
handlePage :: FilePath -> Codex ()
handlePage rqpath = do
  uid <- require getUserLogin <|> unauthorized
  method GET (handleGet uid rqpath <|> notFound) <|>
    method POST (handlePost uid rqpath <|> badRequest)

-- | handle GET requests for pages and files
handleGet uid rqpath = do
  root <- getDocumentRoot
  let filepath = root </> rqpath
  guardFileExists filepath
  let rqdir = takeDirectory rqpath
  let mime = fileType mimeTypes rqpath
  if mime == "text/markdown" then do
    page <- readMarkdownFile filepath >>= fillExerciseLinks uid rqdir
    withSplices (urlSplices rqpath)
      (handleGetQuiz uid rqpath page <|>
       handleGetCode uid rqpath page <|>
       handleGetMarkdown page)
    else
    serveFileAs mime filepath
  
-- | get a multiple choice quiz
handleGetQuiz :: UserLogin -> FilePath -> Page -> Codex ()
handleGetQuiz uid rqpath page = do
  guard (isQuiz page)
  let quiz = shuffleQuiz uid page
  subs <- getPageSubmissions uid rqpath
  let answers = fromMaybe emptyAnswers $ case subs of
                  [] -> Nothing
                  _ ->  getSubmittedAnswers (last subs)
  renderWithSplices "_quiz" (quizSplices quiz answers)


-- | get a coding exercise 
handleGetCode :: UserLogin -> FilePath -> Page -> Codex ()
handleGetCode uid rqpath page = do
  guard (isExercise page)
  tz <- liftIO getCurrentTimeZone
  subs <- getPageSubmissions uid rqpath
  withTimeSplices page $ renderWithSplices "_exercise" $ do
    pageSplices page
    codeSplices page
    feedbackSplices page
    submissionListSplices tz subs
    textEditorSplice
    languageSplices (pageLanguages page) Nothing


handleGetMarkdown :: Page -> Codex ()
handleGetMarkdown page = renderWithSplices "_page" (pageSplices page)
  


-- | handle POST requests for exercise pages
handlePost :: UserLogin -> FilePath -> Codex ()
handlePost uid rqpath = do
  root <- getDocumentRoot
  let filepath = root </> rqpath
  guardFileExists filepath
  let mime = fileType mimeTypes rqpath
  guard (mime == "text/markdown") 
  page <- readMarkdownFile filepath
  handlePostQuiz uid rqpath page <|> handlePostCode uid rqpath page

handlePostQuiz :: UserLogin -> FilePath -> Page -> Codex ()
handlePostQuiz uid rqpath page = do
  guard (isQuiz page)
  ans <- getFormAnswers
  let text = encodeAnswers ans
  sid <- newSubmission uid rqpath (Code "json" text)
  redirectURL (Report sid)
  
handlePostCode :: UserLogin -> FilePath -> Page -> Codex ()
handlePostCode uid rqpath page = do
  guard (isExercise page)
  text <- require (getTextParam "code")
  lang <- Language <$> require (getTextParam "language")
  guard (lang `elem` pageLanguages page) 
  sid <- newSubmission uid rqpath (Code lang text)
  redirectURL (Report sid)

 

-- | handle GET requests for previous submission 
handleReport :: SubmitId -> Codex ()
handleReport sid = method GET $ do
  usr <- require (with auth currentUser) <|> unauthorized
  let uid = authUserLogin usr
  sub <- require (getSubmission sid) <|> notFound
  unless (isAdmin usr || submitUser sub == uid)
      unauthorized
  root <- getDocumentRoot
  let rqpath = submitPath sub
  page <- readMarkdownFile (root </> rqpath)
  if isQuiz page then
    handleQuizReport rqpath page sub
    else
    handleCodeReport rqpath page sub


-- | report a code submission
handleCodeReport :: FilePath -> Page -> Submission -> Codex ()
handleCodeReport rqpath page sub = do
  tz <- liftIO getCurrentTimeZone   
  withTimeSplices page $ renderWithSplices "_report" $ do
    urlSplices rqpath
    pageSplices page
    codeSplices page
    feedbackSplices page
    submitSplices tz sub
    textEditorSplice
    languageSplices (pageLanguages page) (Just $ submitLang sub)

-- | report a quiz submission
handleQuizReport :: FilePath -> Page -> Submission -> Codex ()
handleQuizReport rqpath page sub = do
  tz <- liftIO getCurrentTimeZone
  let quiz = shuffleQuiz (submitUser sub) page
  let answers = fromMaybe emptyAnswers $ getSubmittedAnswers sub
  renderWithSplices "_answers" $ do
    urlSplices rqpath
    pageSplices page
    feedbackSplices page
    submitSplices tz sub
    quizSplices quiz answers


getSubmittedAnswers :: Submission -> Maybe Answers
getSubmittedAnswers = decodeAnswers . codeText . submitCode


-- | fill titles and submission counts on exercise links
fillExerciseLinks :: UserLogin -> FilePath -> Page -> Codex Page
fillExerciseLinks uid rqdir page = do
  root <- getDocumentRoot
  walkM (fetchLink uid root rqdir) page
 

-- | fetch title and submissions count for exercise links
fetchLink uid root rqdir
  elm@(Link attr@(_, classes,_) inlines target@(url,_))
  | "ex" `elem` classes = do
      let path = normalise (rqdir </> url)
      title <- liftIO $ readPageTitle (root </> path)
      count <- countPageSubmissions uid path
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




-- | splices related to code exercises
codeSplices :: Page -> ISplices
codeSplices page = do
  "page-languages" ##
    I.textSplice $ T.intercalate "," $ map fromLanguage $ pageLanguages page
  "language-extensions" ##
   I.textSplice $ languageExtensions $ pageLanguages page
  "default-text" ## maybe (return []) I.textSplice (pageDefaultText page)


feedbackSplices :: Page -> ISplices
feedbackSplices page = do
  "if-feedback" ## I.ifElseISplice (pageFeedback page)
  


-- | splices related to the submission interval for an exercise
timingSplices :: TimeZone -> UTCTime -> Interval UTCTime -> ISplices
timingSplices tz now interval = do
  let timeLeft = fmap (\t -> diffUTCTime t now) (higher interval)
  "valid-from" ##
    I.textSplice $ maybe "N/A" (showTime tz) (lower interval)
  "valid-until" ##
    I.textSplice $ maybe "N/A" (showTime tz) (higher interval)
  "current-timing" ##
    (caseSplice . timeInterval now) interval
  "time-left" ##
    I.textSplice $ maybe "N/A" (\t -> T.pack $ formatNominalDiffTime t) timeLeft

withTimeSplices :: Page -> Codex a -> Codex a
withTimeSplices page action = do
  tz  <- liftIO getCurrentTimeZone
  now <- liftIO getCurrentTime
  events <- getEvents
  let optInt = evalInterval tz events (pageInterval page)
  let splices = case optInt of
        Left err -> do
          "current-timing" ## I.textSplice (T.pack err)
        Right interval -> timingSplices tz now interval
  withSplices splices action


-- | splices relating to a list of submissions
submissionListSplices :: TimeZone -> [Submission] -> ISplices
submissionListSplices tz list = do
  let count = length list
  "submissions-count" ## I.textSplice (T.pack $ show count)
  "if-submitted" ## I.ifElseISplice (count > 0)
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
  "if-evaluating" ## return []
  "loggedInName" ## loggedInName auth
  "ifAdmin" ## do mbAu <- lift (withTop auth currentUser)
                  I.ifElseISplice (maybe False isAdmin mbAu)


