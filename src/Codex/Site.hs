{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Codex.Site
  ( app
  ) where

------------------------------------------------------------------------------

import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Concurrent.QSem 
import           Control.Lens
import           Control.Monad.State
import           Control.Exception  (SomeException)
import           Control.Exception.Lifted  (catch)

import           Data.ByteString.UTF8                        (ByteString)
import qualified Data.HashMap.Strict                         as HM
import           Data.Map.Syntax

import qualified Data.Text                                   as T
import           Data.Maybe(isJust)

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

import qualified Data.Configurator as Conf

------------------------------------------------------------------------------
import           Codex.AceEditor
import           Codex.AdminHandlers
import           Codex.Application
import           Codex.AuthHandlers
import           Codex.Config
import qualified Codex.Db           as  Db
import           Codex.Interval
import           Codex.Page
import           Codex.Submission
import           Codex.Types
import           Codex.Utils
import           Codex.Evaluate
import           Codex.Tester.Result
-- import           Codex.Printout

import           Data.Version                                (showVersion)
import           Paths_codex                                 (version)


import           Text.Pandoc                                 hiding (Code)
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
        -- ensure the request is for an exercise page
  let handlePost = do
        page <- liftIO (readMarkdownFile filepath)
        unless (pageIsExercise page) badRequest
        text <- require (getTextPost "editform.editor")
        lang <- require (return $ pageLanguage page)
        sid <- newSubmission uid rqpath (Code lang text)
        redirectURL (Report sid)
    
  case mime of
    "text/markdown" -> (method GET (servePage uid rqpath) <|>
                        method POST handlePost)
    _ -> method GET (serveFileAs mime filepath)

{-
  where
   handleRedir root rqpath = do
        let filepath = root </> rqpath </> "index.md"
        c <- liftIO (doesFileExist filepath)
        if c then
          redirect (encodePath ("/pub" </> rqpath </> "index.md"))
          else
          notFound
    handlePost uid root rqpath = do
      let filepath = root </> rqpath
      c <- liftIO (doesFileExist filepath)
      guard (c && fileType mimeTypes rqpath == "text/markdown")
      page <- liftIO (readMarkdownFile $ root </> rqpath)
      -- ensure the request is for an exercise page
      guard (pageIsExercise page)
      text <- require (getTextPost "editform.editor")
      lang <- require (return $ pageLanguage page)
      sid <- newSubmission uid rqpath (Code lang text)
      redirectURL (Submit sid)
-}


-- | serve a markdown document
servePage :: UserLogin -> FilePath -> Codex ()
servePage uid rqpath = do
  page <- readPageLinks uid rqpath
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
    inputAceEditorSplices
    

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
    inputAceEditorSplices





-- | read a page, collect exercise links
--  and patch titles of linked pages
readPageLinks :: UserLogin -> FilePath -> Codex Page
readPageLinks uid rqpath = do
  root <- getDocumentRoot
  page <- liftIO $ readMarkdownFile (root </> rqpath)
  let links = queryExerciseLinks page
  let rqdir = takeDirectory rqpath
  -- fetch linked titles; catch and ignore exceptions
  optTitles <- liftIO $
               forM links $ \url ->
               (pageTitle <$> readMarkdownFile (root </> rqdir </> url))
               `catch` (\(_ :: SomeException) -> return Nothing)

  let titles = HM.fromList [(url,title) | (url, Just title)<-zip links optTitles]
  -- fetch submisssion count
  submissions <- HM.fromList <$>
                 forM links ( \url -> do
                     let ex = normalise (rqdir </> url)
                     count <- length <$> getPageSubmissions uid ex
                     return (url, count))
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
  return (walk patch page)




-- | collect all exercise links from a page
queryExerciseLinks :: Page -> [String]
queryExerciseLinks page = query extractURL (pageDescription page)
  where
    extractURL (Link (_,classes,_) _ (url,_)) = [url | "ex" `elem` classes]
    extractURL _                              = []



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
  let fb = submitFeedback page
  "language" ##
    maybe (return []) (I.textSplice . fromLanguage) (pageLanguage page)
  "language-mode" ##
    maybe (return []) (I.textSplice . languageMode) (pageLanguage page)
  "code-text" ##
    maybe (return []) I.textSplice (pageCodeText page)
  "feedback-low" ## I.ifElseISplice (fb >= 25)
  "feedback-medium" ## I.ifElseISplice (fb >= 50)
  "feedback-high" ## I.ifElseISplice (fb >= 75)


-- | splices related to the submission interval for an exercise
timingSplices :: Page -> Codex ISplices
timingSplices page = do
  tz  <- liftIO getCurrentTimeZone
  now <- liftIO getCurrentTime
  events <- getEvents
  let interval = evalI tz events (submitInterval page)
  unless (isJust interval) $ 
    logError ("WARNING: invalid submission interval")
  return $ do
    "valid-from" ##
      I.textSplice $ maybe "N/A" (showTime tz) (lower =<< interval)
    "valid-until" ##
      I.textSplice $ maybe "N/A" (showTime tz) (higher =<< interval)
    "current-timing" ##
      maybe (return []) (caseSplice . rankTime now) interval
 


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

{-
routes = [ ("/login",    handleLogin `catch` internalError)
         , ("/logout",   handleLogout `catch` internalError)
         , ("/register", handleRegister `catch` internalError)
         , ("/pub",      handlePage `catch` internalError)
         , ("/sub/:sid", handleSubmitReport `catch` internalError)
         , ("/submissions/:sid", handleSubmission `catch` internalError)
         , ("/submissions",  handleSubmissionList `catch` internalError)
         , ("/files",  handleBrowse `catch`  internalError)
         , ("/export", handleExport `catch` internalError)
         , ("/printouts", handlePrintouts `catch` internalError)
         , ("/static",  (getStaticRoot >>= serveDirectory) <|> notFound)
         , ("/robots.txt", getStaticRoot >>= \dir -> (serveFile $ dir</>"robots.txt"))
         ]
-}

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
app :: SnapletInit App App
app =
  makeSnaplet "codex" "Web server for programming exercises." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    r <- nestSnaplet "router" router $ initRouter ""
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" Nothing (Just 36000)
    d <- nestSnaplet "db" db S.sqliteInit
    a <- nestSnaplet "auth" auth $ initSqliteAuth sess d
    addAuthSplices h auth
    addConfig h (mempty & scInterpretedSplices .~ staticSplices)
    -- Grab the DB connection pool from the sqlite snaplet and call
    -- into the Model to create all the DB tables if necessary.
    let c = S.sqliteConn $ d ^# snapletValue
    liftIO $ withMVar c $ \conn -> Db.createTables conn
    addRoutes routes
    -- create a semaphore for throttling concurrent evaluation threads
    conf <- getSnapletUserConfig
    tids <- liftIO $ newMVar []
    qs <- liftIO $ newQSem =<< Conf.require conf "system.workers"
    return App { _heist = h
               , _router = r
               , _sess = s
               , _auth = a
               , _db   = d
               , evthids = tids
               , evqs    = qs
               }


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
