{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

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
import           System.Directory (doesFileExist,removeFile)

import           Data.Char (isAlphaNum)
import           Data.ByteString.UTF8                        (ByteString)
import qualified Data.ByteString.UTF8                        as B
import           Data.Map.Syntax

import qualified Data.Text                                   as T

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
import           Codex.AdminHandlers
import           Codex.Application
import           Codex.AuthHandlers
import           Codex.Config
import qualified Codex.Db           as  Db
import           Codex.Page
import           Codex.Handlers
import           Codex.Submission
import           Codex.Types
import           Codex.Utils
import           Codex.Tester
import           Codex.Tasks
import           Data.Version                                (showVersion)
import           Paths_codex                                 (version)

-- | PI Improvements +++++++
import          Codex.Translate (translateMarkdown)
           

-- | handle file requests
handlePage :: FilePath -> Codex ()
handlePage rqpath = do
  uid <- require getUserLogin <|> unauthorized
  method GET (handleGet uid rqpath <|> notFound) <|>
    method POST (handlePost uid rqpath <|> badRequest)

-- | handle GET requests
--handleGet uid rqpath = do
--  root <- getDocumentRoot
--  let filepath = root </> rqpath
--  guardFileExists filepath
--  let mime = fileType mimeTypes filepath
--  if mime == "text/markdown" then do
--    page <- readMarkdownFile filepath
--    Handlers{handleView} <- gets _handlers
--    withSplices (urlSplices rqpath) $ handleView uid rqpath page
--    else
--    -- serve the file if it is not markdown 
--    serveFileAs mime filepath

-- ++++++++++++++++++++++++++++++++++++++++++++++++
-- | PI Improvements                              
-- ++++++++++++++++++++++++++++++++++++++++++++++++
handleGet :: UserLogin -> FilePath -> Codex ()
handleGet uid rqpath = do
  root <- getDocumentRoot
  langParam <- getParam "lang"  -- Get the language from the URL (?lang=pt)
  
  case langParam of
    Nothing -> do  -- If there is no ?lang=XX in the URL, display the original
      let originalFile = root </> rqpath
      guardFileExists originalFile
      serveMarkdown uid originalFile rqpath

    Just langBs -> do  -- If there is ?lang=XX, translate
      let lang = B.toString langBs
      let translatedFile = root </> addLanguage rqpath lang 

      fileExists <- liftIO $ doesFileExist translatedFile

      if fileExists 
        then serveMarkdown uid translatedFile rqpath  -- If the translated file already exists
        else do
          let originalFile = root </> rqpath
          guardFileExists originalFile
          page <- liftIO $ readMarkdownFile originalFile
          
          translatedPage <- translateMarkdown page (mapLangCode lang)

          case translatedPage of
            Just translated -> do
              liftIO $ writeMarkdownFile translatedFile translated
              serveMarkdown uid translatedFile rqpath
            Nothing -> serveMarkdown uid originalFile rqpath  -- If translation fails, display the original

serveMarkdown :: UserLogin -> FilePath -> FilePath -> Codex ()
serveMarkdown uid filepath rqpath = do
  let mime = fileType mimeTypes filepath
  if mime == "text/markdown" then do
    page <- readMarkdownFile filepath
    Handlers{handleView} <- gets _handlers
    withSplices (urlSplices rqpath) $ handleView uid rqpath page
  else
    -- serve the file if it is not markdown 
    serveFileAs mime filepath

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- | handle POST requests
handlePost :: UserLogin -> FilePath -> Codex ()
handlePost uid rqpath = do
  root <- getDocumentRoot
  let filepath = root </> rqpath
  -- check file exists and is markdown
  guardFileExists filepath
  let mime = fileType mimeTypes rqpath
  guard (mime == "text/markdown") 
  page <- readMarkdownFile filepath
  Handlers{handleSubmit} <- gets _handlers
  handleSubmit uid rqpath page

  
-- | handle GET requests for submission reports
handleGetReport :: SubmitId -> Codex ()
handleGetReport sid = method GET $ do
  usr <- require (with auth currentUser) <|> unauthorized
  sub <- require (getSubmission sid) <|> notFound
  let uid = authUserLogin usr
  unless (isAdmin usr || submitUser sub == uid)
      unauthorized
  root <- getDocumentRoot
  let rqpath = submitPath sub
  page <- readMarkdownFile (root </> rqpath)
  Handlers{handleReport} <- gets _handlers
  handleReport uid rqpath page sub





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
    Admin -> render "_admin"
    Login  -> handleLogin 
    Logout -> handleLogout
    Register -> handleRegister 
    Page path -> handlePage (joinPath path)
    Report sid -> handleGetReport sid 
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
nowSplice = do tz<- liftIO getCurrentTimeZone
               t <- liftIO getCurrentTime
               localTimeSplice tz t

versionSplice :: I.Splice Codex
versionSplice = I.textSplice (T.pack (showVersion version))

------------------------------------------------------------------------------
-- | The application initializer.
codexInit :: Handlers Codex -> Tester Result -> SnapletInit App App
codexInit handlers tester =
  makeSnaplet "codex" "Web server for programming exercises." Nothing $ do
    conf <- getSnapletUserConfig
    prefix <- liftIO $ Conf.require conf "url_prefix"
    h <- nestSnaplet "" heist $ heistInit "templates"
    r <- nestSnaplet "router" router (initRouter prefix)
    let sessName = sessionName prefix
    -- remove old site any it exists
    liftIO removeOldSiteKey 
    s <- nestSnaplet sessName sess $
         initCookieSessionManager siteKeyPath sessName Nothing Nothing
    d <- nestSnaplet "db" db S.sqliteInit
    a <- nestSnaplet "auth" auth $ initSqliteAuth sess d
    addAuthSplices h auth
    js <- liftIO $ configSplices conf
    addConfig h (mempty & scInterpretedSplices .~ (staticSplices <> js))
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
    -- | semaphore for limimit concurrent evaluations
    nslots <- liftIO $ Conf.require conf "system.max_concurrent"
    taskGroup <- createTaskGroup nslots
    queue <- createQueue
    return App { _heist = h
               , _router = r
               , _sess = s
               , _auth = a
               , _db   = d
               , _tester = tester
               , _handlers = handlers
               , _queue = queue
               , _taskGroup = taskGroup
               , _logger  = logger
               , _eventcfg = evcfg
               }

removeOldSiteKey :: IO ()
removeOldSiteKey = 
  whenM (doesFileExist siteKeyPath) $ removeFile siteKeyPath

siteKeyPath :: String
siteKeyPath = "site_key.txt"

-- | cookie name for each session is a function of the request path prefix
sessionName :: Text -> ByteString
sessionName prefix
  = "sess_" <> (B.fromString $ T.unpack $ T.filter isAlphaNum prefix)


configSplices :: Config -> IO ISplices
configSplices conf = do
  mathjax_js <- Conf.require conf "mathjax_js"
  ace_editor_js <- Conf.require conf "ace_editor_js"
  return $ do
    "mathjax-js" ## return (map javascriptSrc mathjax_js)
    "ace-editor-js" ## return (map javascriptSrc ace_editor_js)

staticSplices :: ISplices
staticSplices = do
  "admin" ## urlSplice Admin
  "login" ## urlSplice Login
  "logout" ## urlSplice Logout
  "register" ## urlSplice Register
  "home" ## urlSplice (Page ["index.md"])
  "files" ## urlSplice (Files [])
  "submissionList" ## urlSplice SubmissionList
  "version" ## versionSplice
  "timeNow" ## nowSplice
  "if-evaluating" ## return []
  "ifLoggedIn" ## ifLoggedIn auth
  "ifLoggedOut" ## ifLoggedOut auth
  "loggedInName" ## loggedInName auth
  "ifAdmin" ## do mbAu <- lift (withTop auth currentUser)
                  I.ifElseISplice (maybe False isAdmin mbAu)


