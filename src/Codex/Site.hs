{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}

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
import           System.Directory (doesFileExist, removeFile)
import           Data.Char (isAlphaNum)
import           Data.ByteString.UTF8                        (ByteString)
import qualified Data.ByteString.UTF8                        as B
import           Data.Map.Syntax

import           Data.Maybe (fromMaybe)

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
import           Codex.Application (Codex, App(..), AppUrl)
import qualified Codex.Application as App
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
           


-- | handle file requests
handlePage :: FilePath -> Codex ()
handlePage rqpath = do
  uid <- require getUserLogin <|> unauthorized
  method GET (handleGet uid rqpath <|> notFound) <|>
    method POST (handlePost uid rqpath <|> badRequest)


-- | handle GET requests
handleGet :: UserLogin -> FilePath -> Codex ()
handleGet uid rqpath = do
  let mime = fileType mimeTypes rqpath
  if mime == "text/markdown" then do
    page <- handleGetTranslation rqpath
    Handlers{handleView} <- gets _handlers
    withSplices (urlSplices rqpath) $ handleView uid rqpath page
  else -- check if we can serve the file
     do let ext = takeExtension rqpath
        guard (ext `elem` allowedExtensions)
        root <- getDocumentRoot
        serveFileAs mime (root </> rqpath)
     <|> unauthorized

allowedExtensions :: [FilePath]
allowedExtensions
  = [ ".txt", ".jpg", ".jpeg", ".png", ".svg",
      ".pdf", ".ps", ".zip",".tar", ".gz", ".tgz", ".bz2"
    ]


-- | try to load a markdown possibly with translation
handleGetTranslation :: FilePath -> Codex Page
handleGetTranslation rqpath = do
  root <- getDocumentRoot
  let originalFile = root </> rqpath
  guardFileExists originalFile
  optLang <-getUserTranslate
  readMarkdownFile originalFile optLang

  

    

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
  -- check that the user is still allowed to submit
  check <- checkTimeRemaining
  if check then do
    optLang <- fmap T.unpack <$> queryUserMeta uid "translate"
    page <- readMarkdownFile filepath optLang
    Handlers{handleSubmit} <- gets _handlers
    handleSubmit uid rqpath page
    else
    forbidden

  
-- | handle GET requests for submission reports
handleGetReport :: SubmitId -> Codex ()
handleGetReport sid = method GET $ do
  usr <- require (with App.auth currentUser) <|> unauthorized
  sub <- require (getSubmission sid) <|> notFound
  let uid = authUserLogin usr
  unless (isAdmin usr || submitUser sub == uid)
      unauthorized
  let rqpath = submitPath sub
  page <- handleGetTranslation rqpath
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
    App.Admin -> requireAdmin >> render "_admin"
    App.Login  -> handleLogin 
    App.Logout -> handleLogout
    App.Register -> handleRegister 
    App.Page path -> handlePage (joinPath path)
    App.Report sid -> handleGetReport sid 
    App.Files path -> handleBrowse (joinPath path)
    App.SubmissionAdmin sid -> handleSubmissionAdmin sid
    App.SubmissionList-> handleSubmissionList


-- | current logged in full user name
loggedInName :: SnapletLens b (AuthManager b) -> SnapletISplice b
loggedInName authmgr = do
    u <- lift $ withTop authmgr currentUser
    maybe (return []) I.textSplice (u >>= authFullname)


-- | splices for time
timeNowSplice :: I.Splice Codex
timeNowSplice = do
  tz<- liftIO getCurrentTimeZone
  t <- liftIO getCurrentTime
  localTimeSplice (utcToLocalTime tz t)

timeRemainSplice :: I.Splice Codex
timeRemainSplice = do
  opt <- lift getTimeRemaining
  return $ case opt of
    Nothing ->  []
    Just diff -> jsTimer "time-remain" diff
        

versionSplice :: I.Splice Codex
versionSplice = I.textSplice (T.pack (showVersion version))

------------------------------------------------------------------------------
-- | The application initializer.
codexInit :: Handlers Codex -> Tester Result -> SnapletInit App App
codexInit handlers tester =
  makeSnaplet "codex" "Web server for programming exercises." Nothing $ do
    conf <- getSnapletUserConfig
    prefix <- liftIO $ Conf.require conf "url_prefix"
    dpl <- liftIO $ translateSplices conf
    h <- nestSnaplet "" App.heist $ heistInit "templates"
    r <- nestSnaplet "router" App.router (initRouter prefix)
    let sessName = sessionName prefix
    -- remove old site any it exists
    liftIO removeOldSiteKey 
    s <- nestSnaplet sessName App.sess $
         initCookieSessionManager siteKeyPath sessName Nothing Nothing
    d <- nestSnaplet "db" App.db S.sqliteInit
    a <- nestSnaplet "auth" App.auth $ initSqliteAuth App.sess d
    addAuthSplices h App.auth
    js <- liftIO $ configSplices conf
    addConfig h (mempty & scInterpretedSplices .~ (staticSplices <> dpl <> js))
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

translateSplices :: Config -> IO ISplices
translateSplices conf = do
  defaultLang <- Conf.lookup conf "translations.defaultLanguage"
  langs <- Conf.require conf "translations.otherLanguages"
  let splice lang = "language" ## I.textSplice lang
  return $ do
    "translate-default-language" ##  I.textSplice (fromMaybe "default" defaultLang)
    "translate-other-languages" ## I.mapSplices (I.runChildrenWith . splice) langs


staticSplices :: ISplices
staticSplices = do
  "admin" ## urlSplice App.Admin
  "login" ## urlSplice App.Login
  "logout" ## urlSplice App.Logout
  "register" ## urlSplice App.Register
  "home" ## urlSplice (App.Page ["index.md"])
  "files" ## urlSplice (App.Files [])
  "submissionList" ## urlSplice App.SubmissionList
  "version" ## versionSplice
  "timeNow" ## timeNowSplice
  "timeRemaining" ## timeRemainSplice
  "if-evaluating" ## return []
  "ifLoggedIn" ## ifLoggedIn App.auth
  "ifLoggedOut" ## ifLoggedOut App.auth
  "loggedInName" ## loggedInName App.auth
  "ifAdmin" ## do mbAu <- lift (withTop App.auth currentUser)
                  I.ifElseISplice (maybe False isAdmin mbAu)
