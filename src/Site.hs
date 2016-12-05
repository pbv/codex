{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------

import           Control.Exception.Lifted (catch)
import           Control.Monad.State
import           Control.Monad.Trans.Except
import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Lens

-- import           Data.Char (toLower)
import           Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as B
-- import qualified Data.ByteString      as B
-- import           Data.Monoid
import           Data.Maybe(isJust)

import           Data.List (nub, sort)
import           Data.Map.Syntax
import qualified Data.HashMap.Strict as HM

import           Data.Text (Text)
import qualified Data.Text as T
-- import qualified Data.Text.IO as T
import           Data.Text.Encoding (decodeUtf8)

import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.SqliteSimple
import           Snap.Snaplet.SqliteSimple
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe (getSafePath, fileType, serveFileAs, serveDirectory)
import           Heist
import           Heist.Splices     as I
import qualified Heist.Interpreted as I

import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.Format

-- import qualified Data.Configurator as Configurator
-- import           Data.Configurator.Types (Config)

import           Data.Aeson

--import           System.Locale
import           System.FilePath
import           System.Directory (doesFileExist, doesDirectoryExist)
-- import           System.IO.Error
-- import           System.Remote.Monitoring
-- import qualified Text.XmlHtml as X


------------------------------------------------------------------------------
import           Application
import           Db
import           Utils
import           Types
import           Tester
import           Language
import           Markdown
import           Interval
-- import           Text.Pandoc.Builder hiding (Code)
-- import qualified Interval as Interval
-- import           Interval (Interval)
import           Page  (Page(..))
import qualified Page
import           Submission (Submission(..))
import qualified Submission
import           LdapAuth
import           Config

import           Admin

-- import           Report
-- import           Printout

import           Paths_codex(version)
import           Data.Version (showVersion)  

import           AceEditor

-- interpreted splices for Pythondo handlers
type ISplices = Splices (I.Splice Codex)



------------------------------------------------------------------------------
-- | Handle login requests 
handleLogin :: Codex ()
handleLogin =
  method GET (handleLoginForm "login" Nothing) <|>
  method POST handleLoginSubmit


-- | Render login and signup forms
handleLoginForm ::  ByteString -> Maybe AuthFailure -> Codex ()
handleLoginForm file authFail = heistLocal (I.bindSplices errs) $ render file
  where
    errs = "loginError" ## maybe (return []) (I.textSplice . T.pack .show) authFail

getLdap :: Codex (Maybe LdapConf)
getLdap = getSnapletUserConfig >>= (liftIO . getLdapConf "users.ldap")


-- | handler for login attempts
-- first try local user DB, then LDAP (if enabled)
handleLoginSubmit :: Codex ()
handleLoginSubmit = do
  login <- require (getParam "login")
  passwd <- require (getParam "password") 
  ldap <- getLdap
  r <- with auth $ loginByUsername (decodeUtf8 login) (ClearText passwd) False
  case r of
    Right au -> redirect "/page/index.md"
    Left err -> case ldap of
      Nothing -> handleLoginForm "login" (Just err)
      Just cfg -> loginLdapUser cfg login passwd

loginLdapUser :: LdapConf -> ByteString -> ByteString -> Codex ()
loginLdapUser ldapConf login passwd = do
  reply <- with auth $
           withBackend (\r -> liftIO $ ldapAuth r ldapConf login passwd)
  case reply of
    Left err -> handleLoginForm "login" (Just err)
    Right au -> with auth (forceLogin au) >> redirect "/page/index.md"
  
        

------------------------------------------------------------------------------

-- | Handle sign-in requests
handleRegister :: Codex ()
handleRegister =
  method GET (handleLoginForm "register" Nothing) <|>
  method POST handleSubmit
  where
    handleSubmit = do
      r <- with auth newUser
      case r of
        Left err -> handleLoginForm "register" (Just err)
        Right au -> with auth (forceLogin au) >> redirect "/page/index.md"


------------------------------------------------------------------------------
-- | Register a new user 
--
newUser :: Handler b (AuthManager b) (Either AuthFailure AuthUser)
newUser = do
    l <- fmap decodeUtf8 <$> getParam "login"
    p <- getParam "password"
    p2<- getParam "password2"
    n <- fmap decodeUtf8 <$> getParam "fullname"
    e <- fmap decodeUtf8 <$> getParam "email"
    runExceptT $ do
      login  <- maybe (throwE UsernameMissing) return l
      passwd <- maybe (throwE PasswordMissing) return p
      passwd2<- maybe (throwE PasswordMissing) return p2
      name <- maybe (throwE (AuthError "Missing full user name")) return n
      email <- maybe (throwE (AuthError "Missing user email")) return e
      when (passwd /= passwd2) $
        throwE (AuthError "Passwords fields do not match")
      au <- ExceptT (createUser login passwd)
      let meta' = HM.fromList [("fullname", String name)]
      let au' = au { userEmail = Just email, userMeta = meta'}
      ExceptT (saveUser au') `catchE` (\err -> case err of
                                          DuplicateLogin -> return au'
                                          err -> throwE err)
        



------------------------------------------------------------------------------
-- Logs out and redirects the user to the site index.
-- in exam mode procedeed to printout 
handleLogout :: Codex ()
handleLogout = method GET $ do
  uid <- require getUserID <|> unauthorized
  with auth logout 
  redirect "/" 
  {-
   -- handle printout if needed
   ProblemSet{..} <- fst <$> getProblemSet
   when probsetPrintout $ handlePrintout uid probsetProbs
   -}

{-
-- make a printout before ending session
handlePrintout :: UID -> [Problem] -> Codex ()
handlePrintout uid probs = do
  subs <- mapM (getBestSubmission uid) (map probID probs)
  report <- genReport (zip probs subs)
  liftIO $ makePrintout (show uid) report
-}


-- handle page requests
handlePage :: Codex ()
handlePage = do
  uid <- require getUserID <|> unauthorized
  rqpath <- getSafePath
  (method GET (handleGet uid rqpath) <|>
   method POST (handlePost uid rqpath <|> badRequest))
  where
    handleGet uid rqpath =  do
      let filepath = pageFilePath </> rqpath
      isdir <- liftIO (doesDirectoryExist filepath)
      when isdir $
        redirect (B.fromString ("/page" </> rqpath </> "index.md"))
      isfile <- liftIO (doesFileExist filepath)
      when (not isfile) $ notFound
      -- serve according to mime type
      let mime = fileType mimeTypes rqpath
      if mime == "text/markdown" then
        renderPage uid =<< liftIO (Page.readPage pageFilePath rqpath)
        else
        serveFileAs mime filepath
    -- handle page POST requests;
    -- only for exercise pages
    handlePost uid rqpath = do
      let filepath = pageFilePath </> rqpath
      isfile <- liftIO (doesFileExist filepath)
      guard (isfile && fileType mimeTypes rqpath == "text/markdown")
      page <- liftIO (Page.readPage pageFilePath rqpath)
      guard (Page.isExercise page)
      text <- require (getTextPost "editform.editor")
      lang <- require (return $ Page.getLanguage page)
      sub <- Submission.evaluate uid page (Code lang text)
      renderWithSplices "report" $ do 
        pageSplices page
        exerciseSplices page
        submitSplices sub 
        inputAceEditorSplices
    -- 
    renderPage uid page 
      | Page.isExercise page = 
        renderExercise page =<< Submission.getAll uid (Page.path page) 
      | Just links <- Page.getLinks page = 
          renderIndex uid page links
      | otherwise =
        renderWithSplices "page" (pageSplices page)


renderIndex uid page links = do
  querytags <- getQueryTags
  pages <- liftIO $ mapM (Page.readPage pageFilePath) links
  let alltags = nub $ sort $ concatMap Page.getTags pages
  let pages' = filter (\p -> querytags `contained` Page.getTags p) pages
  let abletags = nub $ sort $ concatMap Page.getTags pages'
  subs <- mapM (Submission.getAll uid . Page.path) pages'
  renderWithSplices "indexsheet" $ do
    pageSplices page 
    indexSplices (zip pages' subs)
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


--  render an exercise page
renderExercise :: Page -> [Submission] -> Codex ()
renderExercise page subs = do
  renderWithSplices "exercise" $ do
    pageSplices page
    exerciseSplices page
    submissionListSplices subs 
    inputAceEditorSplices


-- handle GET requests for previous submissions
handleSubmission = method GET $ do
  uid <- require getUserID <|> unauthorized
  sid <- require getSubmitID
  sub <- Submission.getSingle uid sid
  page <- liftIO $ Page.readPage pageFilePath (Submission.path sub)
  renderWithSplices "report" $ do
    pageSplices page
    exerciseSplices page
    submitSplices sub 
    inputAceEditorSplices



-- | splices related to a page
pageSplices :: Page -> ISplices
pageSplices page = do
  "file-path" ## I.textSplice (T.pack $ Page.path page)
  "file-dir"  ## I.textSplice (T.pack $ takeDirectory $ Page.path page)
  "page-title" ##
    I.textSplice (Page.getTitle page)
  "page-description" ##
    return (blocksToHtml $ Page.description page)
  "if-exercise" ##
    I.ifElseISplice (Page.isExercise page)
  "if-indexsheet" ##
    I.ifElseISplice (isJust $ Page.getLinks page)


-- | splices related to exercises
exerciseSplices :: Page -> ISplices
exerciseSplices page = do
  -- splices related to programming languages
  "language" ##
    maybe (return []) (I.textSplice . fromLanguage) (Page.getLanguage page)
  "language-mode" ##
    maybe (return []) (I.textSplice . languageMode) (Page.getLanguage page)
  "code-text" ##
    maybe (return []) I.textSplice (Page.getCodeText page)
  --- splices related to submission intervals 
  let interval = Page.validInterval page
  "valid-from" ##
    maybe (I.textSplice "N/A") timeExprSplice  (Interval.from interval)
  "valid-until" ##
    maybe (I.textSplice "N/A") timeExprSplice (Interval.until interval)
  "timing" ##
    timingSplice interval



indexSplices :: [(Page, [Submission])] -> ISplices
indexSplices pageSubs = do
  "index-list" ##
    I.mapSplices (I.runChildrenWith . auxSplices) pageSubs
  where
    auxSplices (page,subs) = pageSplices page >> submissionListSplices subs


-- | splices relating to a single submission
submitSplices :: Submission -> ISplices
submitSplices Submission{..} = do
  "submit-id" ##  I.textSplice (toText id)
  "submit-path" ## I.textSplice (T.pack path)
  "time" ## do {tz <- liftIO getCurrentTimeZone; utcTimeSplice tz time}
  "code-text" ##  I.textSplice (codeText code)
  "classify" ##  I.textSplice (T.pack $ show $ resultClassify result)
  "message" ## I.textSplice (resultMessage result)
  "timing" ## caseSplice timing
  "valid" ## I.ifElseISplice (timing == Valid)
  "early" ## I.ifElseISplice (timing == Early)
  "overdue" ## I.ifElseISplice (timing == Overdue)
  "accepted" ## I.ifElseISplice (resultClassify result == Accepted)



-- | splices relating to a list of submissions
submissionListSplices :: [Submission] -> ISplices
submissionListSplices list = do
  let count = length list
  "submissions-count" ##
    I.textSplice (T.pack $ show count)
  "if-submitted" ##
    I.ifElseISplice (count > 0) 
  "submissions-list" ##
    I.mapSplices (I.runChildrenWith . submitSplices) list


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





{-
warningsSplices :: [Text] -> ISplices
warningsSplices mesgs = do
  "warnings" ## I.mapSplices (I.runChildrenWith . mesgSplice) mesgs
  where mesgSplice msg = "message" ## I.textSplice msg
-}


{-
-- | splices related to deadlines
timerSplices ::  ProblemID -> UTCTime -> Maybe UTCTime -> ISplices
timerSplices pid now limit = do
  "ifOpen" ## conditionalSplice (maybe True (now<=) limit)
  "ifClosed"  ## conditionalSplice (maybe False (now>) limit)
  "ifTimed"  ## conditionalSplice (isJust limit)
  "endTime" ## maybe (return []) timeSplice limit
  "remainingTime" ## case limit of 
                   Nothing -> return []
                   Just t -> I.textSplice $ T.pack $
                             formatNominalDiffTime $ diffUTCTime t now
  "remainingJsTimer" ## case limit of 
                            Nothing -> return []
                            Just t -> return $ jsTimer tid $ diffUTCTime t now
  where tid = B.toString (fromPID pid) ++ "-js-timer"
-}
  


{-
-- in exam mode show final report before loggin out
-- otherwise, logout immediately
handleConfirmLogout :: Codex ()
handleConfirmLogout = method GET $ do
  uid <- require getUserID <|> badRequest
  (probset,_) <- getProblemSet
  handleFinalReport uid probset


handleFinalReport :: UID -> ProblemSet -> Codex ()
handleFinalReport uid ProblemSet{..} | probsetExam = do
    let pids = map probID probsetProbs
    subs <- mapM (getBestSubmission uid) pids
    let psubs = [(p,s) | (p, Just s)<-zip probsetProbs subs]
    renderWithSplices "finalrep" $
      "problemList" ## I.mapSplices (I.runChildrenWith . splices) psubs
  where splices (prob,sub) = problemSplices prob >> submissionSplices sub

-- not in exam mode: proceed to logout immediately
handleFinalReport _ _ = handleLogout
-}


  

------------------------------------------------------------------------------
-- | The application's routes.

routes :: [(ByteString, Codex ())]
routes = [ ("/login",    handleLogin `catch` internalError)
         , ("/logout",   handleLogout `catch` internalError)
         , ("/register", handleRegister `catch` internalError)
         , ("/page",     handlePage `catch` internalError)
         , ("/submit/:sid", handleSubmission `catch` internalError)
         , ("/browse",    handleListing pageFilePath  `catch` internalError)
         , ("/edit",     handleEdit  pageFilePath `catch` internalError)
         , ("/static",   serveDirectory staticFilePath <|> notFound) 
         ]



-- | current logged in full user name  
loggedInName :: SnapletLens b (AuthManager b) -> SnapletISplice b
loggedInName authmgr = do
    u <- lift $ withTop authmgr currentUser
    maybe (return []) I.textSplice (u >>= authFullname)

{-
authRoles :: SnapletLens b (AuthManager b) ->
             ([Role] -> Bool) -> SnapletISplice b
authRoles authmgr cond = do
   u <- lift $ withTop authmgr currentUser
   maybe (return []) (\u -> I.ifElseISplice (cond $ userRoles u)) u
-}

-- | splice for current date & time
nowSplice :: I.Splice Codex
nowSplice = do tz <- liftIO getCurrentTimeZone
               t <- liftIO getCurrentTime
               utcTimeSplice tz t



versionSplice :: I.Splice Codex
versionSplice = I.textSplice (T.pack (showVersion version))

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = 
  makeSnaplet "codex" "Web server for programming exercises." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" Nothing (Just 3600)

    d <- nestSnaplet "db" db sqliteInit
    a <- nestSnaplet "auth" auth $ initSqliteAuth sess d
    addAuthSplices h auth
    let sc = do 
          "version" ## versionSplice
          "timeNow" ## nowSplice
          "loggedInName" ## loggedInName auth
          "ifAdmin" ## do
            mbAu <- lift (withTop auth currentUser)
            I.ifElseISplice (maybe False (\au -> Role "admin" `elem` userRoles au) mbAu)
          
    addConfig h (mempty & scInterpretedSplices .~ sc)
    -- Grab the DB connection pool from the sqlite snaplet and call
    -- into the Model to create all the DB tables if necessary.
    let c = sqliteConn $ d ^# snapletValue
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




