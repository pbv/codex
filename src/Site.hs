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

import           Control.Monad.CatchIO (catch)
import           Control.Monad.State
import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Lens

import           Data.Char (toLower)
import           Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as B
import qualified Data.ByteString      as B
import           Data.Monoid
import           Data.Maybe(listToMaybe, maybeToList, isJust, fromMaybe)

import           Data.List (nub, sort)

import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.SqliteSimple
import           Snap.Snaplet.SqliteSimple
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Heist
import           Heist.Splices
import qualified Heist.Interpreted as I

import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.Format

import qualified Data.Configurator as Configurator
import           Data.Configurator.Types

import           System.Locale
import           System.FilePath
import           System.IO.Error
import           System.Remote.Monitoring
import qualified Text.XmlHtml as X


------------------------------------------------------------------------------
import           Application
import           Db
import           Utils
import           Types
import           Tester
import           Language
import           Markdown
import           Interval
import           Text.Pandoc.Builder hiding (Code)
-- import qualified Interval as Interval
-- import           Interval (Interval)
import           Page  (Page(..))
import qualified Page
import           Submission (Submission(..))
import qualified Submission
import           LdapAuth
import           Config

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
handleLogin 
  = method GET (with auth $ handleLoginForm Nothing) <|>
    method POST (do { user <- require (getParam "login") 
                    ; passwd <- require (getParam "password")
                    ; conf <- getSnapletUserConfig
                    ; ldapconf <- liftIO (getLdapConf conf)
                    ; with auth $ handleLoginSubmit ldapconf user passwd
                    } <|> badRequest)


-- | Render login form
handleLoginForm :: Maybe Text -> Handler App (AuthManager App) ()
handleLoginForm authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = "loginError" ## maybe (return []) I.textSplice authError

------------------------------------------------------------------------------

-- | Handle login submit using LDAP authentication
handleLoginSubmit :: 
  LdapConf -> ByteString -> ByteString -> Handler App (AuthManager App) ()
handleLoginSubmit ldapConf user passwd = do
  -- optAuth <- withBackend (\r -> liftIO $ ldapAuth r ldapConf user passwd)
  optAuth <- withBackend (\r -> liftIO $ dummyAuth r ldapConf user passwd)
  case optAuth of 
    Nothing -> handleLoginForm err
    Just u -> forceLogin u >> redirect indexPage
  where 
    err = Just "Utilizador ou password incorreto"


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
  uid <- require getUserID <|> unauthorized    -- ensure user is logged in
  path <- B.toString <$> getsRequest rqPathInfo
  page <- liftIO $ Page.readPage pageFileRoot path
  method GET (handlePageGet uid page) <|>
    method POST (handlePagePost uid page)


-- handle page GET requests
handlePageGet uid page 
  | Page.isExercise page = 
      renderExercise page =<< Submission.getAll uid (Page.path page) 
  | Just links <- Page.getLinks page = 
      renderIndex uid page links
  | otherwise =
      renderWithSplices "page" (pageSplices page)


renderIndex uid page links = do
  querytags <- getQueryTags
  pages <- liftIO $ mapM (Page.readPage pageFileRoot) links
  let alltags = nub $ sort $ concatMap Page.getTags pages
  let pages' = filter (\p -> querytags `contained` Page.getTags p) pages
  let abletags = nub $ sort $ concatMap Page.getTags pages'
  subs <- mapM (Submission.getAll uid . Page.path) pages'
  renderWithSplices "indexsheet" $ do
    pageSplices page 
    indexSplices (zip pages' subs)
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


-- handle GET request for a previous submission
handleSubmission = method GET $ do
  uid <- require getUserID <|> unauthorized
  sid <- require getSubmitID
  sub <- Submission.getSingle uid sid
  page <- liftIO $ Page.readPage pageFileRoot (Submission.path sub)
  renderWithSplices "report" $ do
    pageSplices page
    exerciseSplices page
    submitSplices sub 
    inputAceEditorSplices


-- handle POST requests for pages (i.e. exercise submissions)
handlePagePost :: UserID -> Page -> Codex ()
handlePagePost uid page  = do
  guard (Page.isExercise page)
  text <- require (getTextPost "editform.editor")
  lang <- require (return $ Page.getLanguage page)
  sub <- Submission.evaluate uid page (Code lang text)
  renderWithSplices "report" $ do 
    pageSplices page
    exerciseSplices page
    submitSplices sub 
    inputAceEditorSplices



-- | splices related to a page
pageSplices :: Page -> ISplices
pageSplices page = namespaceSplices "page" $ do
  "path" ##
    I.textSplice (T.pack $ B.toString pageContextPath </> Page.path page)
  "parent" ##
    I.textSplice (T.pack $ B.toString pageContextPath </> Page.parent (Page.path page))
  "title" ##
    I.textSplice (Page.getTitle page)
  "description" ##
    return (blocksToHtml $ Page.description page)
  "if-exercise" ##
    conditionalSplice (Page.isExercise page)
  "if-indexsheet" ##
    conditionalSplice (isJust $ Page.getLinks page)


-- | splices related to exercises
exerciseSplices :: Page -> ISplices
exerciseSplices page = namespaceSplices "exercise" $ do
  -- splices related to programming languages
  "language" ##
    maybe (return []) (I.textSplice . fromLanguage) (Page.getLanguage page)
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
submitSplices Submission{..} = namespaceSplices "submit" $ do
  "id" ##  I.textSplice (toText id)
  "path" ## I.textSplice (T.pack path)
  "time" ## do {tz <- liftIO getCurrentTimeZone; utcTimeSplice tz time}
  "code-text" ##  I.textSplice (codeText code)
  "classify" ##  I.textSplice (T.pack $ show $ resultClassify result)
  "message" ## I.textSplice (resultMessage result)
  "timing" ## caseSplice timing
  "valid" ## conditionalSplice (timing == Valid)
  "early" ## conditionalSplice (timing == Early)
  "overdue" ## conditionalSplice (timing == Overdue)
  "accepted" ## conditionalSplice (resultClassify result == Accepted)



-- | splices relating to a list of submissions
submissionListSplices :: [Submission] -> ISplices
submissionListSplices list = do
  let count = length list
  "submissions-count" ##
    I.textSplice (T.pack $ show count)
  "if-submitted" ##
    conditionalSplice (count > 0) 
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
-- | problem set listing handler
handleProblemList :: Codex ()
handleProblemList = method GET $ do
  uid <- require getUserID <|> unauthorized
  now <- liftIO getCurrentTime
  (probset, mesgs) <- getProblemSet
  tags <- getQueryTags
  -- summary of all available problems
  available <- getProblemSummary uid probset 
  -- filter by query tags
  let visible = filter (hasTags tags) available
      
  -- context-dependent splices for tags 
  let tagSplices tag = 
        let checked = tag`elem`tags
            disabled= null (filter (isTagged tag) visible)
        in do "tagText" ## I.textSplice tag
              "tagCheckbox" ## return (checkboxInput tag checked disabled)

  -- render page
  renderWithSplices "problemlist" $ do
    warningsSplices mesgs
    "problemsetDescription" ## return (renderPandoc $ probsetDescr probset)
    "problemsetPath" ## I.textSplice (T.pack $ probsetPath probset)
    "tagList" ## I.mapSplices (I.runChildrenWith . tagSplices) (taglist available)
    "problemList" ##  I.mapSplices (I.runChildrenWith . summarySplices now) visible
    "availableProblems" ## I.textSplice (T.pack $ show $ length available)
    "visibleProblems" ## I.textSplice (T.pack $ show $ length visible)
               
-}


{-
warningsSplices :: [Text] -> ISplices
warningsSplices mesgs = do
  "warnings" ## I.mapSplices (I.runChildrenWith . mesgSplice) mesgs
  where mesgSplice msg = "message" ## I.textSplice msg
-}

{-
-- | splices related to worksheet items
wsItemSplices :: UTCTime -> Either Blocks (Problem,[Submission]) -> ISplices
wsItemSplices now (Left blocks) = do
  "ifProblem" ## conditionalSplice False
  "itemBlocks" ## return (blocksToHtml blocks)
  
wsItemSplices now (Right (prob@Problem{..},list)) 
  = let tot = length list 
        acc = length [sub | sub@Submission{..}<-list,
                      submitResult == Accepted && submitQualifier==OK ]
    in do
      "ifProblem" ## conditionalSplice True
      "submissions" ## I.textSplice (T.pack $ show tot)
      "accepted" ## I.textSplice (T.pack $ show acc)
      "ifSubmissions" ## conditionalSplice (tot > 0)
      "ifAccepted" ## conditionalSplice (acc > 0)
      problemSplices prob
      timerSplices probID now probLimit
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
  
-- | splice an UTC time as a local time string
utcTimeSplice :: Monad m => TimeZone -> UTCTime -> I.Splice m
utcTimeSplice tz t =
  I.textSplice $ T.pack $ formatTime defaultTimeLocale "%c" $ utcToLocalTime tz t
    


{-
-- | splices relating to a list of submissions
submissionListSplices :: Problem -> [Submission] -> ISplices
submissionListSplices prob list = do
  "submissions" ## I.textSplice (T.pack $ show n)
  "ifSubmissions" ## conditionalSplice (n > 0) 
  "submissionList" ## I.mapSplices (I.runChildrenWith . submissionSplices prob) list
  -- "ifAccepted" ##  I.textSplice "IFACCEPTED?"
    -- conditionalSplice (any (\s->submitStatus s==Accepted) list)
  where n = length list

        
-- | splices relating to a single submission
submissionSplices :: Problem -> Submission -> ISplices
submissionSplices Problem{..} Submission{..} = do 
  "submitID"   ## I.textSplice (T.pack $ show $ fromSID submitID)
  "submitPID"  ## I.textSplice (T.pack $ B.toString $ fromPID submitPID)
  "submitTime" ## timeSplice submitTime 
  "submitCode" ## I.textSplice (fromCode submitCode)
  "submitResult" ## I.textSplice (T.pack $ show submitResult)
  "submitQualifier" ## I.textSplice (T.pack $ show submitQualifier)  
  "submitMsg" ## I.textSplice submitMsg
  "ifAccepted" ## conditionalSplice (submitResult == Accepted)
  "ifOverdue" ## conditionalSplice (submitQualifier == Overdue)
  "ifRejected" ## conditionalSplice (submitStatus /= Accepted &&
                                     submitStatus /= Overdue)
-}


{-
handleGetSubmission, handlePostSubmission :: Codex ()
handleGetSubmission 
    = method GET $ do 
        uid <- require getUserID <|> unauthorized
        pid <- require getProblemID 
        sid <- require getSubmissionID
        (prob,mesgs) <- getProblem pid
        sub <- getSubmission uid pid sid
        renderWithSplices "report" $ do inputAceEditorSplices
                                        problemSplices prob  
                                        submissionSplices sub
                                        warningsSplices mesgs


handlePostSubmission = method POST $ do
  uid <- require getUserID <|> unauthorized
  pid <- require getProblemID
  code <- require (getTextPost "editform.editor") 
  (prob,mesgs) <- getProblem pid
  now <- liftIO getCurrentTime
  sub <- postSubmission uid prob code
  incrCounter "submissions"
  renderWithSplices "report" (inputAceEditorSplices >>
                              problemSplices prob >>
                              submissionSplices sub >>
                              timerSplices pid now (probDeadline prob) >>
                              warningsSplices mesgs)
    
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

-----------------------------------------------------------------------------
-- administrator interface
-----------------------------------------------------------------------------
{-
handleAdminEdit :: Codex ()
handleAdminEdit = do
      uid <- require getUserID
      roles <- require getUserRoles
      guard (adminRole `elem` roles)
      method GET handleGet <|> method POST handlePost 
  where
    handleGet = do
      (_, mesgs) <- getProblemSet
      path <- getsRequest rqPathInfo
      opt_pid <- getParam "pid"
      source <- liftIO (catchIOError
                        (T.readFile $ B.toString path)
                        (\_ -> return ""))
      renderWithSplices "editfile" $ do
        inputAceEditorSplices
        warningsSplices mesgs
        "editPath" ## I.textSplice (T.pack $ B.toString path)
        "editText" ## I.textSplice source
        "problemID" ## maybe (return []) (I.textSplice . T.pack . B.toString) opt_pid
    handlePost  = do
      path <- getsRequest rqPathInfo
      opt_pid <- getParam "pid"
      source <- require (getTextPost "editform.editor")
      liftIO (T.writeFile (B.toString path) source)
      redirect (B.append "/problems/" $ maybe "" id opt_pid)


handleAdminSubmissions = method GET $ do
  uid <- require getUserID
  roles <- require getUserRoles
  guard (adminRole `elem` roles)
  serveFileAs "application/x-sqlite3" "submissions.db"
        
-}

      

------------------------------------------------------------------------------
-- | The application's routes.
  {-
routes :: [(ByteString, Codex ())]
routes = [ ("/login",                 handleLogin `catch` internalError)
         , ("/logout",                handleLogout `catch` internalError)
         , ("/problems",              handleProblemList `catch` internalError )  
         , ("/problems/:pid",         handleProblem `catch` internalError )
         , ("/files",                 serveDirectory problemDirPath <|> notFound)
         , ("/edit",                  handleAdminEdit `catch` internalError)
         , ("/submissions/:pid",      handlePostSubmission `catch` internalError)
         , ("/submissions/:pid/:sid", handleGetSubmission  `catch` internalError)
         , ("/admin/submissions",     handleAdminSubmissions `catch` internalError)
         , ("/asklogout",             handleConfirmLogout `catch` internalError)        
         , ("",                       serveDirectory "static" <|> notFound)
         ]
-}


routes :: [(ByteString, Codex ())]
routes = [ ("/login",    handleLogin `catch` internalError)
         , ("/logout",   handleLogout `catch` internalError)
         , ("/page",     handlePage  `catch` internalError)
         , ("/submit/:sid", handleSubmission `catch` internalError)
         , ("",   serveDirectory staticFilePath <|> notFound) 
         ]


pageFileRoot :: FilePath
pageFileRoot = "public"

staticFilePath :: FilePath
staticFilePath = "static"

pageContextPath :: ByteString
pageContextPath = "/page"

indexPage :: ByteString
indexPage = "/page/index.md"


-- | current logged in full user name  
loggedInName :: SnapletLens b (AuthManager b) -> SnapletISplice b
loggedInName authmgr = do
    u <- lift $ withTop authmgr currentUser
    maybe (return []) (I.textSplice . userName) u 

authRoles :: SnapletLens b (AuthManager b) ->
             ([Role] -> Bool) -> SnapletISplice b
authRoles authmgr cond = do
   u <- lift $ withTop authmgr currentUser
   maybe (return []) (\u -> conditionalSplice (cond $ userRoles u)) u

-- | splice for current date & time
nowSplice :: I.Splice Codex
nowSplice = do t <- liftIO (getCurrentTime >>= utcToLocalZonedTime)
               I.textSplice (T.pack $ formatTime defaultTimeLocale "%c" t)



versionSplice :: I.Splice Codex
versionSplice = I.textSplice (T.pack (showVersion version))

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = 
  makeSnaplet "codex" "Web server for programming exercises." Nothing $ do
    conf <- getSnapletUserConfig
    e <- liftIO (initEkg conf)
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 7200)

    d <- nestSnaplet "db" db sqliteInit
    a <- nestSnaplet "auth" auth $ initSqliteAuth sess d
    addAuthSplices h auth
    let sc = do 
          "version" ## versionSplice
          "timeNow" ## nowSplice
          "loggedInName" ## loggedInName auth
          "ifAdmin" ## authRoles auth (adminRole `elem`)
          
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
               , ekg = e
               }



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
  





