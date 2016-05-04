{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
--import           Prelude hiding (catch)
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
-- import           Data.Map (Map)
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
                    ; ldapconf <- gets ldapConf
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
  if Page.isExercise page then
    method GET (Submission.getAll uid path >>= renderExercise page)
    <|>
    method POST (handlePost uid page)
    else
       method GET $ do
         linked <- liftIO $ mapM (Page.readPage pageFileRoot) (links page)
         sublist <- mapM (Submission.getAll uid . Page.path) linked
         renderWithSplices "indexsheet" (pageSplices page >>
                                         indexSplices (zip linked sublist))

--  render an exercise page
renderExercise :: Page -> [Submission] -> Codex ()
renderExercise page subs = do
  tz <- liftIO getCurrentTimeZone
  now <- liftIO getCurrentTime
  env <- require getUserEvents
  renderWithSplices "exercise" (pageSplices page >>
                                exerciseSplices page >>
                                timeSplices tz now env page >>
                                submissionListSplices subs >>
                                inputAceEditorSplices)


-- handle submission of an exercise
handlePost :: UserID -> Page -> Codex ()
handlePost uid page = do
  tz <- liftIO getCurrentTimeZone
  now <- liftIO getCurrentTime
  env <- require getUserEvents
  text <- require (getTextPost "editform.editor")
  let lang = Page.getLanguage page
  sub <- Submission.evaluate uid page (Code lang text)
  renderWithSplices "report" (pageSplices page >>
                              exerciseSplices page >>
                              submissionSplices sub >>
                              timeSplices tz now env page >>
                              inputAceEditorSplices)


-- handle get request for a previous submission
handleSubmission = method GET $ do
  tz <- liftIO getCurrentTimeZone
  now <- liftIO getCurrentTime
  uid <- require getUserID <|> unauthorized
  sid <- require getSubmitID
  env <- require getUserEvents
  sub <- Submission.getSingle uid sid
  page <- liftIO $ Page.readPage pageFileRoot (Submission.path sub)
  renderWithSplices "report" (pageSplices page >>
                              exerciseSplices page >>
                              submissionSplices sub >>
                              timeSplices tz now env page >>
                              inputAceEditorSplices)



-- | splices related to a page
pageSplices :: Page -> ISplices
pageSplices page = do
  "pagePath" ##
    I.textSplice (T.pack $ B.toString pageContextPath </> Page.path page)
  "pageParent" ##
    I.textSplice (T.pack $ B.toString pageContextPath </> Page.parent (Page.path page))
  "pageTitle" ##
    I.textSplice (Page.getTitle page)
  "pageDescription" ##
    return (blocksToHtml $ Page.description page)
  "ifExercise" ##
    conditionalSplice (Page.isExercise page)

exerciseSplices :: Page -> ISplices
exerciseSplices page = do
  "pageLanguage" ##
    I.textSplice (fromLanguage (Page.getLanguage page))
  "pageCodeText" ##
    maybe (return []) I.textSplice (Page.getCodeText page)


indexSplices :: [(Page, [Submission])] -> ISplices
indexSplices pageSubs = do
  "indexList" ##
    I.mapSplices (I.runChildrenWith . auxSplices) pageSubs
  where
    auxSplices (page, subs) = do
                pageSplices page
                exerciseSplices page
                submissionListSplices subs


-- | splices relating to a single submission
submissionSplices :: Submission -> ISplices
submissionSplices Submission{..} = do
  let timing' = fromMaybe OnTime timing
  "submitID" ##
    I.textSplice (toText id)
  "submitPath" ##
    I.textSplice (T.pack path)
  "submitTime" ##
    timeSplice time 
  "submitCodeText" ##
    I.textSplice (codeText code)
  "submitClassify" ##
    I.textSplice (T.pack $ show $ resultClassify result)
  "submitMessage" ##
    I.textSplice (resultMessage result)
  "submitTiming" ##
    caseSplice timing'
  "submitOnTime" ##
    conditionalSplice (timing' == OnTime)
  "submitEarly" ##
    conditionalSplice (timing' == Early)
  "submitOverdue" ##
    conditionalSplice (timing' == Overdue)
  "submitAccepted" ##
    conditionalSplice (resultClassify result == Accepted)




-- | splices relating to a list of submissions
submissionListSplices :: [Submission] -> ISplices
submissionListSplices list = do
  "submissionsCount" ##
    I.textSplice (T.pack $ show count)
  "ifSubmitted" ##
    conditionalSplice (count > 0) 
  "submissionList" ##
    I.mapSplices (I.runChildrenWith . submissionSplices) list
  where count = length list


timeSplices :: TimeZone -> UTCTime -> Events -> Page -> ISplices
timeSplices tz now env Page{..} = do
  "exerciseTiming" ##
    caseSplice (fromMaybe OnTime (Interval.timingVal env interval now))
  "validFrom" ##
    maybe (return [])
    (I.textSplice . T.pack)
    (Interval.from interval >>= showTimeExpr tz env)
  "validUntil" ##
    maybe (return [])
    (I.textSplice . T.pack)
    (Interval.until interval >>= showTimeExpr tz env)





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
-- get tag list from query string
getQueryTags :: Codex [Tag]
getQueryTags = do
  params <- getParams
  return (map (T.pack . B.toString) $ Map.findWithDefault [] "tag" params)
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
  
-- | splice an UTC time as local time 
timeSplice :: UTCTime -> I.Splice Codex
timeSplice time = do tz <- liftIO getCurrentTimeZone
                     I.textSplice $ T.pack $
                       formatTime defaultTimeLocale "%c" $ utcToZonedTime tz time
    


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
    e <- liftIO $ initEkg conf
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

    py <- liftIO $ configPython conf
    hs <- liftIO $ configHaskell conf
    prt <- liftIO $ configPrintConf conf
    ldap <- liftIO $ configLdapConf conf
    return $ App { _heist = h
                 , _sess = s
                 , _auth = a
                 , _db   = d
                 , pythonConf= py
                 , haskellConf = hs
                 , ldapConf = ldap
                 , printConf = prt
                 , ekg = e
                 }



-- initialize EKG server (if configured)
initEkg :: Config -> IO (Maybe Server)
initEkg conf = do enabled <- Configurator.require conf "ekg.enabled"
                  if enabled then 
                    do host <- Configurator.require conf "ekg.host"
                       port <- Configurator.require conf "ekg.port"
                       Just <$> forkServer host port                   
                    else return Nothing
  





