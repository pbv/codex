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
import           Data.Maybe(listToMaybe, isJust)
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
import           Language
import           Markdown
import           Text.Pandoc.Builder
-- import qualified Interval as Interval
-- import           Interval (Interval)
import           Problem
import           Submission
import           LdapAuth
-- import           Report
-- import           Printout

import           Paths_pythondo(version)
import           Data.Version (showVersion)  

import           AceEditor

-- interpreted splices for Pythondo handlers
type ISplices = Splices (I.Splice AppHandler)



------------------------------------------------------------------------------
-- | Handle login requests 
handleLogin :: AppHandler ()
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
{-
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLoginForm err) (redirect "/problems")
  where
    err = Just "Incorrect user or password"

-}

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
handleLogout :: AppHandler ()
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
handlePrintout :: UID -> [Problem] -> AppHandler ()
handlePrintout uid probs = do
  subs <- mapM (getBestSubmission uid) (map probID probs)
  report <- genReport (zip probs subs)
  liftIO $ makePrintout (show uid) report
-}


{-
------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler AppHandler (AuthManager AppHandler) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where

handleForm = render "register"
    handleFormSubmit = do
      login <- reqParam "login" 
      b <- usernameExists (T.pack $ B.toString login)
      if b then render "user_exists"
        else 
        registerUser "login" "password" >> redirect "/"
-}



-- | handle problem requests
handleDocument :: FilePath -> AppHandler ()
handleDocument root = do
  uid <- require getUserID <|> unauthorized    -- ensure user is logged in
  path <- B.toString <$> getsRequest rqPathInfo
  when (null path) $ redirect indexPage
  when (not $ isMarkdown path) pass
  withSplices ("documentPath" ## I.textSplice (T.pack path)) $ do
    ws <- getWorksheet (root </> path)
    mpid <- getProblemID
    case (mpid >>= lookupProblem ws) of
      Nothing -> handleWorksheet ws uid
      Just prob -> do
        msid <- getSubmitID
        case msid of
          Just sid -> handleSubmission prob uid sid
          Nothing -> handleProblem ws prob uid

lookupProblem :: Worksheet Problem -> ProblemID -> Maybe Problem
lookupProblem Worksheet{..}  pid
  = listToMaybe [p | Right p <- worksheetItems, probID p == pid]


handleWorksheet ws uid = method GET $ do
  ws' <- getWorksheetSubmissions uid ws -- collect submissions for this worksheet
  now <- liftIO getCurrentTime
  renderWithSplices "worksheet" $ do
    "worksheetItems" ## I.mapSplices (I.runChildrenWith . wsItemSplices now) (worksheetItems ws')
  

handleProblem ws@Worksheet{..} prob@Problem{..} uid
  = method GET handleGet <|> method POST handlePost
  where
    handleGet = do 
      now <- liftIO getCurrentTime
      subs <- getSubmissions uid probID
      renderWithSplices "problem"
        (problemSplices prob >>
         submissionListSplices prob subs >>
         inputAceEditorSplices >>
         timerSplices probID now probLimit)
    handlePost = do
      now <- liftIO getCurrentTime
      code <- toCode <$> require (getTextPost "editform.editor")
      sub <- newSubmission uid prob code
      renderWithSplices "report"
        (problemSplices prob >>
         submissionSplices prob sub >>
         inputAceEditorSplices >>
         timerSplices probID now probLimit)
      

handleSubmission prob@Problem{..} uid sid = method GET $ do 
  sub <- getSubmission uid probID sid
  renderWithSplices "report" (problemSplices prob >>
                              submissionSplices prob sub >>
                              inputAceEditorSplices)



{-
-----------------------------------------------------------------------------
-- | problem description request 
handleProblem :: AppHandler ()
handleProblem = method GET $ do
    uid <- require getUserID  <|> unauthorized
    pid <- require getProblemID
    (prob,mesgs) <- getProblem pid
    now <- liftIO getCurrentTime
    subs <- getSubmissions uid pid
    renderWithSplices "problem" $
      do problemSplices prob
         inputAceEditorSplices
         submissionsSplice subs
         timerSplices pid now (probDeadline prob)
         warningsSplices mesgs
-}

{-
   
-- | problem set listing handler
handleProblemList :: AppHandler ()
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
getQueryTags :: AppHandler [Tag]
getQueryTags = do
  params <- getParams
  return (map (T.pack . B.toString) $ Map.findWithDefault [] "tag" params)
-}


warningsSplices :: [Text] -> ISplices
warningsSplices mesgs = do
  "warnings" ## I.mapSplices (I.runChildrenWith . mesgSplice) mesgs
  where mesgSplice msg = "message" ## I.textSplice msg



-- | splices related to a single problem       
problemSplices :: Problem -> ISplices
problemSplices Problem{..} = do
  "problemID" ## I.textSplice (T.pack $ B.toString $ fromPID probID)
  "problemLanguage" ## maybe (return []) I.textSplice (lookup "language" probAttrs)
  "problemCode" ## maybe (return []) (I.textSplice . fromCode) probCode
  "problemHeader" ## return (blocksToHtml $ singleton probHeader)
  "problemTitle" ## return (inlinesToHtml $ headerInlines probHeader)
  "problemDescription" ## return (blocksToHtml probDescr)
  {-
   "problemTags" ## I.textSplice (T.unwords probTags)
   "problemPath" ## I.textSplice (T.pack probPath)
   -}



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

  
-- | splice an UTC time as local time 
timeSplice :: UTCTime -> I.Splice AppHandler
timeSplice time = do tz <- liftIO getCurrentTimeZone
                     I.textSplice $ T.pack $
                       formatTime defaultTimeLocale "%c" $ utcToZonedTime tz time
    

-- format a time difference
formatNominalDiffTime :: NominalDiffTime -> String
formatNominalDiffTime secs 
  | secs>=0  = unwords $ 
               [show d ++ "d" | d>0] ++ 
               [show (h`rem`24) ++ "h" | h>0] ++
               [show (m`rem`60) ++ "m" | m>0] ++
               ["<1m" | secs<60]
  | otherwise = "--/--"
  where m = (floor (secs / 60)) :: Int
        h = (m `div` 60) 
        d = (h `div` 24)  



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
  {-
  "ifRejected" ## conditionalSplice (submitStatus /= Accepted &&
                                     submitStatus /= Overdue)
-}


{-
handleGetSubmission, handlePostSubmission :: AppHandler ()
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
handleConfirmLogout :: AppHandler ()
handleConfirmLogout = method GET $ do
  uid <- require getUserID <|> badRequest
  (probset,_) <- getProblemSet
  handleFinalReport uid probset


handleFinalReport :: UID -> ProblemSet -> AppHandler ()
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
handleAdminEdit :: AppHandler ()
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
routes :: [(ByteString, AppHandler ())]
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


routes :: [(ByteString, AppHandler ())]
routes = [ ("/login",    handleLogin `catch` internalError)
         , ("/logout",   handleLogout `catch` internalError)
         , ("",   serveDirectory staticPath <|>
                  (handleDocument publicPath `catch` internalError) <|>
                   notFound)
         ]


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
nowSplice :: I.Splice AppHandler
nowSplice = do t <- liftIO (getCurrentTime >>= utcToLocalZonedTime)
               I.textSplice (T.pack $ formatTime defaultTimeLocale "%c" t)



versionSplice :: I.Splice AppHandler
versionSplice = I.textSplice (T.pack (showVersion version))

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = 
  makeSnaplet "pythondo" "Web system for learning Python programming." Nothing $ do
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


-- emptyConfig :: HeistConfig m
-- emptyConfig = HeistConfig noSplices noSplices noSplices noSplices []


-- initialize EKG server (if configured)
initEkg :: Config -> IO (Maybe Server)
initEkg conf = do enabled <- Configurator.require conf "ekg.enabled"
                  if enabled then 
                    do host <- Configurator.require conf "ekg.host"
                       port <- Configurator.require conf "ekg.port"
                       Just <$> forkServer host port                   
                    else return Nothing
  

{-
-- | get the current problem set
-- TODO: avoid re-reading the same problems every time
-- with some caching mechanism
getProblemSet :: AppHandler (ProblemSet, [Text])
getProblemSet = liftIO (readProblemSet problemSetPath)

getProblem :: PID -> AppHandler (Problem, [Text])
getProblem pid = do
  (probset,mesgs) <- getProblemSet
  case lookupProblemSet pid probset of
    Nothing -> badRequest
    Just p -> return (p,mesgs)
-}

-- get a worksheet by path
getWorksheet :: FilePath -> AppHandler (Worksheet Problem)
getWorksheet  path = liftIO (readWorksheet path)

publicPath :: FilePath
publicPath = "public"

staticPath :: FilePath
staticPath = "static"

isMarkdown :: FilePath -> Bool
isMarkdown path = map toLower (takeExtension path) == ".md"

indexPage :: ByteString
indexPage = "/index.md"


{-
lookupProblemSet :: PID -> ProblemSet -> Maybe Problem
lookupProblemSet pid ProblemSet{..} =
  listToMaybe [p | p <- probsetProbs, probID p == pid] 
-}    

