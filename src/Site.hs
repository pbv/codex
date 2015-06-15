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

import           Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as B
import qualified Data.ByteString      as B
import           Data.Maybe(listToMaybe)
-- import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T

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

import           System.Remote.Monitoring

------------------------------------------------------------------------------
import           Application
import           Db
import           Utils
import           Types
import qualified Interval as Interval
import           Interval (Interval)
import           Problem
import           Submission
import           Summary
import           LdapAuth
import           Printout

import           Paths_pythondo(version)
import           Data.Version (showVersion)  


import qualified Text.XmlHtml as X

-- my application splices
type AppSplices = Splices (I.Splice AppHandler)



------------------------------------------------------------------------------
-- | Handle login requests 
handleLogin :: AppHandler ()
handleLogin 
  = method GET (with auth $ handleLoginForm Nothing) <|>
    method POST (do { user <- require (getParam "login") 
                    ; passwd <- require (getParam "password")
                    ; ldapconf <- getLdapConf
                    ; with auth $ handleLoginSubmit ldapconf user passwd
                    } <|> badRequest)


-- | Render login form
handleLoginForm :: Maybe Text -> Handler Pythondo (AuthManager Pythondo) ()
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
  LdapConf -> ByteString -> ByteString -> Handler Pythondo (AuthManager Pythondo) ()
handleLoginSubmit ldapConf user passwd = do
  optAuth <- withBackend (\r -> liftIO $ ldapAuth r ldapConf user passwd)
  -- optAuth <- withBackend (\r -> liftIO $ dummyAuth r ldapConf user passwd)
  case optAuth of 
    Nothing -> handleLoginForm err
    Just u -> forceLogin u >> redirect "/problems"
  where 
    err = Just "Utilizador ou password incorreto"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
--  in exam mode procedeed to printout 
handleLogout :: AppHandler ()
handleLogout = method GET $ do
    uid <- require getUserID <|> unauthorized
    probset <- getProblemSet
    -- handle printout (if configured)
    handlePrintout uid probset
    with auth logout 
    redirect "/" 


{-
------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler Pythondo (AuthManager Pythondo) ()
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

-----------------------------------------------------------------------------
-- | problem description request 
handleProblem :: AppHandler ()
handleProblem = method GET $ do
    uid <- require getUserID  <|> unauthorized
    pid <- require getProblemID
    prob <- getProblem pid
    now <- liftIO getCurrentTime
    subs <- getSubmissions uid pid
    renderWithSplices "problem" $ do problemSplices prob
                                     submissionsSplice subs
                                     timerSplices pid now (probOpen prob)



        
-- | problem set listing handler
handleProblemList :: AppHandler ()
handleProblemList = method GET $ do
  uid <- require getUserID <|> unauthorized
  now <- liftIO getCurrentTime
  probset <- getProblemSet 
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
    "problemset_description" ## return (renderPandoc $ probsetDescr probset)
    "tag_list" ## I.mapSplices (I.runChildrenWith . tagSplices) (taglist available)
    "problem_list" ##  I.mapSplices (I.runChildrenWith . summarySplices now) visible
    "available_problems" ## I.textSplice (T.pack $ show $ length available)
    "visible_problems" ## I.textSplice (T.pack $ show $ length visible)
               
               
-- get tag list from query string
getQueryTags :: AppHandler [Tag]
getQueryTags = do
  params <- getParams
  return (map (T.pack . B.toString) $ Map.findWithDefault [] "tag" params)



summarySplices :: UTCTime -> ProblemSummary -> AppSplices
summarySplices now ProblemSummary{..} 
    = do problemSplices summaryProb
         timerSplices (probID summaryProb) now (probOpen summaryProb)
         "number_submissions" ## I.textSplice (T.pack $ show summaryAttempts)
         "number_accepted"    ## I.textSplice (T.pack $ show summaryAccepted)
         "if_submitted" ## conditionalSplice (summaryAttempts>0)
         "if_accepted" ## conditionalSplice (summaryAccepted>0)

--         counterSplices summaryAttempts
-- "ifAccepted" ## conditionalSplice (summaryAccepted > 0) 

         


-- | splices related to a single problem       
problemSplices :: Problem -> AppSplices
problemSplices Problem{..} = do
  "problem_id" ## I.textSplice (T.pack $ show probID)
  "problem_title" ## maybe (return []) I.textSplice probTitle
  "problem_description" ## return (renderPandoc probDescr)
  "problem_default" ## maybe (return []) I.textSplice probDefault
  "problem_tags" ## I.textSplice (T.unwords probTags)





-- | splices related to problem's time interval
timerSplices ::  PID -> UTCTime -> Interval UTCTime -> AppSplices
timerSplices pid now open = do
  "if_open" ##  conditionalSplice (now`Interval.elem`open)
  "if_early" ## conditionalSplice (now`Interval.before`open)
  "if_late"  ## conditionalSplice (now`Interval.after`open)
  "if_limited" ## conditionalSplice (Interval.limited open)
  "start_time" ## maybe (return []) timeSplice (Interval.start open)
  "end_time" ## maybe (return []) timeSplice (Interval.end open)
  "remaining_time" ## I.textSplice $ T.pack $ case Interval.end open of 
                   Nothing -> "N/A"
                   Just t' -> formatNominalDiffTime $ diffUTCTime t' now
  "remaining_js_timer" ## case Interval.end open of 
                            Nothing -> return []
                            Just t' -> return $ jsTimer pid (diffUTCTime t' now)

{-
  "ifOpen" ## conditionalSplice (now `Interval.elem` interval)
  "ifEarly" ## conditionalSplice (now `Interval.before`interval)
  "ifLate" ##  conditionalSplice (now `Interval.after`interval)
  "ifLimited" ## conditionalSplice (Interval.limited interval)
  "timerStart" ##  maybe (return []) timeSplice (Interval.start interval)
  "timerEnd" ##  maybe (return []) timeSplice (Interval.end interval)
  "timerLeft"  ## case Interval.end interval of 
    Nothing -> return []
    Just t' -> I.textSplice $ T.pack $ formatNominalDiffTime $ diffUTCTime t' now
-}
  
-- splice an UTC time as local time 
timeSplice :: UTCTime -> I.Splice AppHandler
timeSplice time = do tz <- liftIO getCurrentTimeZone
                     I.textSplice $ T.pack $ formatTime defaultTimeLocale "%c" $ utcToZonedTime tz time
    

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
submissionsSplice :: [Submission] -> AppSplices
submissionsSplice lst = do
  "submissions" ## I.mapSplices (I.runChildrenWith . submissionSplices) lst
  "number_submissions" ## I.textSplice (T.pack $ show n)
  "if_accepted" ## conditionalSplice (any isAccepted lst)
  "if_submitted" ## conditionalSplice (n>0) 
  where n = length lst

        
-- | splices relating to a single submission
submissionSplices :: Submission -> AppSplices
submissionSplices Submission{..} = do 
  "submit_id"   ## I.textSplice (T.pack $ show submitID)
  "submit_pid"  ## I.textSplice (T.pack $ show submitPID)
  "submit_time" ## timeSplice submitTime 
  "submit_text" ## I.textSplice submitText
  "submit_status" ## I.textSplice (T.pack $ show submitStatus)
  "submit_report" ## I.textSplice submitReport 
  "if_accepted" ## conditionalSplice (submitStatus == Accepted) 
  "if_overdue" ##  conditionalSplice (submitStatus == Overdue) 
  "if_rejected" ## conditionalSplice (submitStatus/= Accepted && submitStatus/=Overdue) 




{-
-- | splices concerning submissions count
counterSplices :: Int -> AppSplices
counterSplices n = do 
  "count" ## I.textSplice (T.pack $ show n)
  "ifSubmitted" ## conditionalSplice (n>0)
-}



handleGetSubmission, handlePostSubmission :: AppHandler ()
handleGetSubmission 
    = method GET $ do 
        uid <- require getUserID <|> unauthorized
        pid <- require getProblemID 
        sid <- require getSubmissionID
        prob <- getProblem pid
        sub <- getSubmission uid pid sid
        renderWithSplices "report" $ do problemSplices prob  
                                        submissionSplices sub


handlePostSubmission = method POST $ do
  uid <- require getUserID <|> unauthorized
  pid <- require getProblemID 
  code <- T.decodeUtf8With T.ignore <$> require (getParam "code")
  prob <- getProblem pid
  now <- liftIO getCurrentTime
  sub <- postSubmission uid prob code
  incrCounter "submissions"
  renderWithSplices "report" $ do problemSplices prob 
                                  submissionSplices sub 
                                  timerSplices pid now (probOpen prob)


{-
-- handler for listing all submissions 
-- quick and dirty output routine 
handleSubmissions :: Handler App App ()
handleSubmissions = method GET $ do
  getUser    -- should be authenticated
  stats <- liftIO getAllStatus
  names <- with auth $ withBackend $ \r -> liftIO $ 
    sequence [lookup r txt | uid<-Map.keys stats, let txt=T.pack $ show uid]
  writeBS "UserID\tName\tAccepted\tSubmissions\n"
  sequence_ [ writeBS $ B.fromString $ 
              printf "%s\t%s\t%d\t%d\n" (show uid) (show name) accepts submits
            | ((uid, probs), name) <- zip (Map.assocs stats) names,
              let accepts = Map.size $ Map.filter (any (==Accepted)) probs,
              let submits = sum (Map.elems $ Map.map length probs)
            ]
  where 
    lookup r txt = do m<-lookupByLogin r txt
                      case m of Nothing -> return txt
                                Just au -> return (userName au)
-}

    


-- in exam mode show final report before loggin out
-- otherwise, logout immediately
handleConfirmLogout :: AppHandler ()
handleConfirmLogout = method GET $ do
  uid <- require getUserID <|> badRequest
  probset <- getProblemSet
  handleFinalReport uid probset


handleFinalReport :: UID -> ProblemSet -> AppHandler ()
handleFinalReport uid ProblemSet{..} | probsetExam = do
    let pids = map probID probsetProbs
    subs <- mapM (getBestSubmission uid) pids
    let psubs = [(p,s) | (p, Just s)<-zip probsetProbs subs]
    renderWithSplices "finalrep" $
      "problem_list" ## I.mapSplices (I.runChildrenWith . splices) psubs
  where splices (prob,sub) = problemSplices prob >> submissionSplices sub

-- not in exam mode: proceed to logout immediately
handleFinalReport _ _ = handleLogout



------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, AppHandler ())]
routes = [ ("/login",                 handleLogin `catch` internalError)
         , ("/logout",                handleLogout `catch` internalError)
         , ("/problems",              handleProblemList `catch` internalError )  
         , ("/problems/:pid",         handleProblem `catch` internalError )
         , ("/submissions/:pid",      handlePostSubmission `catch` internalError)
         , ("/submissions/:pid/:sid", handleGetSubmission  `catch` internalError)
         , ("/asklogout",             handleConfirmLogout `catch` internalError)        
         , ("/resources",             serveDirectory "resources" <|> notFound)
         , ("",                       serveDirectory "static" <|> notFound)
         ]
{-
  where logRequest (path,action) = (path, do req<-getRequest
					     logError $ B.fromString $ show req
					     action)
-}


-- | current logged in full user name  
loggedInName :: SnapletLens b (AuthManager b) -> SnapletISplice b
loggedInName authmgr = do
    u <- lift $ withTop authmgr currentUser
    maybe (return []) (I.textSplice . userName) u 


-- | splice for current date & time
nowSplice :: I.Splice AppHandler
nowSplice = do t <- liftIO (getCurrentTime >>= utcToLocalZonedTime)
               I.textSplice (T.pack $ formatTime defaultTimeLocale "%c" t)



versionSplice :: I.Splice AppHandler
versionSplice = I.textSplice (T.pack (showVersion version))

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit Pythondo Pythondo
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
    addConfig h emptyConfig { hcInterpretedSplices = do 
                                 "version" ## versionSplice
                                 "timeNow" ## nowSplice
                                 "loggedInName" ## loggedInName auth }
    -- Grab the DB connection pool from the sqlite snaplet and call
    -- into the Model to create all the DB tables if necessary.
    let c = sqliteConn $ d ^# snapletValue
    liftIO $ withMVar c $
      \conn -> do Db.createTables conn
                  -- putStrLn "Updating problem tags db table"
                  -- probs <- readProblemDir >>= mapM readProblem
                  -- Db.updateProblems conn probs
    addRoutes routes
      
    sandbox <- liftIO $ configSandbox conf
    printout <- liftIO $ configPrintout conf
    ldapConf <- liftIO $ configLdapConf conf
    return $ Pythondo { _heist = h
                      , _sess = s
                      , _auth = a
                      , _db   = d
                      -- , _config = conf
                      , _sandbox = sandbox
                      , _ldapConf = ldapConf
                      , _printout = printout
                      , _ekg = e
                      }


emptyConfig :: HeistConfig m
emptyConfig = HeistConfig noSplices noSplices noSplices noSplices []


-- initialize EKG server (if configured)
initEkg :: Config -> IO (Maybe Server)
initEkg conf = do enabled <- Configurator.require conf "ekg.enabled"
                  if enabled then 
                    do host <- Configurator.require conf "ekg.host"
                       port <- Configurator.require conf "ekg.port"
                       Just <$> forkServer host port                   
                    else return Nothing
  


-- | get the current problem set
-- TODO: avoid re-reading the same problems every time
-- with some caching mechanism
getProblemSet :: AppHandler ProblemSet
getProblemSet = liftIO (readProblemSet problemSetPath)

getProblem :: PID -> AppHandler Problem
getProblem pid = do
  probset <- getProblemSet
  case lookupProblemSet pid probset of
    Nothing -> badRequest
    Just p -> return p


problemSetPath :: FilePath
problemSetPath = "problems/index.md"


lookupProblemSet :: PID -> ProblemSet -> Maybe Problem
lookupProblemSet pid ProblemSet{..} =
  listToMaybe [p | p <- probsetProbs, probID p == pid] 
    

