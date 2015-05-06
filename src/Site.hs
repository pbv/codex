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
-- import qualified Data.ByteString as B
-- import           Data.Maybe
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
import           SubmitSummary
import           LdapAuth
import           Printout

import           Paths_pythondo(version)
import           Data.Version (showVersion)  

-- my application splices
type AppSplices = Splices (I.Splice AppHandler)



------------------------------------------------------------------------------
-- | Handle login requests 
handleLogin :: AppHandler ()
handleLogin 
  = method GET (with auth $ handleLoginForm Nothing) <|>
    method POST (do { user <- getRequiredParam "login"
                    ; passwd <- getRequiredParam "password"
                    ; ldapconf <- getLdapConf
                    ; with auth $ handleLoginSubmit ldapconf user passwd
                    })


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
--  n exam mode procedeed to printout 
handleLogout :: AppHandler ()
handleLogout = method GET $ do 
    exam <- getConfigExam
    when exam handlePrintout 
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
{-
handleProblem = method GET $ do
  uid <- getLoggedUser -- ensure a user is logged in
  pid <- PID <$> getRequiredParam "pid"
  prob <- liftIO $ readProblem pid
  now <- liftIO getCurrentTime
  subs <- getSubmissions uid pid
  exam <- getConfigured "exam" False
  -- if running in exam mode, check that the problem is available 
  when (exam && not (now `Interval.elem` probOpen prob) && null subs)
       badRequest
  -- now list all submissions
  renderWithSplices "problem" $ do problemSplices prob 
                                   submissionsSplice subs
-}

handleProblem = method GET $ do
    uid <- require getUserID    <|> unauthorized
    pid <- require getProblemID 
    prob <- liftIO $ readProblem pid
    now <- liftIO getCurrentTime
    subs <- getSubmissions uid pid
    -- in exam mode check that the problem is available
    exam <- getConfigExam
    when (exam && now `Interval.notElem` probOpen prob && null subs) 
         badRequest
    renderWithSplices "problem" $ do problemSplices prob
                                     submissionsSplice subs
                                     timerSplices now (probOpen prob)

        


-- | problem listing handler
handleProblemList :: AppHandler ()
handleProblemList = method GET $ do
  uid <- require getUserID <|> unauthorized
  tags <- getQueryTags
  now <- liftIO getCurrentTime
  exam <- getConfigExam
  -- summary of all available problems
  available <- if exam then
                  filter (submitAvailable now) <$> getSubmitSummary uid
               else getSubmitSummary uid
  -- filter by query tags
  let visible = filter (\s-> tags `isSublistOf` summaryTags s) available
      
  -- context-dependent splices for tags 
  let tagSplices tag = 
        let checked = tag`elem`tags
            disabled= null (filter (\s->tag `elem` summaryTags s) visible)
        in do "tagText" ## I.textSplice tag
              "tagCheckbox" ## return (checkboxInput tag checked disabled)

  -- render page
  renderWithSplices "problemlist" $ do
    "tagList" ## I.mapSplices (I.runChildrenWith . tagSplices) (allTags available)
    "problemList" ##  I.mapSplices (I.runChildrenWith . summarySplices now) visible
    "availableProblems" ## I.textSplice (T.pack $ show $ length available)
    "visibleProblems" ## I.textSplice (T.pack $ show $ length visible)
               
               
-- get tag list from query string
getQueryTags :: AppHandler [ProblemTag]
getQueryTags = do
  params <- getParams
  return (map (T.pack . B.toString) $ Map.findWithDefault [] "tag" params)



summarySplices :: UTCTime -> SubmitSummary -> AppSplices
summarySplices now SubmitSummary{..}
    = do problemSplices summaryProb
         counterSplices summarySubmits
         timerSplices now (probOpen summaryProb)
         "ifAccepted" ## conditionalSplice (summaryAccepted > 0) 



-- | splices related to a single problem       
problemSplices :: Problem -> AppSplices
problemSplices prob@Problem{..} = do
  "probID" ## I.textSplice (T.pack $ show probID)
  "probTitle" ## maybe (return []) I.textSplice probTitle
  "probDoc" ## return (renderProblem prob)
  "probDefault" ## maybe (return []) I.textSplice probDefault
  "probTags" ## I.textSplice (T.unwords probTags)


-- | splices related to a timer interval
timerSplices ::  UTCTime -> Interval UTCTime -> AppSplices
timerSplices now interval = do
  "ifOpen" ## conditionalSplice (now `Interval.elem` interval)
  "ifEarly" ## conditionalSplice (now `Interval.before`interval)
  "ifLate" ##  conditionalSplice (now `Interval.after`interval)
  "ifLimited" ## conditionalSplice (Interval.limited interval)
  "timerStart" ##  maybe (return []) timeSplice (Interval.start interval)
  "timerEnd" ##  maybe (return []) timeSplice (Interval.end interval)
  "timerLeft"  ## case Interval.end interval of 
    Nothing -> return []
    Just t' -> I.textSplice $ T.pack $ formatNominalDiffTime $ diffUTCTime t' now

  
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
        
-- | splices relating to a single submission
submissionSplices :: Submission -> AppSplices
submissionSplices s = do 
  "submitID"   ## I.textSplice (T.pack $ show $ submitID s)
  "submitPID"  ## I.textSplice (T.pack $ show $ submitPID s)
  "submitTime" ## timeSplice (submitTime s)
  "submitText" ## I.textSplice (submitText s)
  "submitStatus" ## I.textSplice (T.pack $ show r)
  "submitReport" ## I.textSplice (submitReport s)
  "ifAccepted" ## conditionalSplice (r == Accepted)
  "ifOverdue" ##  conditionalSplice (r == Overdue)
  "ifRejected" ## conditionalSplice (r/= Accepted && r/=Overdue)
  where r = submitStatus s

-- splices relating to a list of submissions
submissionsSplice :: [Submission] -> AppSplices
submissionsSplice lst = do
  "submissions" ## I.mapSplices (I.runChildrenWith . submissionSplices) lst
  "ifAccepted" ## conditionalSplice (any isAccepted lst)
  counterSplices (length lst)


-- | splices concerning submissions count
counterSplices :: Int -> AppSplices
counterSplices n = do 
  "count" ## I.textSplice (T.pack $ show n)
  "ifSubmitted" ## conditionalSplice (n>0)




handleGetSubmission, handlePostSubmission :: AppHandler ()
handleGetSubmission 
    = method GET $ do 
        uid <- require getUserID <|> unauthorized
        pid <- require getProblemID 
        sid <- require getSubmissionID 
        prob <- liftIO $ readProblem pid
        sub <- getSubmission uid pid sid
        renderWithSplices "report" $ do problemSplices prob  
                                        submissionSplices sub


handlePostSubmission = method POST $ do
  uid <- require getUserID <|> unauthorized
  pid <- require getProblemID 
  prob <- liftIO $ readProblem pid
  now <- liftIO getCurrentTime
  -- if running in exam mode, check that the problem is available for submission
  pids <- getSubmittedPIDs uid
  exam <- getConfigExam
  when (exam && now `Interval.notElem` probOpen prob && pid `notElem` pids) 
       badRequest
  incrCounter "submissions"
  code <- T.decodeUtf8With T.ignore <$> getRequiredParam "code"
  sub <- postSubmission uid prob code
  renderWithSplices "report" $ do problemSplices prob 
                                  submissionSplices sub 
                                  timerSplices  now (probOpen prob)


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

    


-- in exam mode show final report and confirm logout
-- otherwise, logout immediately
handleConfirmLogout :: AppHandler ()
handleConfirmLogout 
    = do exam <- getConfigExam
         if exam then handleFinalReport else handleLogout


handleFinalReport :: AppHandler ()
handleFinalReport = method GET $ do
    uid <- require getUserID <|> unauthorized
    pids <- getSubmittedPIDs uid
    probs <- liftIO $ mapM readProblem pids
    subs <- mapM (getBestSubmission uid) pids
    let psubs = [(p,s) | (p, Just s)<-zip probs subs]
    renderWithSplices "finalrep" $
      "problemList" ## I.mapSplices (I.runChildrenWith . splices) psubs
  where splices (prob,sub) = do problemSplices prob 
                                submissionSplices sub




------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, AppHandler ())]
routes = [ ("/login",                 handleLogin `catch` internalError)
         , ("/logout",                handleLogout `catch` internalError)
         , ("/problems/:pid",         handleProblem `catch` internalError )
         , ("/problems",              handleProblemList `catch` internalError)
         , ("/submissions/:pid",      handlePostSubmission `catch` internalError)
         , ("/submissions/:pid/:sid", handleGetSubmission  `catch` internalError)
         , ("/asklogout",             handleConfirmLogout `catch` internalError)        
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
    return $ Pythondo { _heist = h
                      , _sess = s
                      , _auth = a
                      , _db   = d
                      , _config = conf
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
  


