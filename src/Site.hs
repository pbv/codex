{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

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
--import qualified Data.ByteString as B
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Map as Map

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

import           Data.Configurator
import           Data.Configurator.Types

import           System.Locale

import           System.Remote.Monitoring

------------------------------------------------------------------------------
import           Application
import           Db
import           Utils
import           Types
import           Problem
import           Submission
import           LdapAuth
import           Printout

  

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
  --optAuth <- withBackend (\r -> liftIO $ ldapAuth r ldapConf user passwd)
  optAuth <- withBackend (\r -> liftIO $ dummyAuth r ldapConf user passwd)
  case optAuth of 
    Nothing -> handleLoginForm err
    Just au -> forceLogin au >> redirect "/problems"
  where 
    err = Just "Utilizador ou password incorreto"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: AppHandler ()
handleLogout = method GET $ do 
    uid <- getLoggedUser   --  ensure user is logged in   
    -- procedeed to printout in exam mode
    exam <- getConfigured "exam" False
    when exam $ handlePrintout uid
    with auth logout 
    redirect "/" 


{-
------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
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
  uid <- getLoggedUser -- ensure a user is logged in
  pid <- PID <$> getRequiredParam "pid"
  prob <- liftIO $ getProblem pid
  -- check if problem is visible; deny request if not
  now <- liftIO getCurrentTime
  when (not $ isVisible now prob) badRequest
  -- now list all submissions
  subs <- getSubmissions uid (probID prob)
  renderWithSplices "problem" $ 
    do problemSplices prob 
       submissionsSplice subs 
       numberSplices (length subs) 
       acceptedSplice (any isAccepted subs) 


-- | problem listing handler
handleProblemList :: AppHandler ()
handleProblemList = method GET $ do
  uid <- getLoggedUser
  -- get submissions summary for the user logged in 
  summary <- getSubmissionsSummary uid 
  -- list currently visible problems 
  now <- liftIO getCurrentTime  
  probs <- filter (isVisible now) <$> liftIO getProblems
  -- filter results for visible problems only
  let list = [(p, sub, acc) | p<-probs,
              let (sub, acc) = head ([(sub',acc') | (pid,sub',acc')<-summary,
                                      pid == probID p] ++ [(0,0)])
              ]
  -- construct splices and render page
  renderWithSplices "problemlist" $ do
    "problemList" ##  I.mapSplices (I.runChildrenWith . splices) list
  where splices (prob,sub,acc) 
          = do problemSplices prob 
               numberSplices sub
               acceptedSplice (acc>0) 



problemSplices :: Problem UTCTime -> AppSplices
problemSplices p = do
  "problemID" ## I.textSplice (T.pack $ show $ probID p)
  "problemTitle" ## I.textSplice (probTitle p)
  "description" ## return (probDescr p)
  "submitText" ## I.textSplice (probSubmit p)
  "startTime" ##  maybe (return []) timeSplice (probStart p)
  "endTime" ##  maybe (return []) timeSplice (probEnd p)
  "ifAvailable" ## do t <- liftIO getCurrentTime
                      conditionalSplice (isAvailable t p)
  "ifEarly" ## do t<-liftIO getCurrentTime; conditionalSplice (isEarly t p)
  "ifLate" ## do t<-liftIO getCurrentTime; conditionalSplice (isLate t p)
  "ifLimited" ## conditionalSplice (isJust $ probEnd p)
  "timeLeft"  ## case probEnd p of 
    Nothing -> I.textSplice "N/A"
    Just t' -> do t <- liftIO getCurrentTime
                  I.textSplice $ T.pack $ formatNominalDiffTime $ diffUTCTime t' t

    
-- convert UTC time to local time zone
timeSplice :: UTCTime -> I.Splice AppHandler
timeSplice t = do tz <- liftIO getCurrentTimeZone
                  I.textSplice $ T.pack $ 
                    formatTime defaultTimeLocale "%c" $ 
                    utcToZonedTime tz t
    

-- format a time difference
formatNominalDiffTime :: NominalDiffTime -> String
formatNominalDiffTime secs 
  | secs>=0  = unwords $ 
               [show d ++ "d" | d>0] ++ 
               [show (h`rem`24) ++ "h" | h>0] ++
               [show (m`rem`60) ++ "m" | m>0] ++
               ["<1m" | secs<60]
  | otherwise = "--/--"
  where m = round (secs / 60) :: Int
        h = (m `div` 60) 
        d = (h `div` 24)  
        


submitSplices :: Submission -> AppSplices
submitSplices s = do 
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

     
submissionsSplice :: [Submission] -> AppSplices
submissionsSplice lst 
  = "submissions" ## I.mapSplices (I.runChildrenWith . submitSplices) lst

acceptedSplice :: Bool -> AppSplices
acceptedSplice b = "ifAccepted" ## conditionalSplice b


               

-- splices concerning the number of submissions
numberSplices :: Int -> AppSplices
numberSplices n = do 
  "countSubmissions" ## I.textSplice (T.pack $ show n)
  "ifSubmissions" ## conditionalSplice (n>0)



handleGetSubmission, handlePostSubmission :: AppHandler ()
handleGetSubmission = method GET $ do
  uid <- getLoggedUser
  pid <- PID <$> getRequiredParam "pid"
  sid <- read . B.toString <$> getRequiredParam "sid"
  prob <- liftIO $ getProblem pid
  sub <- getSubmission uid sid
  renderWithSplices "report" $ do problemSplices prob  
                                  submitSplices sub 


handlePostSubmission = method POST $ do
  uid <- getLoggedUser
  pid <-  PID <$> getRequiredParam "pid"
  prob <- liftIO $ getProblem pid
  -- check that the problem is available for submission
  now <- liftIO getCurrentTime
  when (not $ isAvailable now prob) badRequest
  incrCounter "submissions"
  code <- T.decodeUtf8With T.ignore <$> getRequiredParam "code"
  sub <- postSubmission uid prob code
  renderWithSplices "report" $ do problemSplices prob 
                                  submitSplices sub 


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
handleConfirmLogout = do handleLogout
  --exam <- getConfigured "exam" False
  --if exam then handleFinalReport else handleLogout

{-
handleFinalReport :: AppHandler ()
handleFinalReport = method GET $ do
    uid <- getLoggedUser  
    probs <- liftIO getProblems
    subs <- sequence [do subs<-getReports uid prob 
                         return (lastSubmission subs)
                     | prob<-probs]
    let psubs = [(p,s) | (p, Just s)<-zip probs subs]
    renderWithSplices "finalrep" $
      "problemList" ## I.mapSplices (I.runChildrenWith . splices) psubs
  where splices (prob,sub) = do problemSplices prob 
                                -- acceptedSplices (accepted sub)        
                                submitSplices sub
-}



------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, AppHandler ())]
routes = [ ("/login",                 handleLogin `catch` internalError)
         , ("/logout",                handleLogout `catch` internalError)
         -- , ("/register", with auth handleNewUser)
         , ("/problems/:pid",         handleProblem `catch` internalError )
         , ("/problems",              handleProblemList `catch` internalError)
         -- , ("/submissions",        handleSubmissions `catch` internalError)
         , ("/submissions/:pid",      handlePostSubmission `catch` internalError)
         , ("/submissions/:pid/:sid", handleGetSubmission  `catch` internalError)
         , ("/asklogout",             handleConfirmLogout `catch` internalError)        
         , ("",                       serveDirectory "static" <|> notFound)
         ]




-- | current logged in full user name  
loggedInName :: SnapletLens b (AuthManager b) -> SnapletISplice b
loggedInName authmgr = do
    u <- lift $ withTop authmgr currentUser
    maybe (return []) (I.textSplice . userName) u 


-- | splice for current date & time
nowSplice :: I.Splice AppHandler
nowSplice = do t <- liftIO (getCurrentTime >>= utcToLocalZonedTime)
               I.textSplice (T.pack $ formatTime defaultTimeLocale "%c" t)






------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = 
  makeSnaplet "pythondo" "Web system for learning Python programming." Nothing $ do
    conf <- getSnapletUserConfig
    ekg <- liftIO $ initEkg conf
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 7200)

    d <- nestSnaplet "db" db sqliteInit
    a <- nestSnaplet "auth" auth $ initSqliteAuth sess d
    addAuthSplices h auth
    addConfig h emptyConfig { hcInterpretedSplices = do 
                                 "timeNow" ## nowSplice
                                 "loggedInName" ## loggedInName auth }
      
      
    -- Grab the DB connection pool from the sqlite snaplet and call
    -- into the Model to create all the DB tables if necessary.
    let c = sqliteConn $ d ^# snapletValue
    liftIO $ withMVar c $ \conn -> Db.createTables conn

    addRoutes routes
    return $ App h s d a conf ekg 


emptyConfig :: HeistConfig m
emptyConfig = HeistConfig noSplices noSplices noSplices noSplices []


-- initialize EKG server (if configured)
initEkg :: Config -> IO (Maybe Server)
initEkg conf = do enabled <- require conf "ekg.enabled"
                  if enabled then 
                    do host <- require conf "ekg.host"
                       port <- require conf "ekg.port"
                       Just <$> forkServer host port                   
                    else return Nothing
  


