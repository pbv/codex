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

import           Control.Exception (SomeException)
import           Control.Exception.Lifted (catch)
import           Control.Monad.State
import           Control.Monad.Trans.Except
import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Lens

import           Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as B
import qualified Data.ByteString      as B
import           Data.Maybe(isJust, fromMaybe)

import           Data.List (nub, sort)
import           Data.Map.Syntax
import qualified Data.HashMap.Strict as HM

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.SqliteSimple
import qualified Snap.Snaplet.SqliteSimple as S
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe (getSafePath, fileType,
                                      serveFile, serveFileAs, serveDirectory)
import           Heist
import           Heist.Splices     as I
import qualified Heist.Interpreted as I

import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Aeson

import           System.FilePath
import           System.Directory (doesFileExist, doesDirectoryExist)


------------------------------------------------------------------------------
import           Application
import           Db
import           Utils
import           Types
import           Tester
import           Language
import           Markdown
import           Interval
import           Page  
-- import qualified Page
-- import           Submission (Submission(..), Patterns(..), Sorting(..))
-- import qualified Submission
import           Submission
import           LdapAuth
import           Config
import           Admin

import           Paths_codex(version)
import           Data.Version (showVersion)  

import           AceEditor

import           Text.Pandoc hiding (Code)
import           Text.Pandoc.Walk as Pandoc

-- interpreted splices for handlers
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
  login <-  require (getParam "login")
  passwd <- require (getParam "password") 
  ldap <- getLdap
  r <- with auth $ loginByUsername (T.decodeUtf8 login) (ClearText passwd) False
  case r of
    Right au -> redirect "/"
    Left err -> case ldap of
      Nothing -> handleLoginForm "login" (Just err)
      Just cfg -> loginLdapUser cfg login passwd


loginLdapUser :: LdapConf -> ByteString -> ByteString -> Codex ()
loginLdapUser ldapConf login passwd = do
  r <- with auth $ withBackend (\r -> liftIO $ ldapAuth r ldapConf login passwd)
  case r of
    Left err -> handleLoginForm "login" (Just err)
    Right au -> with auth (forceLogin au) >> redirect "/"
  
        

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
        Right au -> with auth (forceLogin au) >> redirect "/pub/index.md"


------------------------------------------------------------------------------
-- | Register a new user 
--
newUser :: Handler b (AuthManager b) (Either AuthFailure AuthUser)
newUser = do
    l <- fmap T.decodeUtf8 <$> getParam "login"
    p <- getParam "password"
    p2<- getParam "password2"
    n <- fmap T.decodeUtf8 <$> getParam "fullname"
    e <- fmap T.decodeUtf8 <$> getParam "email"
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
-- | Logs out and redirects the user to the site index.
-- in exam mode procedeed to printout 
handleLogout :: Codex ()
handleLogout = method GET $ do
  uid <- require getUserID <|> unauthorized
  with auth logout 
  redirect "/" 


-- | handle page requests
handlePage :: Codex ()
handlePage = do
  uid <- require getUserID <|> unauthorized
  rqpath <- getSafePath
  (method GET (handleGet uid rqpath <|> handleRedir rqpath)
   <|>
   method POST (handlePost uid rqpath))
  where
    handleGet uid rqpath =  do
      let filepath = publicPath </> rqpath
      c <- liftIO (doesFileExist filepath)
      when (not c) pass
      -- serve according to mime type
      let mime = fileType mimeTypes rqpath
      if mime == "text/markdown" then
        servePage uid rqpath
        else
        serveFileAs mime filepath
    -- handle redirects
    handleRedir rqpath = do
        let filepath = publicPath </> rqpath </> "index.md"
        c <- liftIO (doesFileExist filepath)
        if c then 
          redirect (encodePath ("/pub" </> rqpath </> "index.md"))
          else
          notFound
    -- handle POST requests; only for exercise pages
    handlePost uid rqpath = do
      let filepath = publicPath </> rqpath
      c <- liftIO $ doesFileExist filepath
      guard (c && fileType mimeTypes rqpath == "text/markdown")
      page <- liftIO (readPage publicPath rqpath)
      guard (pageIsExercise page)
      text <- require (getTextPost "editform.editor")
      lang <- require (return $ pageLanguage page)
      sub <- evaluate uid page (Code lang text)
      -- tz <- liftIO getCurrentTimeZone
      t <- liftIO getZonedTime
      let tz = zonedTimeZone t
      renderWithSplices "report" $ do 
        pageSplices page
        exerciseSplices t page 
        submitSplices tz sub 
        inputAceEditorSplices

        
-- | serve a markdown document 
servePage :: UserID -> FilePath -> Codex ()
servePage uid rqpath = do
  page <- readPageLinks uid rqpath
  if pageIsExercise page then do
    renderExercise page =<< getPageSubmissions uid rqpath
    else
    renderWithSplices "page" (pageSplices page)
  

--  render an exercise page
renderExercise :: Page -> [Submission] -> Codex ()
renderExercise page subs = do
  -- tz <- liftIO getCurrentTimeZone
  t <- liftIO getZonedTime
  let tz = zonedTimeZone t
  renderWithSplices "exercise" $ do
    pageSplices page
    exerciseSplices t page
    submissionListSplices tz subs 
    inputAceEditorSplices



-- | read a page, collect exercise links
--  and patch with titles of linked pages
readPageLinks :: UserID -> FilePath -> Codex Page
readPageLinks uid rqpath = do
  page <- liftIO $ readPage publicPath rqpath
  let links = queryExerciseLinks page
  let dir = takeDirectory rqpath
  -- fetch linked titles 
  optTitles <- liftIO $
               forM links $ \url ->
               (pageTitle <$> readPage publicPath (dir</>url))
               `catch` (\(_ :: SomeException) -> return Nothing)
               
  let titles = HM.fromList [(url,title) | (url, Just title)<-zip links optTitles]
  --    
  -- fetch submisssion count
  submissions <- HM.fromList <$>
                 (forM links $ \url -> do
                     count <- length <$> getPageSubmissions uid (dir</>url)
                     return (url, count))
  --
  -- patch relevant links
  let patch elm@(Link attr@(_, classes, _) inlines target@(url, _))
        | "ex" `elem` classes =
             let title = if null inlines then (HM.lookupDefault [] url titles)
                         else inlines
                 count =  HM.lookupDefault 0 url submissions
             in (Span nullAttr
                     [Link attr title target,
                      LineBreak,
                      Span ("",["info"],[])
                      [Str "(", Str (show count), Space,
                       Str "submissions", Str ")"]
                      ])
        | otherwise = elm
      patch elm = elm
  --
  return page { pageDescription = walk patch (pageDescription page) }

  


-- | collect all exercise links from a page
queryExerciseLinks :: Page -> [String]
queryExerciseLinks page = query extractURL (pageDescription page)
  where
    extractURL (Link attr@(_,classes,_) inlines target@(url,_))  
      = [url | "ex" `elem` classes]
    extractURL _  = []




{-        
renderIndex uid page links = do
  querytags <- getQueryTags
  pages <- liftIO $ mapM (Page.readPage publicPath) links
  let alltags = nub $ sort $ concatMap Page.getTags pages
  let pages' = filter (\p -> querytags `contained` Page.getTags p) pages
  let abletags = nub $ sort $ concatMap Page.getTags pages'
  subs <- mapM (Submission.getReport uid . Page.path) pages'
  tz <- liftIO getCurrentTimeZone
  renderWithSplices "indexsheet" $ do
    pageSplices page 
    indexSplices tz (zip pages' subs)
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
-}





-- handle request for submission lists
handleSubmissionList :: Codex ()    
handleSubmissionList = do
  au <- require (with auth currentUser) <|> unauthorized
  when (not $ isAdmin au) unauthorized
  method GET handleGet <|> method POST handlePost
  where
    handleGet =  listSubmissions emptyPatterns Asc Nothing
    handlePost = do
      optPage <- readParam "page"
      sorting <- require (readParam "sorting")
      id_pat <- T.decodeUtf8 <$> require (getParam "id_pat")
      uid_pat <- T.decodeUtf8 <$> require (getParam "uid_pat")
      path_pat<- T.decodeUtf8 <$> require (getParam "path_pat")
      lang_pat<- T.decodeUtf8 <$> require (getParam "lang_pat")
      class_pat<- T.decodeUtf8 <$> require (getParam "class_pat")
      timing_pat<- T.decodeUtf8 <$> require (getParam "timing_pat")
      let patts = Patterns { idPat = id_pat, userPat = uid_pat,
                             pathPat = path_pat, langPat = lang_pat,
                             classPat = class_pat, timingPat = timing_pat }
      listSubmissions patts sorting optPage


listSubmissions :: Patterns -> Sorting -> Maybe Int -> Codex ()
listSubmissions patts@Patterns{..} sort optPage = do
  count <- countSubmissions patts
  let entries = 50   -- # entries per page 
  let npages
        | count>0 = ceiling (fromIntegral count / fromIntegral entries :: Double)
        | otherwise = 1
  let page = 1 `max` fromMaybe 1 optPage `min` npages
  let offset = (page - 1) * entries
  subs <- filterSubmissions patts sort entries offset
  tz <- liftIO getCurrentTimeZone
  renderWithSplices "submission-list" $ do
    "id_pat" ## I.textSplice idPat
    "uid_pat" ## I.textSplice userPat
    "path_pat" ## I.textSplice pathPat
    "lang_pat" ## I.textSplice langPat
    "class_pat" ## I.textSplice classPat
    "timing_pat" ## I.textSplice timingPat
    "page" ## I.textSplice (T.pack $ show page)
    "submissions-count" ## I.textSplice (T.pack $ show count)
    "if-ascending" ## I.ifElseISplice (sort == Asc)
    "if-submissions" ## I.ifElseISplice (count > 0)
    "page-count" ## I.textSplice (T.pack $ show npages)
    "submissions" ## I.mapSplices (I.runChildrenWith . submitSplices tz) subs


-- | handle requests for a single submission
handleSubmission :: Codex ()
handleSubmission
  = handleMethodOverride (method GET handleGet <|>
                          method DELETE handleDelete)
  where handleGet = do
          au <- require (with auth currentUser) <|> unauthorized
          let uid = authUserID au
          sid <- require getSubmitID
          sub <- require (getSubmission sid) <|> notFound
          when (not (isAdmin au) && submitUser sub /= uid)
            unauthorized
          page <- liftIO $ readPage publicPath (submitPath sub)
          t <- liftIO getZonedTime
          let tz = zonedTimeZone t
          renderWithSplices "report" $ do
            pageSplices page
            exerciseSplices t page
            submitSplices tz sub 
            inputAceEditorSplices
        handleDelete = do
          au <- require (with auth currentUser) <|> unauthorized
          let uid = authUserID au
          sid <- require getSubmitID
          sub <- require (getSubmission sid) <|> notFound
          when (not (isAdmin au) && submitUser sub /= uid)
            unauthorized
          deleteSubmission sid
          redirect (encodePath ("/pub" </> submitPath sub))


handleExport :: Codex ()
handleExport = method POST $ do
  au <- require (with auth currentUser) <|> unauthorized
  when (not (isAdmin au)) unauthorized
  sep <- B.toString <$> require (getParam "sep")
  serveFile =<< exportSubmissions "export.txt" sep
  


-- | splices related to a page
pageSplices :: Page -> ISplices
pageSplices page = do
  let dir = takeDirectory $ pagePath page
  "file-path" ## I.textSplice (T.pack $ pagePath page)      
  "file-path-url" ## I.textSplice (T.decodeUtf8 $ encodePath $ pagePath page)
  "page-description" ## return (blocksToHtml $ pageDescription page)
  "if-exercise" ## I.ifElseISplice (pageIsExercise page)


-- | splices related to exercises
exerciseSplices :: ZonedTime -> Page -> ISplices
exerciseSplices t page = do
  -- splices related to programming languages
  "language" ##
    maybe (return []) (I.textSplice . fromLanguage) (pageLanguage page)
  "language-mode" ##
    maybe (return []) (I.textSplice . languageMode) (pageLanguage page)
  "code-text" ##
    maybe (return []) I.textSplice (pageCodeText page)
  --- splices related to submission intervals 
  let interval = fromMaybe Always $ pageValid t page 
  "valid-from" ##
    maybe (I.textSplice "N/A") timeExprSplice  (Interval.from interval)
  "valid-until" ##
    maybe (I.textSplice "N/A") timeExprSplice (Interval.until interval)
  "case-timing" ##
    timingSplice interval


{-
indexSplices :: TimeZone -> [(Page, [Submission])] -> ISplices
indexSplices tz pageSubs = do
  "index-list" ##
    I.mapSplices (I.runChildrenWith . auxSplices) pageSubs
  where
    auxSplices (page,subs) = pageSplices page >> submissionListSplices tz subs
-}

-- | splices relating to a single submission
submitSplices :: TimeZone -> Submission -> ISplices
submitSplices tz Submission{..} = do
  "submit-id" ##  I.textSplice (toText submitID)
  "submit-user-id" ## I.textSplice (toText submitUser)
  "submit-path" ## I.textSplice (T.decodeUtf8 $ encodePath submitPath)
  "received" ## utcTimeSplice tz submitTime
  "code-lang" ## I.textSplice (fromLanguage $ codeLang submitCode)
  "code-text" ##  I.textSplice (codeText submitCode)
  "classify" ##  I.textSplice (T.pack $ show $ resultClassify submitResult)
  "message" ## I.textSplice (resultMessage submitResult)
  "case-timing" ## caseSplice submitTiming
  "timing" ## I.textSplice (T.pack $ show submitTiming)
  "valid" ## I.ifElseISplice (submitTiming == Valid)
  "early" ## I.ifElseISplice (submitTiming == Early)
  "overdue" ## I.ifElseISplice (submitTiming == Overdue)
  "accepted" ## I.ifElseISplice (resultClassify submitResult == Accepted)



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

  


------------------------------------------------------------------------------
-- | The application's routes.

routes :: [(ByteString, Codex ())]
routes = [ ("/login",    handleLogin `catch` internalError)
         , ("/logout",   handleLogout `catch` internalError)
         , ("/register", handleRegister `catch` internalError)
         , ("/pub",      handlePage `catch` internalError)
         , ("/submited/:sid", handleSubmission `catch` internalError)
         , ("/submited",  handleSubmissionList `catch` internalError)
         , ("/files",  handleBrowse publicPath `catch`  internalError)
         , ("/export", handleExport `catch` internalError)
         , ("/static",   serveDirectory staticPath <|> notFound) 
         ]



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


-- | evaluate a submission
-- run code tester and record submission into Db
evaluate :: UserID -> Page -> Code -> Codex Submission
evaluate uid page code = do 
  now <- liftIO getZonedTime
  let utc = zonedTimeToUTC now
  env <- require getUserEvents
  let interval = fromMaybe Always $ pageValid now page
  let r = Interval.evalI env interval utc
  case r of
    Right timing -> do
      result <- codeTester page code
      insertSubmission uid (pagePath page) utc code result timing
    Left msg -> 
      error (T.unpack msg)
      -- insertSubmission uid (pagePath page) utc code (miscError msg) Valid





------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = 
  makeSnaplet "codex" "Web server for programming exercises." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" Nothing (Just 3600)

    d <- nestSnaplet "db" db S.sqliteInit
    a <- nestSnaplet "auth" auth $ initSqliteAuth sess d
    addAuthSplices h auth
    let sc = do 
          "version" ## versionSplice
          "timeNow" ## nowSplice
          "loggedInName" ## loggedInName auth
          "ifAdmin" ## do
            mbAu <- lift (withTop auth currentUser)
            I.ifElseISplice (maybe False isAdmin mbAu)
          
    addConfig h (mempty & scInterpretedSplices .~ sc)
    -- Grab the DB connection pool from the sqlite snaplet and call
    -- into the Model to create all the DB tables if necessary.
    let c = S.sqliteConn $ d ^# snapletValue
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




