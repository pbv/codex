{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

------------------------------------------------------------------------------
-- | Administration facilities; file browsing

module Codex.AdminHandlers(
  handleBrowse,
  handleSubmission,
  handleSubmissionList,
  handleExport
  ) where

import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session                        (touchSession)
import qualified Snap.Snaplet.SqliteSimple                   as S
import           Snap.Util.FileServe hiding (mimeTypes)
import           Snap.Util.FileUploads
import           Heist
import           Heist.Splices     as I
import qualified Heist.Interpreted as I

import qualified Data.Map as Map
import           Data.Time.Clock
import           Data.Time.LocalTime
import           System.FilePath
import           System.Directory
import           System.IO
import           Control.Monad
import           Control.Monad.Trans (liftIO)
import           Control.Exception.Lifted (catch, SomeException)
import           Control.Applicative


import           Data.Text (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

import           Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as B
import qualified Data.ByteString  as B

import           Data.Maybe (fromMaybe,catMaybes)
import           Data.List (sort, intercalate)
import           Data.Map.Syntax

import qualified Data.Configurator as Configurator

import           Codex.Types
import           Codex.Page
import           Codex.Tester.Result
import           Codex.Application
import           Codex.Submission
import           Codex.Evaluate
import           Codex.AceEditor
import           Codex.Utils
import           Codex.Config


--
-- | Handle file browsing requests
--
handleBrowse :: Codex ()
handleBrowse = do
  -- ensure that a user with admin privileges is logged in
  usr <- require (with auth currentUser) <|> unauthorized
  unless (isAdmin usr) unauthorized
  with sess touchSession  -- refresh inactivity timeout
  root <- getDocumentRoot
  handleMethodOverride
    (method GET (handleGet root) <|>
     method POST (handleUpload root) <|>
     method PUT (handleEdit root) <|>
     method DELETE (handleDelete root) <|>
     method PATCH (handleRename root))


handleGet :: FilePath -> Codex ()
handleGet root = file <|> directory <|> notFound
  where
    directory = do
      rqpath <- getSafePath
      let filepath = root </> rqpath
      c <- liftIO $ doesDirectoryExist filepath
      unless c pass
      entries <- liftIO $ listDir filepath
      tz <- liftIO getCurrentTimeZone
      renderWithSplices "file-list" $ do
        pathSplices rqpath
        listingSplices tz rqpath entries
        messageSplices []
    file = do
      rqpath <- getSafePath
      let filepath = root </> rqpath
      c <- liftIO $ doesFileExist filepath
      unless c pass
      let mime = fileType mimeTypes rqpath
      contents <- if B.isPrefixOf "text" mime then
                    liftIO $ T.readFile filepath
                    else return ""
      let fileSplices = do
          "file-mime" ## I.textSplice (T.decodeUtf8 mime)
          "file-contents" ## I.textSplice contents
          "if-image-file" ## I.ifElseISplice (B.isPrefixOf "image" mime)
          "if-text-file" ## I.ifElseISplice (B.isPrefixOf "text" mime)
      renderWithSplices "file-edit" (pathSplices rqpath >>
                                     fileSplices >>
                                     inputAceEditorSplices)



listingSplices tz path list =
  "file-list" ## I.mapSplices (I.runChildrenWith . splices) list
  where splices (name, mime, time) = do
          pathSplices (path </> name)
          "file-name" ## I.textSplice (T.pack name)
          "file-type" ## I.textSplice (T.decodeUtf8 mime)
          "file-modified" ## utcTimeSplice tz time
          "if-text" ## ifElseISplice (B.isPrefixOf "text" mime)
          "if-dir" ## ifElseISplice (mime == "DIR")




pathSplices :: Monad m => FilePath -> Splices (I.Splice m)
pathSplices rqpath = do
  "file-path" ## I.textSplice (T.pack rqpath)
  "file-path-url" ## I.textSplice (T.decodeUtf8 $ encodePath rqpath)


listDir :: FilePath -> IO [(FilePath, ByteString, UTCTime)]
listDir root = do
  -- all entries in alphabetical order, directories first
  cnts <- getDirectoryContents root
  dirs  <- (sort . filter (/=".")) <$>
           filterM (doesDirectoryExist . (root</>)) cnts
  files <- sort <$> filterM (doesFileExist . (root</>)) cnts
  times <- forM dirs $ \path -> getModificationTime (root</>path)
  times'<- forM files $ \path -> getModificationTime (root</>path)
  return ([(path, "DIR", time) | (path,time)<-zip dirs times] ++
          [(path, mime, time) | (path,time)<-zip files times',
            let mime = fileType mimeTypes path])


handleEdit :: FilePath -> Codex ()
handleEdit root = do
  rqpath <- getSafePath
  contents <- require (getTextPost "editform.editor")
  liftIO $ T.writeFile (root</>rqpath) contents
  redirect (encodePath ("/files" </> rqpath))

{-
-- create files;  not yet enabled
handlePost ::  FilePath -> Codex ()
handlePost base = do
  c <- (read . B.toString) <$> require (getParam "newfile")
  if c then handleCreate base else handleUpload base

handleCreate :: FilePath -> Codex ()
handleCreate base = do
  rqpath <- getSafePath
  filename <- B.toString <$> require (getParam "filename")
  let path = base </> rqpath </> filename
  liftIO $ do
    c <- doesFileExist path
    if c then ioError (userError $ "file " ++ show path ++ " already exists")
      else T.writeFile path ""
  redirect (encodePath ("/files" </> rqpath))
-}

handleUpload :: FilePath -> Codex ()
handleUpload dest = do
  rqpath <- getSafePath
  c <- liftIO $ doesDirectoryExist (dest</>rqpath)
  unless c pass
  tmpdir <- liftIO getTemporaryDirectory
  msgs <- handleFileUploads tmpdir defaultUploadPolicy
    (const $ allowWithMaximumSize (getMaximumFormInputSize defaultUploadPolicy))
    (doUpload (dest </> rqpath))
  entries <- liftIO $ listDir (dest</>rqpath)
  tz <- liftIO getCurrentTimeZone
  renderWithSplices "file-list" (pathSplices rqpath >>
                                 listingSplices tz rqpath entries >>
                                 messageSplices (catMaybes msgs))


doUpload :: FilePath -> PartInfo ->
              Either PolicyViolationException FilePath -> IO (Maybe Text)
doUpload _ _ (Left e) =
  return (Just $ T.pack $ show e)
doUpload dest partinfo (Right src) = do
  let srcName = takeFileName src
  let destName = maybe srcName B.toString (partFileName partinfo)
  (copyFile src (dest</>destName) >> return Nothing) `catch`
    (\(e::SomeException) -> return (Just $ T.pack $ show e))



handleDelete :: FilePath -> Codex ()
handleDelete base = do
  rqpath <- getSafePath
  liftIO $ removeFile (base </> rqpath)
  let rqdir = takeDirectory rqpath
  redirect (encodePath ("/files" </> rqdir))


handleRename :: FilePath -> Codex ()
handleRename base = do
  rqpath <- getSafePath
  let rqdir = takeDirectory rqpath
  dest <- B.toString <$> require (getParam "destname")
  let srcfile = base </> rqpath
  let destfile = base </> rqdir </> dest
  liftIO $ renameFile srcfile destfile
  redirect (encodePath ("/files" </> rqdir))



--
--  | Handle requests for submission listing
--
handleSubmissionList :: Codex ()
handleSubmissionList =  handleMethodOverride $ do
  usr <- require (with auth currentUser) <|> unauthorized
  unless (isAdmin usr) unauthorized
  with sess touchSession  -- refresh inactivity timeout
  patts <- getPatterns
  page <- fromMaybe 1 <$> readParam "page"
  sorting <- fromMaybe Asc <$> readParam "sorting"
  methods [GET,POST] (listSubmissions patts sorting page)
    <|>
   method PATCH (reevalSubmissions patts sorting >>
                 listSubmissions patts sorting page)
    <|>
    method (Method "CANCEL") (cancelPending >>
                              setPending [] >>
                              listSubmissions patts sorting page)
  
            


  

-- | List submissions
listSubmissions :: Patterns -> Sorting -> Int -> Codex ()
listSubmissions patts@Patterns{..} sorting page = do
  let patts' = normalizePatterns patts
  count <- countSubmissions patts'
  let entries = 50   -- # entries per page
  let npages
        | count>0 = ceiling (fromIntegral count / fromIntegral entries :: Double)
        | otherwise = 1
  -- restrict to visible pages
  let page' = 1 `max` page `min` npages
  let offset = (page' - 1) * entries  
  subs <- filterSubmissions patts' sorting entries offset
  tz <- liftIO getCurrentTimeZone
  renderWithSplices "submission-list" $ do
    "id_pat" ## I.textSplice idPat
    "uid_pat" ## I.textSplice userPat
    "path_pat" ## I.textSplice pathPat
    "lang_pat" ## I.textSplice langPat
    "class_pat" ## I.textSplice classPat
    "timing_pat" ## I.textSplice timingPat
    "page" ## I.textSplice (T.pack $ show page')
    "sorting" ## I.textSplice (T.pack $ show sorting)
    "submissions-count" ## I.textSplice (T.pack $ show count)
    "if-ascending" ## I.ifElseISplice (sorting == Asc)
    "if-submissions" ## I.ifElseISplice (count > 0)
    "page-count" ## I.textSplice (T.pack $ show npages)
    "prev-page" ## I.textSplice (T.pack $ show $ max 1 (page'-1))
    "next-page" ## I.textSplice (T.pack $ show $ min npages (page'+1))
    "submissions" ## I.mapSplices (I.runChildrenWith . submitSplices tz) subs
    let qs = Map.fromList $ map (\(k,v) -> (k,[T.encodeUtf8 v])) $
             [("id_pat", idPat),
              ("uid_pat", userPat),
              ("path_pat", pathPat),
              ("lang_pat", langPat),
              ("class_pat", classPat),
              ("timing_pat", timingPat)
             ]
    "patterns" ## I.textSplice (T.decodeUtf8 $ printUrlEncoded qs)


-- | Re-evaluate selected submissions 
reevalSubmissions :: Patterns -> Sorting -> Codex ()
reevalSubmissions patts@Patterns{..} sorting  = do
  let patts' = normalizePatterns patts
  count <- countSubmissions patts'
  subs  <- filterSubmissions patts' sorting count 0
  liftIO $ putStrLn "marking submission state"
  sqlite <- S.getSqliteState
  liftIO $ markEvaluating sqlite (map submitId subs)
  liftIO $ putStrLn "canceling previous pending evaluations"
  cancelPending
  tids <- mapM evaluate subs
  setPending tids



-- | Export all submissions
handleExport :: Codex ()
handleExport = method POST $ do
  au <- require (with auth currentUser) <|> unauthorized
  unless (isAdmin au) unauthorized
  sep <- B.toString <$> require (getParam "sep")
  serveFile =<< exportSubmissions "export.txt" sep



-- | Create a CSV text file with submission listsing
-- NB: the caller should remove the temporary file
exportSubmissions :: FilePath -> String -> Codex FilePath
exportSubmissions filetpl sep  = do
  tmpDir <- liftIO getTemporaryDirectory
  (tmpPath, handle) <-
    liftIO $ openTempFileWithDefaultPermissions tmpDir filetpl
  liftIO (hPutStrLn handle header)
  withSubmissions () (output handle)
  liftIO (hClose handle)
  return tmpPath
  where
    header = intercalate sep ["id", "user_id", "path", "language",
                              "classify", "timing", "received"]
    output :: Handle -> () -> Submission -> IO ()
    output h _ Submission{..} = do
      let row = intercalate sep [show submitId,
                                 show submitUser,
                                 show submitPath,
                                 show (codeLang submitCode),
                                 show (resultClassify submitResult),
                                 show submitTiming,
                                 show (show submitTime)
                                ]
      hPutStrLn h row




-- | Handle admin requests for a single submission
handleSubmission :: Codex ()
handleSubmission = handleMethodOverride $ do
    usr <- require (with auth currentUser) <|> unauthorized
    unless (isAdmin usr) unauthorized
    sid <- require getSubmitId
    sub <- require (getSubmission sid) <|> notFound
    method GET (report sub) <|>
      method PATCH (reevaluate sub) <|>
      method DELETE (delete sub)
  where
    -- get report on a submission
    report sub = do
      root <- getDocumentRoot
      page <- liftIO $ readPage root (submitPath sub)
      tz <- liftIO getCurrentTimeZone
      renderWithSplices "submission" $ do
        pageSplices page
        submitSplices tz sub

    -- delete a submission
    delete sub = do
      deleteSubmission (submitId sub)
      redirect (encodePath ("/pub" </> submitPath sub))

    -- revaluate a single submission
    reevaluate sub = do
      let sid = submitId sub
      sqlite <- S.getSqliteState
      liftIO $ markEvaluating sqlite [sid]
      evaluate sub
      redirect (encodePath ("/submissions" </> show sid))


