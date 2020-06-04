{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

------------------------------------------------------------------------------
-- | Administration facilities; file browsing

module Codex.AdminHandlers(
  handleBrowse,
  handleSubmissionAdmin,
  handleSubmissionList
  ) where

 
import           Snap.Core hiding (path)
import           Snap.Snaplet.Heist
-- import qualified Snap.Snaplet.SqliteSimple                   as S
import           Snap.Snaplet.Router
import           Snap.Util.FileServe hiding (mimeTypes)
import           Snap.Util.FileUploads
import           Heist.Splices     as I
import qualified Heist.Interpreted as I

import           Data.Time.Clock
import           Data.Time.LocalTime
import           System.FilePath
import           System.Directory
import           System.IO
import           Control.Monad
import           Control.Monad.Trans (liftIO)
import           Control.Exception.Lifted (catch, IOException)
import           Control.Applicative


import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

import           Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as B
import qualified Data.ByteString  as B

import           Data.Maybe (fromMaybe,catMaybes)
import           Data.List (sort, intercalate)
import           Data.Map.Syntax


import           Codex.Types
import           Codex.Page
import           Codex.Tester.Result
import           Codex.Application
import           Codex.Submission
import           Codex.Evaluate
import           Codex.AceEditor
import           Codex.Utils
import           Codex.Config
import           Codex.Printout


--
-- | Handle file browsing requests
-- ensure that a user with admin privileges is logged in
--
handleBrowse :: FilePath -> Codex ()
handleBrowse rqpath = do
  requireAdmin
  handleMethodOverride
    (method GET (handleGet rqpath)  <|>
     method POST (handleUpload rqpath) <|>
     method PUT (handleEdit rqpath) <|>
     method DELETE (handleDelete rqpath) <|>
     method PATCH (handleRename rqpath))


handleGet :: FilePath -> Codex ()
handleGet rqpath = file <|> directory <|> notFound
  where
    directory = do
      root <- getDocumentRoot
      let filepath = root </> rqpath
      guardDirectoryExists filepath
      entries <- liftIO (listDir filepath)
      tz <- liftIO getCurrentTimeZone
      renderWithSplices "_file_list" $ do
        fileUrlSplices rqpath
        listingSplices tz rqpath entries
        messageSplices []
    file = do
      root <- getDocumentRoot
      let filepath = root </> rqpath
      guardFileExists filepath
      let mime = fileType mimeTypes rqpath
      contents <- if B.isPrefixOf "text" mime then
                    liftIO (T.readFile filepath)
                    else return ""
      let fileSplices =
            do "file-mime" ## I.textSplice (T.decodeUtf8 mime)
               "file-contents" ## I.textSplice contents
               "if-image-file" ## I.ifElseISplice (B.isPrefixOf "image" mime)
               "if-text-file" ## I.ifElseISplice (B.isPrefixOf "text" mime)
      renderWithSplices "_file_edit" (fileUrlSplices rqpath >>
                                      pageUrlSplices rqpath >>
                                      fileSplices >>
                                      textEditorSplice)


listingSplices ::
  TimeZone -> FilePath -> [(FilePath, ByteString, UTCTime)] -> ISplices
listingSplices tz path list =
  "file-list" ## I.mapSplices (I.runChildrenWith . splices) list
  where splices (name, mime, time) = do
          fileUrlSplices (path </> name)
          "file-name" ## I.textSplice (T.pack name)
          "file-type" ## I.textSplice (T.decodeUtf8 mime)
          "file-modified" ## localTimeSplice tz time
          "if-text" ## ifElseISplice (B.isPrefixOf "text" mime)
          "if-dir" ## ifElseISplice (mime == "DIR")



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
handleEdit rqpath = do
  root <- getDocumentRoot
  contents <- require (getTextParam "editform.text")
  liftIO $ T.writeFile (root</>rqpath) contents
  redirectURL (Files $ splitDirectories rqpath)

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
handleUpload rqpath = do
  --  liftIO $ putStrLn ("handleUpload " ++ show rqpath)
  root <- getDocumentRoot
  let filepath = root </> rqpath
  c <- liftIO $ doesDirectoryExist filepath
  unless c pass
  tmpdir <- liftIO getTemporaryDirectory
  msgs <- handleFileUploads tmpdir defaultUploadPolicy
    (const $ allowWithMaximumSize (getMaximumFormInputSize defaultUploadPolicy))
    (doUpload filepath)
  entries <- liftIO (listDir filepath)
  tz <- liftIO getCurrentTimeZone
  renderWithSplices "_file_list" (listingSplices tz rqpath entries >>
                                  messageSplices (catMaybes msgs))


doUpload :: FilePath -> PartInfo ->
              Either PolicyViolationException FilePath -> IO (Maybe Text)
doUpload _ _ (Left e) =
  return (Just $ T.pack $ show e)
doUpload dest partinfo (Right src) = do
  let srcName = takeFileName src
  let destName = maybe srcName B.toString (partFileName partinfo)
  (copyFile src (dest</>destName) >> return Nothing) `catch`
    (\(e::IOException) -> return (Just $ T.pack $ show e))



handleDelete :: FilePath -> Codex ()
handleDelete rqpath = do
  root <- getDocumentRoot
  let filepath = root </> rqpath
  liftIO (removeFile filepath)
  let rqdir = takeDirectory rqpath
  redirectURL (Files $ splitDirectories rqdir)


handleRename :: FilePath -> Codex ()
handleRename rqpath = do
  root <- getDocumentRoot
  let rqdir = takeDirectory rqpath
  dest <- B.toString <$> require (getParam "destname")
  let srcfile = root </> rqpath
  let destfile = root </> rqdir </> dest
  liftIO $ renameFile srcfile destfile
  redirectURL (Files $ splitDirectories rqdir)



--
--  | Handle requests for submission listing
--
handleSubmissionList :: Codex ()
handleSubmissionList = do
  requireAdmin  
  patts <- getPatterns
  page <- fromMaybe 1 <$> readParam "page"
  order <- fromMaybe Ascending <$> readParam "order"
  handleMethodOverride $ do 
    methods [GET,POST] (listSubmissions patts order page)
    <|>
    method PATCH (reevalSubmissions patts order >>
                    listSubmissions patts order page)
    <|>
    method (Method "CANCEL") (cancelPending >>
                              listSubmissions patts order page)
    <|>
    method (Method "EXPORT") (exportSubmissions patts order)
    <|>
    method (Method "PRINT") (generatePrintouts patts order)
            

-- | List submissions
listSubmissions :: Patterns -> Codex.Submission.Ordering -> Int -> Codex ()
listSubmissions patts order reqpage = do
  count <- countSubmissions patts
  let entries = 50   -- # entries per page
  let npages
        | count>0 = ceiling (fromIntegral count / fromIntegral entries :: Double)
        | otherwise = 1
  -- restrict to visible pages
  let page = 1 `max` reqpage `min` npages
  let offset = (page - 1) * entries  
  subs <- filterSubmissions patts order entries offset
  tz <- liftIO getCurrentTimeZone
  renderWithSplices "_submission_list" $ do
    patternSplices patts
    "page" ## I.textSplice (T.pack $ show page)
    "order" ## I.textSplice (T.pack $ show order)
    "submissions-count" ## I.textSplice (T.pack $ show count)
    "if-ascending" ## I.ifElseISplice (order == Ascending)
    "if-submissions" ## I.ifElseISplice (count > 0)
    "page-count" ## I.textSplice (T.pack $ show npages)
    "submissions" ## I.mapSplices (I.runChildrenWith . submitSplices tz) subs
    "submissions-prev-url" ## urlParamsSplice SubmissionList
                               (("page", Just (T.pack $ show $ page-1)) :
                                ("order", Just (T.pack $ show order)) :
                                 patts)
    "submissions-next-url" ## urlParamsSplice SubmissionList
                               (("page", Just (T.pack $ show $ page+1)) :
                                 ("order", Just (T.pack $ show order)) :
                                patts)

-- | Start re-evaluation of selected submissions 
reevalSubmissions :: Patterns -> Codex.Submission.Ordering -> Codex ()
reevalSubmissions patts order  = do
  cancelPending
  count <- countSubmissions patts
  subs  <- filterSubmissions patts order count 0
  evaluateMany subs



-- | Export listed submissions
exportSubmissions :: Patterns -> Codex.Submission.Ordering -> Codex ()
exportSubmissions patts ord = do
  sep <- B.toString <$> require (getParam "sep")
  serveFile =<< exportSubmissions' patts ord "export.txt" sep

-- | Create a CSV text file with submission listsing
-- TODO: the temporary file is *not* removed
exportSubmissions' ::
  Patterns -> Codex.Submission.Ordering -> FilePath -> String -> Codex FilePath
exportSubmissions' patts ord filetpl sep  = do
  tmpDir <- liftIO getTemporaryDirectory
  (tmpPath, handle) <-
    liftIO $ openTempFileWithDefaultPermissions tmpDir filetpl
  liftIO (hPutStrLn handle header)
  withFilterSubmissions patts ord () (output handle)
  liftIO (hClose handle)
  return tmpPath
  where
    header = intercalate sep ["id", "user_id", "path", "language",
                              "status", "policy", "received"]
    output :: Handle -> () -> Submission -> IO ()
    output h _ Submission{..} = do
      let row = intercalate sep [show submitId,
                                 show submitUser,
                                 show submitPath,
                                 show (codeLang submitCode),
                                 show (resultStatus submitResult),
                                 show submitCheck,
                                 show (show submitTime)
                                ]
      hPutStrLn h row




-- | Handle admin requests for a single submission
handleSubmissionAdmin :: SubmitId -> Codex ()
handleSubmissionAdmin sid = do
  requireAdmin
  sub <- require (getSubmission sid) <|> notFound
  handleMethodOverride $ do 
      method GET (report sub)
      <|>
      method PATCH (reevaluate sub)
      <|>
      method DELETE (delete sub)
  where
    -- get report on a submission
    report :: Submission -> Codex ()
    report sub = do
      root <- getDocumentRoot
      page <- readMarkdownFile (root </>submitPath sub)
      tz <- liftIO getCurrentTimeZone
      userName <- queryFullname (submitUser sub)
      renderWithSplices "_submission_admin" $ do
        pageSplices page
        submitSplices tz sub
        "submit-user-name" ## I.textSplice $ fromMaybe "?" userName

    -- delete a submission
    delete :: Submission -> Codex ()
    delete sub = do
      deleteSubmission (submitId sub)
      redirectURL SubmissionList

    -- revaluate a single submission
    reevaluate :: Submission -> Codex ()
    reevaluate sub = do
      -- let sid = submitId sub
      -- sqlite <- S.getSqliteState
      -- liftIO $ markEvaluating sqlite [sid]
      evaluate sub
      redirectURL (SubmissionAdmin (submitId sub))


