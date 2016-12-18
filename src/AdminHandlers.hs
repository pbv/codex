{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}

------------------------------------------------------------------------------
-- | Administration facilities; file browsing

module AdminHandlers(
  handleBrowse,
  handleSubmissionList,
  handleExport
  ) where

import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe hiding (mimeTypes)
import           Snap.Util.FileUploads
import           Heist
import           Heist.Splices     as I
import qualified Heist.Interpreted as I


import           Data.Time.Clock
import           Data.Time.LocalTime
import           System.FilePath
import           System.Directory
import           System.IO
import           Control.Monad
import           Control.Monad.Trans (liftIO)
import           Control.Exception.Lifted 
import           Control.Applicative


import           Data.Text (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

import           Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as B
import qualified Data.ByteString  as B

import           Data.Maybe (fromMaybe,catMaybes)
import           Data.List (sort, intersperse)
import           Data.Map.Syntax
import           Types
import           Language
import           Tester
import           Application
import           Submission
import           AceEditor
import           Utils
import           Config


--
-- | handle file browser requests
-- ensure that an user with admin privileges is logged in
--
handleBrowse :: FilePath -> Codex ()
handleBrowse base = do
  au <- require (with auth currentUser) <|> unauthorized
  when (not $ isAdmin au) unauthorized
  handleMethodOverride (method GET (handleGet base) <|>
                        method POST (handleUpload base) <|>
                        method PUT (handleEdit base) <|>
                        method DELETE (handleDelete base) <|>
                        method PATCH (handleRename base))


handleGet :: FilePath -> Codex ()
handleGet base = file <|> directory <|> notFound
  where
    directory = do
      rqpath <- getSafePath
      let path = base </> rqpath
      c <- liftIO $ doesDirectoryExist path  
      when (not c) pass
      entries <- liftIO $ listDir path
      tz <- liftIO getCurrentTimeZone
      renderWithSplices "file-list" $ do
        pathSplices rqpath
        listingSplices tz rqpath entries
        messageSplices []
    file = do
      rqpath <- getSafePath
      let path = base </> rqpath
      c <- liftIO $ doesFileExist path
      when (not c) pass
      let mime = fileType mimeTypes rqpath
      contents <- if B.isPrefixOf "text" mime then
                    liftIO $ T.readFile path
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
listDir base = do
  entries <-  getDirectoryContents base
  dirs  <- filterM (doesDirectoryExist . (base</>)) entries
  files <- filterM (doesFileExist . (base</>)) entries
  lst <- forM (sort (filter (/=".") dirs)) $ \path -> do
    time <- getModificationTime (base</>path)
    return (path, "DIR", time)
  lst' <- forM (sort files) $ \path -> do
    time <- getModificationTime (base</>path)
    return (path, fileType mimeTypes path, time)
  return (lst ++ lst')
  


handleEdit :: FilePath -> Codex ()
handleEdit base = do
  rqpath <- getSafePath
  contents <- require (getTextPost "editform.editor")
  liftIO $ T.writeFile (base</>rqpath) contents
  redirect (encodePath ("/files" </> rqpath))


handleUpload :: FilePath -> Codex ()
handleUpload dest = do
  rqpath <- getSafePath
  b <- liftIO $ doesDirectoryExist (dest</>rqpath)
  when (not b) pass
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




--  | handle requests for submission lists
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


-- | Export all submissions
handleExport :: Codex ()
handleExport = method POST $ do
  au <- require (with auth currentUser) <|> unauthorized
  when (not (isAdmin au)) unauthorized
  sep <- B.toString <$> require (getParam "sep")
  serveFile =<< exportSubmissions "export.txt" sep
  


-- | export all submissions to a CSV text file
-- NB: the caller should remove the temporary file 
exportSubmissions :: FilePath -> String -> Codex FilePath
exportSubmissions filetpl sep  = do
  tmpDir <- liftIO getTemporaryDirectory
  (path, handle) <-
    liftIO $ openTempFileWithDefaultPermissions tmpDir filetpl
  liftIO (hPutStrLn handle header)
  count <- withSubmissions () (output handle)
  liftIO (hClose handle)
  return path
  where
    header = concat $ intersperse sep ["id", "user_id", "path", "language",
                                       "classify", "timing", "received"]
    output :: Handle -> () -> Submission -> IO ()
    output h _ Submission{..} = do
      let row = concat $ intersperse sep [show (fromSID submitID),
                                          show (fromUID submitUser),
                                          show submitPath,
                                          show (fromLanguage $
                                                codeLang submitCode),
                                          show (resultClassify submitResult),
                                          show submitTiming,
                                          show (show submitTime)
                                         ]
      hPutStrLn h row
