{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}

------------------------------------------------------------------------------
-- | Administration facilities

module Admin(
  handleBrowse
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

-- import           System.Locale
import           Data.Time.Clock
import           Data.Time.LocalTime
import           System.FilePath
import           System.Directory
import           System.IO.Error
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

import qualified Data.HashMap.Strict as HM

import           Data.Maybe (catMaybes, maybeToList)
import           Data.List (union, sort, sortBy)
import           Data.Monoid
import           Data.Map.Syntax
import           Application
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
  let rqdir = takeDirectory rqpath
  "file-path" ## I.textSplice (T.pack rqpath)
  "file-dir" ## I.textSplice (T.pack rqdir)            
  "file-path-url" ## I.textSplice (T.decodeUtf8 $ encodePath rqpath)
  "file-dir-url" ## I.textSplice (T.decodeUtf8 $ encodePath rqdir)


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
  let rqdir = takeDirectory rqpath         
  contents <- require (getTextPost "editform.editor")
  liftIO $ T.writeFile (base</>rqpath) contents
  redirect (encodePath ("/files" </> rqdir))


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
doUpload dest partinfo (Left e) =
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


       
  {-
-----------------------------------------------------------------------------
ensureAdmin :: Codex ()
ensureAdmin =  do
  au <- require (with auth currentUser) <|> unauthorized
  when (not $ isAdmin au) unathorized
-}



