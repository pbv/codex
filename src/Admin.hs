{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}

------------------------------------------------------------------------------
-- | Administration facilities

module Admin where

import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe (MimeMap, defaultMimeTypes,
                                      fileType, getSafePath, serveFileAs)
import           Heist
import           Heist.Splices
import qualified Heist.Interpreted as I

-- import           System.Locale
import           Data.Time.Clock
import           Data.Time.LocalTime
import           System.FilePath
import           System.Directory
import           System.IO.Error
import           Control.Monad
import           Control.Monad.Trans (liftIO)
import           Control.Applicative

import           Data.Text (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

import           Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as B
import qualified Data.ByteString  as B

import qualified Data.HashMap.Strict as HM

import           Data.List (union, sort, sortBy)
import           Data.Monoid
import           Data.Map.Syntax
import           Application
import           AceEditor
import           Utils



-- | generate a directory listing
--
handleListing :: FilePath -> Codex ()
handleListing base
  = requireAdmin >> method GET (directory <|> file <|> notFound)
  where
    directory = do
      rqpath <- getSafePath
      let dirpath = base </> rqpath
      b <- liftIO $ doesDirectoryExist dirpath
      when (not b) pass
      entries <- liftIO $ listDir dirpath
      tz <- liftIO getCurrentTimeZone  
      renderWithSplices "file-list" $ do
        "request-path" ## I.textSplice (T.pack rqpath)
        listingSplices tz rqpath entries
    --
    file = do
      rqpath <- getSafePath
      let filepath = base </> rqpath
      b <- liftIO $ doesFileExist filepath
      when (not b) pass
      serveFileAs (fileType mimeTypes rqpath) filepath

  

listingSplices tz path list =
  "file-list" ## I.mapSplices (I.runChildrenWith . splices) list
  where splices (name, mime, time) = do
          "file-name" ## I.textSplice (T.pack name)
          "file-type" ## I.textSplice (T.decodeUtf8 mime)
          "file-modified" ## utcTimeSplice tz time
          "if-text" ## ifElseISplice (B.isPrefixOf "text/" mime)
          "if-dir" ## ifElseISplice (mime == "DIR")
          "request-path" ## I.textSplice (T.pack (path </> name))


listDir :: FilePath -> IO [(FilePath, ByteString, UTCTime)]
listDir base = do
  entries <-  getDirectoryContents base
  dirs  <- filterM (doesDirectoryExist . (base</>)) entries
  files <- filterM (doesFileExist . (base</>)) entries
  --
  lst <- forM (sort (filter (/=".") dirs)) $ \path -> do
    time <- getModificationTime (base</>path)
    return (path, "DIR", time)
  --
  lst' <- forM (sort (filter (not.hidden) files)) $ \path -> do
    time <- getModificationTime (base</>path)
    return (path, fileType mimeTypes path, time)
  --  
  return (lst ++ lst')
  

hidden :: FilePath -> Bool
hidden f = head f == '#' || last f == '~'


handleEdit :: FilePath -> Codex ()
handleEdit base
  = requireAdmin >> (method GET get <|> method POST post <|> badRequest)
 where get = do
         rqpath <- getSafePath
         let path = base </> rqpath
         b <- liftIO $ doesFileExist path
         when (not b) pass
         contents <- liftIO (T.readFile path)
         renderWithSplices "editfile" $ do
           inputAceEditorSplices
           "edit-path" ## I.textSplice (T.pack rqpath)
           "edit-text" ## I.textSplice contents
       post = do
         rqpath <- getSafePath
         contents <- require (getTextPost "editform.editor")
         liftIO $ T.writeFile (base</>rqpath) contents
         redirect (B.fromString ("/browse" </> takeDirectory rqpath))

-- | ensure that an user with admin priviliges is logged in
requireAdmin :: Codex ()
requireAdmin =  do
  AuthUser{..} <- require (with auth currentUser) <|> unauthorized
  guard (Role "admin" `elem` userRoles) <|> unauthorized



mimeTypes :: MimeMap
mimeTypes = HM.union defaultMimeTypes $
            HM.fromList [(".tst", "text/plain"),
                         (".py",  "text/plain"),
                         (".md",  "text/markdown")]

