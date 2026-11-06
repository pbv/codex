{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-
  Two-factor authentication: token-based validation.

  A new table @two_fa_tokens@ stores valid single-use tokens.
  When a token is used for login it is locked to that user and cannot
  be reused. 
-}
module Codex.TwoFactor where

import Control.Concurrent.MVar (withMVar)
import Control.Lens ((^#))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Configurator as Configurator

import           Data.Text(Text)
import Data.Time.Clock (getCurrentTime, UTCTime)
import Data.Maybe (fromMaybe,listToMaybe)
import Snap.Snaplet
import Snap.Snaplet.SqliteSimple
  ( Sqlite (..)
  , sqliteConn 
  , withSqlite
  )
import Database.SQLite.Simple (execute_)
import Database.SQLite.Simple (Only (..))
import qualified Database.SQLite.Simple as S

import Codex.Application
import Codex.Types (UserLogin (..)) 

-- | Represents a stored 2FA token.
data TwoFaToken = TwoFaToken
  { tokenId   :: !Int
  , tokenText  :: !Text
  , tokenUsed  :: !Bool
  , tokenUsedBy :: !(Maybe UserLogin)
  , tokenUsedAt :: !(Maybe UTCTime)
  } deriving (Show)

-- | Initialise the two_fa_tokens table (if the feature is enabled).
initTwoFaTable :: Snaplet Sqlite -> IO ()
initTwoFaTable sql = do
  let c = sqliteConn $ sql ^# snapletValue
  withMVar c $ \conn -> do
    -- Create table only; do NOT touch existing tables.
    execute_ conn
      "CREATE TABLE IF NOT EXISTS two_fa_tokens (\
       \id INTEGER PRIMARY KEY AUTOINCREMENT, \
       \token TEXT UNIQUE NOT NULL, \
       \used INTEGER NOT NULL DEFAULT 0, \
       \used_by TEXT, \
       \used_at TIMESTAMP)"



-- | Convert a DB row into a TwoFaToken.
rowToToken :: (Int, Text, Bool, Maybe UserLogin, Maybe UTCTime) -> TwoFaToken
rowToToken (tokid, token, used, by, at) =
  TwoFaToken
    { tokenId   = tokid
    , tokenText  = token
    , tokenUsed  = used
    , tokenUsedBy = by 
    , tokenUsedAt = at
    }



-- | Check that a token is valid for a given user
-- if unused then assign to the user; else check that the assigned user matches
-- this is all done atomically because we grab the connection MVar
validateTwoFaToken :: Text -> UserLogin -> Codex (Either String TwoFaToken)
validateTwoFaToken tok ul = 
    withSqlite $ \conn -> do
      rows <- S.query conn
        "SELECT id, token, used, used_by, used_at \
        \FROM two_fa_tokens WHERE token = ?"
        (Only tok)
      let opt2Fa = fmap rowToToken (listToMaybe rows)
      case opt2Fa of
        Nothing -> return (Left "Invalid token")
        Just twoFa -> case tokenUsedBy twoFa of
          Just ul' -> if ul == ul' 
                        then return (Left "Token already used")
                        else do
                          return (Left "Token already used")
          Nothing -> do
            -- assign the token to the current user
            time <- getCurrentTime
            S.execute conn 
                "UPDATE two_fa_tokens SET used = 1, used_by = ?, used_at = ? \
                \WHERE id = ?" 
                (ul, time, tokenId twoFa)
            return $ Right twoFa { tokenUsed = True
                                 , tokenUsedBy = Just ul
                                 , tokenUsedAt = Just time 
                                 } 



isTwoFactorEnabled :: Codex Bool
isTwoFactorEnabled = do
  conf <- getSnapletUserConfig
  opt <- liftIO $ Configurator.lookup conf "users.two_factor"
  return (fromMaybe False opt) 
    