{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           System.Environment
import           System.Exit
import           System.Console.GetOpt

import           Data.Configurator.Types
import qualified Data.Configurator as Configurator

import           Codex.Types
import           Codex.Page
import           Codex.Tester
import           Codex.Testers


--- | command line options
data Options
  = Options { optEnv  :: Maybe FilePath
            , optVerb :: Int
            } deriving Show

defaultOptions :: Options
defaultOptions = Options { optEnv = Nothing
                         , optVerb = 2
                         }


optionsList :: [OptDescr (Options -> Options)]
optionsList =
  [ Option ['e'] ["environment"]
    (ReqArg (\e opts -> opts {optEnv=Just e}) "FILE")
    "configuration file"
  , Option ['v'] ["verbosity"]
    (ReqArg (\n opts -> opts {optVerb=read n}) "LEVEL")
    "verbosity level (0-2)"
  ]

header :: String
header = "Usage: codex-tester [OPTION...] page-file code-file"
  
getOpts :: Options -> IO (Options, [String])  
getOpts defOptions  = do
  args <- getArgs
  case getOpt Permute optionsList args of
    (flags, args', []) ->
      return (foldl (flip id) defOptions flags, args')
    (_,_,errs) ->
      throwIO $ userError (concat errs ++ usageInfo header optionsList)

  

main :: IO ()
main = do
  (opts, args) <- getOpts defaultOptions
  case optEnv opts of
    Nothing -> throwIO $ userError "no config environment specified"
    Just cfg -> do
      conf <- Configurator.load [Required cfg]
      process (optVerb opts) conf args


process :: Int -> Config -> [FilePath] -> IO ()
process verb conf [pagePath,codePath] = do
  page <- readMarkdownFile pagePath
  lang <- return (pageLanguage page)
          `maybeThrow` userError "no language defined"
  txt <- T.readFile codePath
  res <- runTester conf pagePath page (Code lang txt) allTesters
         `maybeThrow` userError "no tester defined"
  T.putStr $ ppResult verb $ res
  if resultClassify res == Accepted then
    exitSuccess
    else
    exitFailure
process _ _ _ =
  throwIO $ userError $ usageInfo header optionsList


ppResult :: Int -> Result -> T.Text
ppResult verb r
  = T.unlines $
  ([ T.pack $ show $ resultClassify r | verb>=1 ] ++
   [ resultMessage r | verb >= 2]
  )


maybeThrow :: IO (Maybe a) -> IOError -> IO a
maybeThrow act ex = do
  r <- act
  case r of
    Nothing -> throwIO ex
    Just v -> return v


