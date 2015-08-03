module Config where

import Data.List (foldl')

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

--
-- Command line stuff
--

data Config = Config
  { quiet :: !Bool
  , hcPath :: Maybe FilePath
  , hcPkgPath :: Maybe FilePath
  , showUsage :: Bool
  }

defaultConfig :: Config
defaultConfig = Config False Nothing Nothing False

optDescs :: [OptDescr (Config -> Config)]
optDescs =
  [Option ['q'] ["quiet"] (NoArg (\c -> c { quiet = True })) "Generate machine readable output"
  ,Option ['w'] ["with-compiler"] (ReqArg (\p c -> c { hcPath = Just p }) "PATH") "Give the path to a particular compiler"
  ,Option [] ["with-hc-pkg"] (ReqArg (\p c -> c { hcPkgPath = Just p }) "PATH") "Give the path to the package tool"
  ,Option ['h'] ["help"] (NoArg (\c -> c { showUsage = True })) "Show this help text"
  ]

printUsage :: IO a
printUsage =
  do prog <- getProgName
     hPutStr stderr (usageInfo prog optDescs)
     exitFailure

getConfig :: [String] -> IO (Config, [String])
getConfig args =
  case getOpt Permute optDescs args of
    (fns, cmds, []) ->
      do let c = foldl' (\config fn -> fn config) defaultConfig fns
         case cmds of
           [cmd] | not (showUsage c) -> return (c,cmds)
           _     -> printUsage
    (_,[],errs) -> do mapM_ (hPutStrLn stderr) errs
                      exitFailure
    (_,_,_) -> do hPutStrLn stderr "Unsupported arguments"
                  exitFailure
