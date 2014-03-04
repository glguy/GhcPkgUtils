module Outdated where

import Data.Char                (isSpace)
import System.Process           (readProcess)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import System.Environment
import System.Console.GetOpt
import System.Exit
import System.IO
import System.FilePath
import Data.List

import CabalVersions
main :: [String] -> IO ()
main args = do
  config <- getConfig args
  userPackages <- fmap processPackageList ghcPkgListUser
  latest  <- loadLatestVersions =<< determineRepoCachePath
  mapM_ (check config latest) userPackages

ghcPkgListUser :: IO String
ghcPkgListUser = readProcess "ghc-pkg" ["list", "--user"] ""

check :: Config -> Map String Version -> (String, Version) -> IO ()
check config latest (name, currentVersion) =
  case Map.lookup name latest of
    Just cabalVersion
      | cabalVersion > currentVersion ->
          if quiet config
          then putStrLn name
          else putStrLn (name ++ " current: " ++ prettyVersion currentVersion
                              ++ " latest: "  ++ prettyVersion cabalVersion)
    _ -> return ()

processPackageList :: String -> [(String, Version)]
processPackageList = map clean1 . init . tail . lines
  where
  clean1 xs = (name, version)
    where
    (version', name') = break (== '-') (reverse (dropWhile isSpace xs))
    name = reverse (drop 1 name')
    Just version = parseVersion (reverse version')

--
-- Command line stuff
--

data Config = Config
  { quiet :: !Bool }

defaultConfig :: Config
defaultConfig = Config False

optDescs :: [OptDescr (Config -> Config)]
optDescs = [Option ['q'] ["quiet"] (NoArg (\c -> c { quiet = True })) "generate machine readable output"]

getConfig :: [String] -> IO Config
getConfig args =
  case getOpt Permute optDescs args of
    (fns, [], []) -> return (foldl' (\config fn -> fn config) defaultConfig fns)
    (_,[],errs) -> do mapM_ (hPutStrLn stderr) errs
                      exitFailure
    (_,_,_) -> do hPutStrLn stderr "Unsupported arguments"
                  exitFailure

determineRepoCachePath :: IO FilePath
determineRepoCachePath =
  do home <- getEnv "HOME"
     let cabalConfig = home </> ".cabal" </> "config"
     txt <- readFile cabalConfig
     case mapMaybe (stripPrefix "remote-repo-cache:") (lines txt) of
       [path] -> let dir = dropWhile isSpace (dropWhileEnd isSpace path)
                 in return (dir </> "hackage.haskell.org" </> "00-index.tar.gz")
       [] -> fail "No remote-repo-cache field in ~/.cabal/config"
       _  -> fail "Ambiguous remote-repo-cache in ~/.cabal/config"
