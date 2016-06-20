module Outdated where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (listToMaybe)
import Data.Version

import System.Directory (getAppUserDataDirectory)
import System.FilePath

import Distribution.ParseUtils
import Distribution.InstalledPackageInfo (InstalledPackageInfo(..))
import Distribution.Simple.PackageIndex (allPackages)
import Distribution.Package(PackageIdentifier(..), PackageName(..))

import Config
import CabalVersions (loadLatestVersions)
import InstalledPackages (getUserPackages)

main :: Config -> [String] -> IO ()
main config _ = do
  userPackages <- getUserPackages config
  latest  <- loadLatestVersions =<< determineRepoCachePath
  mapM_ (check config latest) (allPackages userPackages)

check :: Config -> Map String Version -> InstalledPackageInfo -> IO ()
check config latest pkg =
  let pkgId = sourcePackageId pkg
      name = unPackageName (pkgName pkgId)
      currentVersion = pkgVersion pkgId
  in
  case Map.lookup name latest of
    Just cabalVersion
      | cabalVersion > currentVersion ->
          let output
                | quiet config = name
                | otherwise = name ++ " current: " ++ showVersion currentVersion
                                   ++ " latest: "  ++ showVersion cabalVersion
          in putStrLn output
    _ -> return ()

--
-- Look up cabal's repository cache
--

determineRepoCachePath :: IO FilePath
determineRepoCachePath =
  do cabalConfig <- loadCabalConfig
     case lookupField "remote-repo-cache" cabalConfig of
       Nothing -> fail "No remote-repo-cache field in cabal configuration"
       Just dir -> return (dir </> "hackage.haskell.org" </> "00-index.tar.gz")

-- | Default cabal configuration directory as defined in
-- Distribution.Client.Config
defaultCabalDir :: IO FilePath
defaultCabalDir = getAppUserDataDirectory "cabal"

-- | Default cabal configuration file as defined in
-- Distribution.Client.Config
defaultCabalConfigFile :: IO FilePath
defaultCabalConfigFile =
  do dir <- defaultCabalDir
     return (dir </> "config")

loadCabalConfig :: IO [Field]
loadCabalConfig =
  do cabalFileName <- defaultCabalConfigFile
     contents <- readFile cabalFileName
     case readFields contents of
       ParseFailed perror -> fail ("Unable to parse cabal configuration: " ++ show perror)
       ParseOk _warnings fields -> return fields

lookupField :: String -> [Field] -> Maybe String
lookupField k fields = listToMaybe [ v | F _ k' v <- fields, k == k' ]
