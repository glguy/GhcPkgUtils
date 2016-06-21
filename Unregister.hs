module Unregister where

import InstalledPackages
import Distribution.Text
import Distribution.Package (PackageId)
import Distribution.Simple.PackageIndex
import Distribution.InstalledPackageInfo
import Distribution.Simple.GHC
import Distribution.Simple.Program.HcPkg
import Distribution.Simple.Compiler (PackageDB(..))
import Distribution.Verbosity (normal)

import Config
import GraphExtraction

-- | Unregister the given list of packages and return
-- the full list of unregistered packages.
main :: Config -> [String] -> IO ()
main config pkgStrs =
  do (_,pgmConfig) <- getProgramConfiguration config
     let hcPkg = hcPkgInfo pgmConfig
     pkgIds <- traverse parsePkg pkgStrs
     pkgIndex <- getUserPackages config
     let plan = computePlan pkgIds pkgIndex
     mapM_ (unregister hcPkg normal UserPackageDB) plan

parsePkg :: String -> IO PackageId
parsePkg str =
  case simpleParse str of
    Nothing -> fail ("Unable to parse package: " ++ str)
    Just p -> return p

computePlan ::
  [PackageId] ->
  PackageIndex InstalledPackageInfo ->
  [PackageId]
computePlan rootIds pkgIndex = sourcePackageId . lookupVertex <$> plan
  where
    rootPkgs     = lookupSourcePackageId pkgIndex =<< rootIds
    rootVertexes = unitIdToVertex' . installedUnitId <$> rootPkgs
    plan         = extract pkgGraph rootVertexes

    (pkgGraph, lookupVertex, unitIdToVertex) = dependencyGraph pkgIndex
    unitIdToVertex' i =
      case unitIdToVertex i of
        Nothing -> error ("computePlan: " ++ show i)
        Just v  -> v
