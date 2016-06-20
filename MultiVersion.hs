module MultiVersion where

import Data.List
import qualified Data.Map as Map
import Data.Version
import Distribution.Simple.PackageIndex (InstalledPackageIndex, allPackages)
import Distribution.Package (PackageIdentifier(..), PackageName(..))
import Distribution.InstalledPackageInfo (InstalledPackageInfo(..))


import Config
import InstalledPackages

main :: Config -> [String] -> IO ()
main config _args = do
  pkgs <- getAllPackages config
  putStr
     . unlines
     . concatMap (\(p,vs) -> map (\v -> p ++ "-" ++ showVersion v) vs)
     . Map.toList
     . Map.filter (not . null)
     . fmap removeMaximum
     . Map.fromListWith (++)
     . map (\(x,y) -> (x,[y]))
     . map toPkgTuple
     $ allPackages pkgs

toPkgTuple pkg = (name, currentVersion)
  where
  pkgId = sourcePackageId pkg
  name = unPackageName (pkgName pkgId)
  currentVersion = pkgVersion pkgId

removeMaximum :: [Version] -> [Version]
removeMaximum xs = delete (maximum xs) xs
