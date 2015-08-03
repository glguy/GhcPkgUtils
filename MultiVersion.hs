module MultiVersion where

import Data.List
import qualified Data.Map as Map
import Data.Version

import Distribution.Simple.Compiler (PackageDB(GlobalPackageDB,UserPackageDB))

import Config
import InstalledPackages

main :: Config -> [String] -> IO ()
main config _args = do
  pkgs <- getPackages config [GlobalPackageDB, UserPackageDB]
  putStr
     . unlines
     . concatMap (\(p,vs) -> map (\v -> p ++ "-" ++ showVersion v) vs)
     . Map.toList
     . Map.filter (not . null)
     . fmap removeMaximum
     . Map.fromListWith (++)
     . map (\(x,y) -> (x,[y]))
     $ pkgs

removeMaximum :: [Version] -> [Version]
removeMaximum xs = delete (maximum xs) xs
