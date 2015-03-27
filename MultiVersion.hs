module MultiVersion where

import GhcPkgPath
import Data.List
import Data.List.Split
import qualified Data.Map as Map
import Text.Read (readMaybe)
import System.Process

main :: [String] -> IO ()
main _args = do
  ghc_pkg <- getGhcPkgPath
  pkgs <- readProcess ghc_pkg ["list"] ""
  putStr
     . unlines
     . concatMap (\(p,vs) -> map (\v -> p ++ "-" ++ showVersion v) vs)
     . Map.toList
     . Map.filter (not . null)
     . fmap removeMaximum
     . Map.fromListWith (++)
     . fmap (fmap return . splitVersion)
     . filter ("   " `isPrefixOf`)
     . lines
     $ pkgs

removeMaximum :: [Version] -> [Version]
removeMaximum xs = delete (maximum xs) xs

splitVersion :: String -> (String, Version)
splitVersion xs = (name, readVersion version)
  where
  (a,_:b) = break (=='-') (reverse xs)
  name = reverse b
  version = reverse a

newtype Version = Version [Int]
  deriving (Eq, Ord)

readVersion :: String -> Version
readVersion x =
  case mapM readMaybe (splitOn "." (x \\ "()")) of
    Nothing -> error x
    Just v -> Version v

showVersion :: Version -> String
showVersion (Version v) = intercalate "." (map show v)
