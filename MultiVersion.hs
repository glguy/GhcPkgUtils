module Main where

import Data.List
import Data.List.Split
import Data.List.Split
import qualified Data.Map as Map
import Control.Lens
import Text.Read (readMaybe)
import System.Process

main = do
  pkgs <- readProcess "ghc-pkg" ["list"] ""
  putStr
     . unlines
     . concatMap (\(p,vs) -> map (\v -> p ++ "-" ++ showVersion v) vs)
     . Map.toList
     . Map.filter (not . null)
     . fmap removeMaximum
     . Map.fromListWith (++)
     . fmap (over _2 return . splitVersion)
     . filter ("   " `isPrefixOf`)
     . lines
     $ pkgs

removeMaximum :: [Version] -> [Version]
removeMaximum xs = delete (maximum xs) xs

splitVersion xs = (name, readVersion version)
  where
  (a,_:b) = break (=='-') (reverse xs)
  name = reverse b
  version = reverse a

newtype Version = Version [Int]
  deriving (Eq, Ord)

readVersion x =
  case mapM readMaybe (splitOn "." (x \\ "()")) of
    Nothing -> error x
    Just v -> Version v

showVersion (Version v) = intercalate "." (map show v)
