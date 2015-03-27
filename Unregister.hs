module Unregister where

import GhcPkgPath
import System.Process
import Control.Monad
import System.Exit
import Data.List
import System.Environment

data UnregisterOutcome
  = Success
  | NotFound
  | Depends [String]

unregisterPackage :: String -> IO UnregisterOutcome
unregisterPackage name = do
  ghc_pkg <- getGhcPkgPath
  (exit, out, err) <- readProcessWithExitCode ghc_pkg ["unregister",name] ""
  return $ case exit of
    ExitSuccess -> Success
    _ | "ghc-pkg: cannot find package" `isPrefixOf` err -> NotFound
      | "ghc-pkg: unregistering"       `isPrefixOf` err -> Depends (parseDepends err)
      | otherwise -> error ("Can't parse: " ++ err)

parseDepends :: String -> [String]
parseDepends
  = words
  . takeWhile (/= '(')
  . drop 2
  . dropWhile (/= ':')
  . drop 1
  . dropWhile (/= ':')

-- | Unregister the given list of packages and return
-- the full list of unregistered packages.
main :: [String] -> IO ()
main = aux []
  where
  aux visited [] = return ()
  aux visited (name:names)
    | name `elem` visited = aux visited names
    | otherwise = do
        res <- unregisterPackage name
        case res of
          Success -> do
            putStrLn ("Unregistered: " ++ name)
            aux (name:visited) names
          Depends deps -> do
            aux visited (deps ++ name:names)
          NotFound -> do
            putStrLn ("Not found: " ++ name)
            aux visited names

