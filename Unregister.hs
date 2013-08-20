module Main where

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
  (exit, out, err) <- readProcessWithExitCode "ghc-pkg" ["unregister",name] ""
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
recursiveUnregisterPackages :: [String] -> IO [String]
recursiveUnregisterPackages = foldM aux []
  where
  aux visited name
    | name `elem` visited = return visited
    | otherwise = do
        res <- unregisterPackage name
        case res of
          Success -> do
            putStrLn ("Unregistered " ++ name)
            return (name:visited)
          Depends deps -> do
            visited' <- foldM aux visited deps
            aux visited' name
          NotFound -> do
            fail (name ++ " not found")

main :: IO ()
main = void (recursiveUnregisterPackages =<< getArgs)
