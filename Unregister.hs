module Unregister where

import System.Process
import System.Exit
import Data.List

import Config

data UnregisterOutcome
  = Success
  | NotFound
  | Depends [String]

computeGhcPkg :: Config -> FilePath
computeGhcPkg config =
  case (hcPath config, hcPkgPath config) of
    (_, Just p) -> p
    (Just p, _) -> p ++ "-pkg"
    _           -> "ghc-pkg"

unregisterPackage :: Config -> String -> IO UnregisterOutcome
unregisterPackage config name = do
  let ghc_pkg = computeGhcPkg config
  (exit, _out, err) <- readProcessWithExitCode ghc_pkg ["unregister",name] ""
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
main :: Config -> [String] -> IO ()
main config = aux []
  where
  aux _ [] = return ()
  aux visited (name:names)
    | name `elem` visited = aux visited names
    | otherwise = do
        res <- unregisterPackage config name
        case res of
          Success -> do
            putStrLn ("Unregistered: " ++ name)
            aux (name:visited) names
          Depends deps -> do
            aux visited (deps ++ name:names)
          NotFound -> do
            putStrLn ("Not found: " ++ name)
            aux visited names

