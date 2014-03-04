module Main(main) where

import System.Environment(getArgs)
import System.IO(hPutStrLn, stderr)
import qualified Unregister
import qualified Outdated
import qualified MultiVersion

knownProgs :: [(String,[String] -> IO ()]
knownProgs =
  [ ("unregister",      Unregister.main)
  , ("outdated",        Outdated.main)
  , ("multi-version",   MultiVersion.main)
  ]

main :: IO ()
main =
  do as <- getArgs
     case as of
       [] -> showUsage
       cmd : params ->
         case lookup cmd knownProgs of
           Just go -> go params
           Nothing -> do hPutStrLn stderr ("Unknown command: " ++ cmd)
                         showUsage

showUsage :: IO ()
showUsage =
  do hPutStrLn stderr "Valid commands:"
     hPutStrLn stderr $ unlines $ map fst knownProgs



