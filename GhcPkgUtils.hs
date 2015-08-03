module Main(main) where

import System.Environment(getArgs)
import System.IO(hPutStrLn, stderr)

import Config (Config, getConfig)
import qualified Unregister
import qualified Outdated
import qualified MultiVersion

knownProgs :: [(String,Config -> [String] -> IO ())]
knownProgs =
  [ ("unregister",    Unregister.main)
  , ("outdated",      Outdated.main)
  , ("multiversion",  MultiVersion.main)
  ]

main :: IO ()
main =
  do (c,as) <- getConfig =<< getArgs
     case as of
       [] -> showUsage
       cmd : params ->
         case lookup cmd knownProgs of
           Just go -> go c params
           Nothing -> do hPutStrLn stderr ("Unknown command: " ++ cmd)
                         showUsage

showUsage :: IO ()
showUsage =
  do hPutStrLn stderr "Valid commands:"
     hPutStrLn stderr $ unlines $ map fst knownProgs
