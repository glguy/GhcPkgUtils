module GhcPkgPath (getGhcPkgPath) where

import System.Environment

getGhcPkgPath :: IO FilePath
getGhcPkgPath = maybe "ghc-pkg" (++"-pkg") `fmap` lookupEnv "GHC"
