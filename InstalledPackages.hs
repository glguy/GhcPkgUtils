module InstalledPackages where

import Data.Version

import Distribution.Simple.GHC (configure, getPackageDBContents, getInstalledPackages)
import Distribution.Verbosity (silent)
import Distribution.Simple.Program.Db (defaultProgramDb)
import Distribution.InstalledPackageInfo (InstalledPackageInfo_(..))
import Distribution.Package (PackageIdentifier(..), PackageName(..))
import Distribution.Simple.Compiler (PackageDB(..))
import Distribution.Simple.PackageIndex (allPackages)
import Distribution.Simple.Program (ProgramConfiguration)

import Config

--
-- Programatically query the installed package DB
--

getProgramConfiguration :: Config -> IO ProgramConfiguration
getProgramConfiguration config =
  do (_compiler, _platform, programConfiguration)
       <- configure silent (hcPath config) (hcPkgPath config) defaultProgramDb
     return programConfiguration

getAllPackages :: Config -> IO [(String, Version)]
getAllPackages config =
  do programConfiguration <- getProgramConfiguration config
     packageIndex <- getInstalledPackages silent [GlobalPackageDB, UserPackageDB] programConfiguration
     return
       [ ( unPackageName (pkgName pkgId), pkgVersion pkgId )
       | pkgInfo <- allPackages packageIndex
       , let pkgId = sourcePackageId pkgInfo ]

getUserPackages :: Config -> IO [(String, Version)]
getUserPackages config =
  do programConfiguration <- getProgramConfiguration config
     pkgs <- getPackageDBContents silent UserPackageDB programConfiguration
     return
       [ ( unPackageName (pkgName pkgId), pkgVersion pkgId )
       | pkgInfo <- allPackages pkgs
       , let pkgId = sourcePackageId pkgInfo ]
