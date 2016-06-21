module InstalledPackages where

import Distribution.Simple.GHC
   (configure, getPackageDBContents, getInstalledPackages)
import Distribution.Verbosity (silent)
import Distribution.Simple.Program.Db (defaultProgramDb)
import Distribution.Simple.Compiler (Compiler, PackageDB(..))
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import Distribution.Simple.Program (ProgramConfiguration)

import Config

--
-- Programatically query the installed package DB
--

getProgramConfiguration :: Config -> IO (Compiler, ProgramConfiguration)
getProgramConfiguration config =
  do (compiler, _platform, programConfiguration)
       <- configure silent (hcPath config) (hcPkgPath config) defaultProgramDb
     return (compiler, programConfiguration)

getAllPackages :: Config -> IO InstalledPackageIndex
getAllPackages config =
  do (compiler, programConfiguration) <- getProgramConfiguration config
     getInstalledPackages silent compiler [GlobalPackageDB, UserPackageDB] programConfiguration


getUserPackages :: Config -> IO InstalledPackageIndex
getUserPackages config =
  do (_, programConfiguration) <- getProgramConfiguration config
     getPackageDBContents silent UserPackageDB programConfiguration
