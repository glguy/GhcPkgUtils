module InstalledPackages where

import Data.Version

import Distribution.Simple.GHC (configure, getInstalledPackages)
import Distribution.Verbosity (silent)
import Distribution.Simple.Program.Db (defaultProgramDb)
import Distribution.Simple.Compiler (PackageDBStack)
import Distribution.Simple.PackageIndex (allPackages)
import Distribution.InstalledPackageInfo (InstalledPackageInfo_(..))
import Distribution.Package (PackageIdentifier(..), PackageName(..))

import Config

--
-- Programatically query the installed package DB
--

getPackages :: Config -> PackageDBStack -> IO [(String, Version)]
getPackages config packageDbStack =
  do (_compiler, _platform, programConfiguration)
       <- configure silent (hcPath config) (hcPkgPath config) defaultProgramDb

     packageIndex
       <- getInstalledPackages silent packageDbStack programConfiguration

     return
       [ ( unPackageName (pkgName pkgId), pkgVersion pkgId )
       | pkgInfo <- allPackages packageIndex
       , let pkgId = sourcePackageId pkgInfo ]
