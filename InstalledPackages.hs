module InstalledPackages where

import Data.Version

import Distribution.Simple.GHC (configure, getInstalledPackages)
import Distribution.Verbosity (silent)
import Distribution.Simple.Program.Db (defaultProgramDb)
import Distribution.Simple.Compiler (PackageDBStack)
import Distribution.Simple.PackageIndex (allPackages)
import Distribution.InstalledPackageInfo (InstalledPackageInfo_(..))
import Distribution.Package (PackageIdentifier(..), PackageName(..))
import Distribution.Simple.PackageIndex (InstalledPackageIndex)

import Config

--
-- Programatically query the installed package DB
--

getPackageIndex :: Config -> PackageDBStack -> IO InstalledPackageIndex
getPackageIndex config packageDbStack =
  do (_compiler, _platform, programConfiguration)
       <- configure silent (hcPath config) (hcPkgPath config) defaultProgramDb

     getInstalledPackages silent packageDbStack programConfiguration

getPackages :: Config -> PackageDBStack -> IO [(String, Version)]
getPackages config packageDbStack =
  do packageIndex <- getPackageIndex config packageDbStack
     return
       [ ( unPackageName (pkgName pkgId), pkgVersion pkgId )
       | pkgInfo <- allPackages packageIndex
       , let pkgId = sourcePackageId pkgInfo ]
