module InstalledPackages where

import Data.Version

import Distribution.Simple.GHC (configure, getPackageDBContents)
import Distribution.Verbosity (silent)
import Distribution.Simple.Program.Db (defaultProgramDb)
import Distribution.Simple.Compiler (PackageDB(UserPackageDB))
import Distribution.Simple.PackageIndex (allPackages)
import Distribution.InstalledPackageInfo (InstalledPackageInfo, InstalledPackageInfo_(..))
import Distribution.Package (PackageIdentifier(..), PackageName(..), PackageId(..))

import CabalVersions

--
-- Programatically query the installed package DB
--

getInstalledPackages :: IO [(String, Version)]
getInstalledPackages =
  do let hcPath = Nothing
         hcPkgPath = Nothing

     (_compiler, _platform, programConfiguration)
       <- configure silent hcPath hcPkgPath defaultProgramDb

     packageIndex
       <- getPackageDBContents silent UserPackageDB programConfiguration

     return
       [ ( unPackageName (pkgName pkgId), pkgVersion pkgId )
       | pkgInfo <- allPackages packageIndex
       , let pkgId = sourcePackageId pkgInfo ]
