module CabalVersions where

import Codec.Compression.GZip (decompress)
import Control.Monad
import Data.List (isSuffixOf, intercalate)
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import System.FilePath
import Text.Read (readMaybe)
import qualified Codec.Archive.Tar as Tar
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map

--
-- Version strings
--

newtype Version = Version { versionComponents :: [Int] }
  deriving (Read, Show, Ord, Eq)

prettyVersion :: Version -> String
prettyVersion = intercalate "." . map show . versionComponents

loadLatestVersions :: FilePath -> IO (Map String Version)
loadLatestVersions = fmap (Map.fromListWith max) . loadCabalEntries

parseVersion :: String -> Maybe Version
parseVersion = fmap Version . mapM readMaybe . splitOn "."

--
--
--

loadCabalEntries :: FilePath -> IO [(String, Version)]
loadCabalEntries = parsePaths <=< tarGzPaths <=< L.readFile
  where
  tarGzPaths = entriesToPaths . Tar.read . decompress
  parsePaths = return . mapMaybe pathToPackageVersion

entriesToPaths :: Monad m => Tar.Entries Tar.FormatError -> m [FilePath]
entriesToPaths Tar.Done        = return []
entriesToPaths (Tar.Fail e)    = fail (show e)
entriesToPaths (Tar.Next e es) = (Tar.entryPath e :) `liftM` entriesToPaths es

pathToPackageVersion :: FilePath -> Maybe (String, Version)
pathToPackageVersion path = do
  [name, versionStr, cabal] <- return (splitDirectories path)
  let cabalSuffix = "" <.> "cabal"
  guard (cabalSuffix `isSuffixOf` cabal)
  version <- parseVersion versionStr
  return (name, version)
