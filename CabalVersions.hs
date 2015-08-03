module CabalVersions where

import Codec.Compression.GZip (decompress)
import Control.Monad
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import Data.Version
import System.FilePath
import qualified Codec.Archive.Tar as Tar
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map
import Text.ParserCombinators.ReadP (readP_to_S, eof)

--
-- Version strings
--

loadLatestVersions :: FilePath -> IO (Map String Version)
loadLatestVersions = fmap (Map.fromListWith max) . loadCabalEntries

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
  guard (cabal == name <.> "cabal")
  version <- parseVersion' versionStr
  return (name, version)

parseVersion' :: String -> Maybe Version
parseVersion' str =
  case readP_to_S (parseVersion <* eof) str of
    [(x,"")] -> Just x
    _        -> Nothing
