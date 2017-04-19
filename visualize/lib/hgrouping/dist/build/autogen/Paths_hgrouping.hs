module Paths_hgrouping (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,2], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/cosc301/.cabal/bin"
libdir     = "/home/cosc301/.cabal/lib/hgrouping-0.2/ghc-7.6.3"
datadir    = "/home/cosc301/.cabal/share/hgrouping-0.2"
libexecdir = "/home/cosc301/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "hgrouping_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hgrouping_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "hgrouping_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hgrouping_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
