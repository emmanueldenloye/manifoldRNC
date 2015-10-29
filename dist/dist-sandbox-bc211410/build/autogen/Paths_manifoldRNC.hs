module Paths_manifoldRNC (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/emmanuel/Dropbox/haskell/brun_fast_git/.cabal-sandbox/bin"
libdir     = "/home/emmanuel/Dropbox/haskell/brun_fast_git/.cabal-sandbox/lib/x86_64-linux-ghc-7.10.2/manifoldRNC-0.1.0.0-Cp8eAc9aaftLqJqidG85BB"
datadir    = "/home/emmanuel/Dropbox/haskell/brun_fast_git/.cabal-sandbox/share/x86_64-linux-ghc-7.10.2/manifoldRNC-0.1.0.0"
libexecdir = "/home/emmanuel/Dropbox/haskell/brun_fast_git/.cabal-sandbox/libexec"
sysconfdir = "/home/emmanuel/Dropbox/haskell/brun_fast_git/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "manifoldRNC_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "manifoldRNC_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "manifoldRNC_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "manifoldRNC_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "manifoldRNC_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
