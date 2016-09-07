module Paths_SymbolicHaskell (
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

bindir     = "/home/birk/.cabal/bin"
libdir     = "/home/birk/.cabal/lib/x86_64-linux-ghc-7.10.2/SymbolicHaskell-0.1.0.0-Kh2ixF1exNIB2OcHpIH26N"
datadir    = "/home/birk/.cabal/share/x86_64-linux-ghc-7.10.2/SymbolicHaskell-0.1.0.0"
libexecdir = "/home/birk/.cabal/libexec"
sysconfdir = "/home/birk/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "SymbolicHaskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "SymbolicHaskell_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "SymbolicHaskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "SymbolicHaskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "SymbolicHaskell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
