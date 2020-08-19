{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_ekg_core (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,1,7] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/Jonny/.cabal/bin"
libdir     = "/Users/Jonny/.cabal/lib/x86_64-osx-ghc-8.6.5/ekg-core-0.1.1.7-inplace"
dynlibdir  = "/Users/Jonny/.cabal/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/Jonny/.cabal/share/x86_64-osx-ghc-8.6.5/ekg-core-0.1.1.7"
libexecdir = "/Users/Jonny/.cabal/libexec/x86_64-osx-ghc-8.6.5/ekg-core-0.1.1.7"
sysconfdir = "/Users/Jonny/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ekg_core_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ekg_core_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ekg_core_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ekg_core_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ekg_core_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ekg_core_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
