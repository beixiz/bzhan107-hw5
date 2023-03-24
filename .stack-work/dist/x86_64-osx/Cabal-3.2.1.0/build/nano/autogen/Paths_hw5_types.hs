{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_hw5_types (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/beixi/Documents/bzhan107-hw5/.stack-work/install/x86_64-osx/f34287af1da82f13b2a88c84d3e0938a14347170539707f794dcfd1b439d60cd/8.10.4/bin"
libdir     = "/Users/beixi/Documents/bzhan107-hw5/.stack-work/install/x86_64-osx/f34287af1da82f13b2a88c84d3e0938a14347170539707f794dcfd1b439d60cd/8.10.4/lib/x86_64-osx-ghc-8.10.4/hw5-types-0.1.0.0-5aJYXbCJlQB720w9YxnKvx-nano"
dynlibdir  = "/Users/beixi/Documents/bzhan107-hw5/.stack-work/install/x86_64-osx/f34287af1da82f13b2a88c84d3e0938a14347170539707f794dcfd1b439d60cd/8.10.4/lib/x86_64-osx-ghc-8.10.4"
datadir    = "/Users/beixi/Documents/bzhan107-hw5/.stack-work/install/x86_64-osx/f34287af1da82f13b2a88c84d3e0938a14347170539707f794dcfd1b439d60cd/8.10.4/share/x86_64-osx-ghc-8.10.4/hw5-types-0.1.0.0"
libexecdir = "/Users/beixi/Documents/bzhan107-hw5/.stack-work/install/x86_64-osx/f34287af1da82f13b2a88c84d3e0938a14347170539707f794dcfd1b439d60cd/8.10.4/libexec/x86_64-osx-ghc-8.10.4/hw5-types-0.1.0.0"
sysconfdir = "/Users/beixi/Documents/bzhan107-hw5/.stack-work/install/x86_64-osx/f34287af1da82f13b2a88c84d3e0938a14347170539707f794dcfd1b439d60cd/8.10.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hw5_types_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hw5_types_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hw5_types_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hw5_types_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hw5_types_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hw5_types_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
