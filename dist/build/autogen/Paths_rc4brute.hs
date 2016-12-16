{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_rc4brute (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
version = Version [0,1,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/emedvedev/Library/Haskell/bin"
libdir     = "/Users/emedvedev/Library/Haskell/ghc-8.0.1-x86_64/lib/rc4brute-0.1.0"
datadir    = "/Users/emedvedev/Library/Haskell/share/ghc-8.0.1-x86_64/rc4brute-0.1.0"
libexecdir = "/Users/emedvedev/Library/Haskell/libexec"
sysconfdir = "/Users/emedvedev/Library/Haskell/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "rc4brute_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "rc4brute_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "rc4brute_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "rc4brute_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "rc4brute_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
