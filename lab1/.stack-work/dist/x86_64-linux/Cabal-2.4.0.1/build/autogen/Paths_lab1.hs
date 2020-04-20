{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_lab1 (
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

bindir     = "/home/daryasy/Functional_Programming/lab1/.stack-work/install/x86_64-linux/b40832058456b1df67f7fd3547ab4db5821530293442e404aa18d0e6b948015a/8.6.5/bin"
libdir     = "/home/daryasy/Functional_Programming/lab1/.stack-work/install/x86_64-linux/b40832058456b1df67f7fd3547ab4db5821530293442e404aa18d0e6b948015a/8.6.5/lib/x86_64-linux-ghc-8.6.5/lab1-0.1.0.0-L160RguoaWO8J13YC4aUzQ"
dynlibdir  = "/home/daryasy/Functional_Programming/lab1/.stack-work/install/x86_64-linux/b40832058456b1df67f7fd3547ab4db5821530293442e404aa18d0e6b948015a/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/daryasy/Functional_Programming/lab1/.stack-work/install/x86_64-linux/b40832058456b1df67f7fd3547ab4db5821530293442e404aa18d0e6b948015a/8.6.5/share/x86_64-linux-ghc-8.6.5/lab1-0.1.0.0"
libexecdir = "/home/daryasy/Functional_Programming/lab1/.stack-work/install/x86_64-linux/b40832058456b1df67f7fd3547ab4db5821530293442e404aa18d0e6b948015a/8.6.5/libexec/x86_64-linux-ghc-8.6.5/lab1-0.1.0.0"
sysconfdir = "/home/daryasy/Functional_Programming/lab1/.stack-work/install/x86_64-linux/b40832058456b1df67f7fd3547ab4db5821530293442e404aa18d0e6b948015a/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "lab1_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "lab1_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "lab1_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "lab1_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "lab1_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "lab1_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
