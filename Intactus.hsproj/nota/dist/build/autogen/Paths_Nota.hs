{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Nota (
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
version = Version [1,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/pmk/Library/Haskell/bin"
libdir     = "/Users/pmk/Library/Containers/com.haskellformac.Haskell.basic/Data/Library/Application Support/lib/ghc/Nota-1.0"
dynlibdir  = "/Users/pmk/Library/Containers/com.haskellformac.Haskell.basic/Data/Library/Application Support/lib/ghc/Nota-1.0"
datadir    = "/Users/pmk/Library/Containers/com.haskellformac.Haskell.basic/Data/Library/Application Support/share/Nota-1.0"
libexecdir = "/Users/pmk/Library/Containers/com.haskellformac.Haskell.basic/Data/Library/Application Support/libexec"
sysconfdir = "/Users/pmk/Library/Containers/com.haskellformac.Haskell.basic/Data/Library/Application Support/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Nota_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Nota_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Nota_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Nota_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Nota_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Nota_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
