{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_Ex04 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/fai/Desktop/3141/Ex04/.stack-work/install/aarch64-osx/9bc10b5ccb25316fc89a6c8a1edd98914f82ac0b6736b8d63bbe760846667c9c/9.4.8/bin"
libdir     = "/Users/fai/Desktop/3141/Ex04/.stack-work/install/aarch64-osx/9bc10b5ccb25316fc89a6c8a1edd98914f82ac0b6736b8d63bbe760846667c9c/9.4.8/lib/aarch64-osx-ghc-9.4.8/Ex04-1.0-78qAPVaAyCYFGPi7z4m31q-Ex04"
dynlibdir  = "/Users/fai/Desktop/3141/Ex04/.stack-work/install/aarch64-osx/9bc10b5ccb25316fc89a6c8a1edd98914f82ac0b6736b8d63bbe760846667c9c/9.4.8/lib/aarch64-osx-ghc-9.4.8"
datadir    = "/Users/fai/Desktop/3141/Ex04/.stack-work/install/aarch64-osx/9bc10b5ccb25316fc89a6c8a1edd98914f82ac0b6736b8d63bbe760846667c9c/9.4.8/share/aarch64-osx-ghc-9.4.8/Ex04-1.0"
libexecdir = "/Users/fai/Desktop/3141/Ex04/.stack-work/install/aarch64-osx/9bc10b5ccb25316fc89a6c8a1edd98914f82ac0b6736b8d63bbe760846667c9c/9.4.8/libexec/aarch64-osx-ghc-9.4.8/Ex04-1.0"
sysconfdir = "/Users/fai/Desktop/3141/Ex04/.stack-work/install/aarch64-osx/9bc10b5ccb25316fc89a6c8a1edd98914f82ac0b6736b8d63bbe760846667c9c/9.4.8/etc"

getBinDir     = catchIO (getEnv "Ex04_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "Ex04_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "Ex04_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "Ex04_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Ex04_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Ex04_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
