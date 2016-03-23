module Paths_cplug (
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

bindir     = "/Users/michaelklein/Coding/dispc/.stack-work/install/x86_64-osx/lts-4.0/7.10.3/bin"
libdir     = "/Users/michaelklein/Coding/dispc/.stack-work/install/x86_64-osx/lts-4.0/7.10.3/lib/x86_64-osx-ghc-7.10.3/cplug-0.1.0.0-9sUAxtbz0wBBSNmmqMCrWS"
datadir    = "/Users/michaelklein/Coding/dispc/.stack-work/install/x86_64-osx/lts-4.0/7.10.3/share/x86_64-osx-ghc-7.10.3/cplug-0.1.0.0"
libexecdir = "/Users/michaelklein/Coding/dispc/.stack-work/install/x86_64-osx/lts-4.0/7.10.3/libexec"
sysconfdir = "/Users/michaelklein/Coding/dispc/.stack-work/install/x86_64-osx/lts-4.0/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cplug_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cplug_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "cplug_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cplug_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cplug_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
