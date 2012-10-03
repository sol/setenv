{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module System.SetEnv (
  setEnv
, unsetEnv
) where

#ifdef mingw32_HOST_OS
import GHC.Windows
import Foreign.Safe
import Foreign.C
import Control.Monad
#else
import qualified System.Posix.Env as Posix
#endif

#ifdef mingw32_HOST_OS
# if defined(i386_HOST_ARCH)
#  define WINDOWS_CCONV stdcall
# elif defined(x86_64_HOST_ARCH)
#  define WINDOWS_CCONV ccall
# else
#  error Unknown mingw32 arch
# endif

foreign import WINDOWS_CCONV unsafe "windows.h GetLastError"
  c_GetLastError:: IO DWORD

eRROR_ENVVAR_NOT_FOUND :: DWORD
eRROR_ENVVAR_NOT_FOUND = 203

#endif

-- | Set the contents of the specified environment variable.
setEnv :: String -> String -> IO ()
setEnv key_ value_
  | null value = unsetEnv key
  | otherwise  = setEnv_ key value
  where
    key   = key_ -- takeWhile (/= '\NUL') key_
    value = takeWhile (/= '\NUL') value_

setEnv_ :: String -> String -> IO ()
#ifdef mingw32_HOST_OS
setEnv_ key value = withCWString key $ \k -> withCWString value $ \v -> do
  success <- c_SetEnvironmentVariable k v
  unless success (throwGetLastError "setEnv")

foreign import WINDOWS_CCONV unsafe "windows.h SetEnvironmentVariableW"
  c_SetEnvironmentVariable :: LPTSTR -> LPTSTR -> IO Bool
#else
setEnv_ k v = Posix.setEnv k v True
#endif

-- | Remove the specified environment variable from the environment of the
-- current process.
unsetEnv :: String -> IO ()
#ifdef mingw32_HOST_OS
unsetEnv key = withCWString key $ \k -> do
  success <- c_SetEnvironmentVariable k nullPtr
  unless success $ do
    -- unsetting an environment variable that does not exist is not an error,
    -- so we ignore eRROR_ENVVAR_NOT_FOUND
    err <- c_GetLastError
    unless (err == eRROR_ENVVAR_NOT_FOUND) $ do
      throwGetLastError "unsetEnv"
#else
unsetEnv = Posix.unsetEnv
#endif
