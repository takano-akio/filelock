{-# LANGUAGE InterruptibleFFI #-}

module System.FileLock.Internal.Flock
#ifndef USE_FLOCK
  () where
#else
  (Lock, lock, tryLock, unlock) where

#include <sys/file.h>

import Control.Applicative
import Control.Concurrent (yield)
import qualified Control.Exception as E
import Data.Bits
import Foreign.C.Error
import Foreign.C.Types
import System.Posix.Files
import System.Posix.IO (openFd, closeFd, defaultFileFlags, OpenMode(..), setFdOption, FdOption(..))
import System.Posix.Types
import Prelude

type Lock = Fd

lock :: FilePath -> Bool -> IO Lock
lock path exclusive = do
  fd <- open path
  (`E.onException` closeFd fd) $ do
    True <- flock fd exclusive True
    return fd

tryLock :: FilePath -> Bool -> IO (Maybe Lock)
tryLock path exclusive = do
  fd <- open path
  (`E.onException` closeFd fd) $ do
    success <- flock fd exclusive False
    if success
      then return $ Just $ fd
      else Nothing <$ closeFd fd

unlock :: Lock -> IO ()
unlock fd = closeFd fd

open :: FilePath -> IO Fd
open path = do
  fd <- openFd path WriteOnly (Just stdFileMode) defaultFileFlags
  -- Ideally, we would open the file descriptor with CLOEXEC enabled, but since
  -- unix 2.8 hasn't been released yet and we want backwards compatibility with
  -- older releases, we set CLOEXEC after opening the file descriptor.  This
  -- may seem like a race condition at first. However, since the lock is always
  -- taken after CLOEXEC is set, the worst that can happen is that a child
  -- process inherits the open FD in an unlocked state. While non-ideal from a
  -- performance standpoint, it doesn't introduce any locking bugs.
  setFdOption fd CloseOnExec True
  return fd

flock :: Fd -> Bool -> Bool -> IO Bool
flock (Fd fd) exclusive block = do
  r <- c_flock fd $ modeOp .|. blockOp
  if r == 0
    then return True -- success
    else do
      errno <- getErrno
      case () of
        _ | errno == eWOULDBLOCK
            -> return False -- already taken
          | errno == eINTR -> do
              -- If InterruptibleFFI interrupted the syscall with EINTR,
              -- we need to give the accompanying Haskell exception a chance to bubble.
              -- See also https://gitlab.haskell.org/ghc/ghc/issues/8684#note_142404.
              E.allowInterrupt
              flock (Fd fd) exclusive block
          | otherwise -> throwErrno "flock"
  where
    modeOp = case exclusive of
      False -> #{const LOCK_SH}
      True -> #{const LOCK_EX}
    blockOp = case block of
      True -> 0
      False -> #{const LOCK_NB}

-- `interruptible` so that async exceptions like `timeout` can stop it
-- when used in conjunction with `LOCK_NB`.
foreign import ccall interruptible "flock"
  c_flock :: CInt -> CInt -> IO CInt

#endif /* USE_FLOCK */
