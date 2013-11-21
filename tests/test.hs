{-# LANGUAGE ViewPatterns #-}

import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import System.Environment
import System.Exit
import System.Process

import System.FileLock

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["shared", read -> duration]
      -> doLockTest "shared" Shared duration
    ["exclusive", read -> duration]
      -> doLockTest "exclusive" Exclusive duration
    _ -> void $ mapConcurrently id
      [ callSelf ["shared", "300"]
      , callSelf ["shared", "200"]
      , msleep 10 >> callSelf ["exclusive", "500"]
      , msleep 50 >> callSelf ["shared", "500"]
      , msleep 700 >> callSelf ["shared", "10"]
      ]

callSelf :: [String] -> IO ()
callSelf args = do
  self <- getProgName
  ExitSuccess <- rawSystem ("./" ++ self) args
  return ()

msleep :: Int -> IO ()
msleep = threadDelay . (*1000)

doLockTest :: String -> SharedExclusive -> Int -> IO ()
doLockTest ty sex duration = do
  withFileLock lockfile sex $ \_ -> do
    putStrLn $ "took " ++ desc
    msleep duration
  putStrLn $ "released " ++ desc
  where
    desc = ty ++ " lock"

lockfile :: String
lockfile = "lock"
