module SPC_Tests (tests) where

import Control.Concurrent (threadDelay)
import Data.IORef
import SPC
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  localOption (mkTimeout 3000000) $
    testGroup
      "SPC (core)"
      [ testCase "worker test" $ do
          spc <- startSPC
          ref <- newIORef False
          _ <- workerAdd spc "worker1"
          j <- jobAdd spc $ Job (writeIORef ref True) 1
          r1 <- jobStatus spc j
          r1 @?= JobRunning
          r2 <- jobWait spc j
          r2 @?= Just Done
          v <- readIORef ref
          v @?= True
          j2 <- jobAdd spc $ Job (writeIORef ref False) 1
          r3 <- jobStatus spc j2
          r3 @?= JobRunning
          r4 <- jobWait spc j2
          r4 @?= Just Done
          v2 <- readIORef ref
          v2 @?= False,
        testCase "Cancel job" $ do
          spc <- startSPC
          ref <- newIORef False
          _ <- workerAdd spc "worker1"
          j <- jobAdd spc $ Job (threadDelay 20000000 >> writeIORef ref True) 1
          r1 <- jobStatus spc j
          r1 @?= JobRunning
          jobCancel spc j
          r2 <- jobWait spc j
          r2 @?= Just DoneCancelled,
        testCase "Timeout" $ do
          spc <- startSPC
          ref <- newIORef False
          _ <- workerAdd spc "worker1"
          j <- jobAdd spc $ Job (threadDelay 2000000 >> writeIORef ref True) 1
          r1 <- jobStatus spc j
          r1 @?= JobRunning
          r2 <- jobWait spc j
          r2 @?= Just DoneTimeout,
        testCase "Crash" $ do
          spc <- startSPC
          _ <- workerAdd spc "worker1"
          j1 <- jobAdd spc $ Job (error "boom") 1
          r1 <- jobWait spc j1
          r1 @?= Just DoneCrashed
          -- Ensure new jobs can still work with the one worker
          ref <- newIORef False
          j2 <- jobAdd spc $ Job (writeIORef ref True) 1
          r2 <- jobWait spc j2
          r2 @?= Just Done
          v <- readIORef ref
          v @?= True,
        testCase "Shutdown" $ do
          spc <- startSPC
          ref <- newIORef False
          Right worker <- workerAdd spc "worker1"
          j <- jobAdd spc $ Job (threadDelay 2000000 >> writeIORef ref True) 1
          r1 <- jobStatus spc j
          r1 @?= JobRunning
          _ <- workerStop worker
          r2 <- jobWait spc j
          r2 @?= Just DoneCancelled
      ]
