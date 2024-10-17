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
      "SPC"
      [ testCase "Add job" $ do
          spc <- startSPC
          j <- jobAdd spc (Job (pure ()) 1)
          status <- jobStatus spc j
          status @?= JobPending
          jobCancel spc j
          newStatus <- jobStatus spc j
          newStatus @?= JobDone DoneCancelled
      ]
