module APL.Interp_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpIO (runEvalIO)
import APL.InterpPure (runEval)
import APL.Monad
import APL.Util (captureIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

eval' :: Exp -> Val
eval' = runEval . eval

evalIO' :: Exp -> IO (Either Error Val)
evalIO' = runEvalIO . eval

tests :: TestTree
tests = testGroup "Free monad interpreters" [pureTests, ioTests]

pureTests :: TestTree
pureTests =
  testGroup
    "Pure interpreter"
    [ testGroup
        "ReaderOp"
        [ testCase "Let" $
            eval' (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
              @?= ValInt 5,
          testCase
            "LocalEnv"
            $ runEval
              (localEnv (const [("x", ValInt 1)]) $ askEnv)
              @?= [("x", ValInt 1)]
        ]
    ]

ioTests :: TestTree
ioTests =
  testGroup
    "IO interpreter"
    []
