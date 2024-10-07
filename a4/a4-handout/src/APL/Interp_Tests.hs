module APL.Interp_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpIO (runEvalIO)
import APL.InterpPure (runEval)
import APL.Monad
import APL.Util (captureIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

eval' :: Exp -> ([String], Either Error Val)
eval' = runEval . eval

evalIO' :: Exp -> IO (Either Error Val)
evalIO' = runEvalIO . eval

tests :: TestTree
tests = testGroup "Free monad interpreters" [pureTests, ioTests]

pureTests :: TestTree
pureTests =
  testGroup
    "Pure interpreter"
    [ testCase "localEnv" $
        runEval
          ( localEnv (const [("x", ValInt 1)]) askEnv
          )
          @?= ([], Right [("x", ValInt 1)]),
      --
      testCase "Let" $
        eval' (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= ([], Right (ValInt 5)),
      --
      testCase "Let (shadowing)" $
        eval'
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= ([], Right (ValBool True)),
      --
      testCase "State" $
        runEval
          ( do
              putState [(ValInt 0, ValInt 1)]
              modifyState $ map (\(key, _) -> (key, ValInt 5))
              getState
          )
          @?= ([], Right [(ValInt 0, ValInt 5)]),
      --
      testCase "Print" $
        runEval (evalPrint "test")
          @?= (["test"], Right ()),
      --
      testCase "Error" $
        runEval
          ( do
              _ <- failure "Oh no!"
              evalPrint "test"
          )
          @?= ([], Left "Oh no!"),
      --
      testCase "Div0" $
        eval' (Div (CstInt 7) (CstInt 0))
          @?= ([], Left "Division by zero"),
      --
      testCase "TryCatch 1" $
        runEval (catch (failure "Oh no!") (pure "Success!"))
          @?= ([], Right "Success!"),
      --
      testCase "TryCatch 2" $
        runEval (catch (pure "Success!") (failure "Oh no!"))
          @?= ([], Right "Success!"),
      --
      testCase "TryCatch 2" $
        eval' (TryCatch (Div (CstInt 1) (CstInt 0)) (CstInt 1))
          @?= ([], Right (ValInt 1)),
      --
      testCase "TryCatch 3" $
        eval' (TryCatch (Add (CstInt 0) (CstBool True)) (Div (CstInt 1) (CstInt 0)))
          @?= ([], Left "Division by zero"),
      --
      testCase "TryCatch KvPut err" $
        eval' (TryCatch (Let "x" (KvPut (CstInt 1) (CstInt 2)) (Div (CstInt 1) (CstInt 0))) (KvGet (CstInt 1)))
          @?= ([], Left "Invalid key: ValInt 1"),
      --
      testCase "KvPut/KvGet" $
        eval' (Let "x" (KvPut (CstInt 1) (CstInt 2)) (KvGet (CstInt 1)))
          @?= ([], Right $ ValInt 2),
      --
      testCase "KvPut/KvGet error" $
        eval' (Let "x" (KvPut (CstInt 1) (CstInt 2)) (KvGet (CstInt 2)))
          @?= ([], Left "Invalid key: ValInt 2"),
      --
      testCase "KvPut override" $
        eval' (Let "x" (KvPut (CstInt 1) (CstInt 2)) (Let "y" (KvPut (CstInt 1) (CstInt 3)) (KvGet (CstInt 1))))
          @?= ([], Right $ ValInt 3),
      --
      testCase "StatePutOp/StateGetOp" $
        runEval
          ( do
              putState [(ValInt 0, ValInt 1)]
              modifyState $ map (\(key, _) -> (key, ValInt 5))
              getState
          )
          @?= ([], Right [(ValInt 0, ValInt 5)]),
      --
      testCase "TransactionOp" $
        runEval
          ( transaction (evalPrint "Weee" >> failure "Die")
          )
          @?= (["Weee"], Right ()),
      --
      testCase "TransactionOp 2" $
        runEval
          ( transaction (evalKvPut (ValInt 0) (ValInt 1)) >> eval (KvGet (CstInt 0))
          )
          @?= ([], Right $ ValInt 1),
      --
      testCase "TransactionOp 3" $
        runEval
          (transaction (evalKvPut (ValInt 0) (ValInt 1) >> failure "die") >> eval (KvGet (CstInt 0)))
          @?= ([], Left "Invalid key: ValInt 0"),
      --
      testCase "Nested transaction" $
        runEval
          ( transaction (transaction (evalKvPut (ValInt 0) (ValInt 1) >> transaction (evalKvPut (ValInt 0) (ValInt 1) >> failure "die"))) >> eval (KvGet (CstInt 0))
          )
          @?= ([], Right $ ValInt 1)
    ]

ioTests :: TestTree
ioTests =
  testGroup
    "IO interpreter"
    [ testCase "print" $ do
        let s1 = "Lalalalala"
            s2 = "Weeeeeeeee"
        (out, res) <-
          captureIO [] $
            runEvalIO $ do
              evalPrint s1
              evalPrint s2
        (out, res) @?= ([s1, s2], Right ()),
      -- NOTE: This test will give a runtime error unless you replace the
      -- version of `eval` in `APL.Eval` with a complete version that supports
      -- `Print`-expressions. Uncomment at your own risk.
      testCase "print 2" $ do
        (out, res) <-
          captureIO [] $
            evalIO' $
              Print "This is also 1" $
                Print "This is 1" $
                  CstInt 1
        (out, res) @?= (["This is 1: 1", "This is also 1: 1"], Right $ ValInt 1),
      --
      testCase "TryCatch" $
        let e = "Oh no!"
         in do
              (out, res) <-
                captureIO [] $
                  runEvalIO $
                    catch (failure e) (evalPrint "test")
              (out, res) @?= (["test"], Right ()),
      --
      testCase "TryCatch KvPut err" $
        let e = "Oh no!"
         in do
              (out, res) <-
                captureIO ["ValInt 3"] $
                  runEvalIO $
                    catch (evalKvPut (ValInt 1) (ValInt 2) >> failure e) (evalKvGet (ValInt 1))
              (out, res) @?= (["Key not found: ValInt 1. Enter a value: "], Right $ ValInt 3),
      --
      testCase "KvPut/KvGet" $
        let v1 = CstInt 1
            v2 = CstInt 2
         in do
              (out, res) <-
                captureIO [] $
                  evalIO' $
                    Let "x" (KvPut v1 v2) (KvGet v1)
              (out, res) @?= ([], Right $ ValInt 2),
      --
      testCase "KvPut/KvGet error" $
        let v1 = CstInt 1
            v2 = CstInt 2
         in do
              (out, res) <-
                captureIO ["ValInt 1"] $
                  evalIO' $
                    Let "x" (KvPut v1 v2) (KvGet (CstInt 2))
              (out, res) @?= (["Key not found: ValInt 2. Enter a value: "], Right $ ValInt 1),
      --
      testCase "KvPut/KvGet 2" $
        let v1 = CstInt 1
            v2 = CstInt 2
            v3 = CstInt 3
         in do
              (out, res) <-
                captureIO [] $
                  evalIO' $
                    Let "x" (KvPut v1 v2) (KvPut v1 v3)
              (out, res) @?= ([], Right $ ValInt 3),
      --
      testCase "KvPut override" $
        let v1 = CstInt 1
            v2 = CstInt 2
            v3 = CstInt 3
         in do
              (out, res) <-
                captureIO [] $
                  evalIO' $
                    Let "x" (KvPut v1 v2) (Let "y" (KvPut v1 v3) (KvGet v1))
              (out, res) @?= ([], Right $ ValInt 3),
      --
      testCase "KvGet Wrong input" $
        let v1 = CstInt 1
         in do
              (out, res) <-
                captureIO ["Wrong"] $
                  evalIO' $
                    KvGet v1
              (out, res) @?= (["Key not found: ValInt 1. Enter a value: "], Left "Invalid value input: Wrong"),
      --
      testCase "StatPutOp/StateGetOp" $
        let s1 = [(ValInt 0, ValInt 1)]
            s2 = [(ValInt 0, ValInt 5)]
         in do
              (out, res) <-
                captureIO [] $
                  runEvalIO $
                    do
                      putState s1
                      modifyState $ map (\(key, _) -> (key, ValInt 5))
                      getState
              (out, res) @?= ([], Right s2),
      --
      testCase "TransactionOp" $ do
        (out, res) <-
          captureIO [] $
            runEvalIO $
              transaction (evalPrint "Weee" >> failure "Die")
        (out, res) @?= (["Weee"], Right ()),
      --
      testCase "TransactionOp 2" $ do
        (out, res) <-
          captureIO [] $
            runEvalIO $
              transaction (evalKvPut (ValInt 0) (ValInt 1)) >> eval (KvGet (CstInt 0))
        (out, res) @?= ([], Right $ ValInt 1),
      --
      testCase "TransactionOp 3" $ do
        (out, res) <-
          captureIO ["Wrong"] $
            runEvalIO $
              transaction (evalKvPut (ValInt 0) (ValInt 1) >> failure "die") >> eval (KvGet (CstInt 0))
        (out, res) @?= (["Key not found: ValInt 0. Enter a value: "], Left "Invalid value input: Wrong"),
      --
      testCase "Nested transaction" $ do
        (out, res) <-
          captureIO [] $
            runEvalIO $
              transaction (transaction (evalKvPut (ValInt 0) (ValInt 1) >> transaction (evalKvPut (ValInt 0) (ValInt 1) >> failure "die"))) >> eval (KvGet (CstInt 0))
        (out, res) @?= ([], Right $ ValInt 1)
    ]
