module APL.Tests
  ( properties,
  )
where

import APL.AST (Exp (..), VName, printExp, subExp)
import APL.Check (checkExp)
import APL.Error (isDomainError, isTypeError, isVariableError)
import APL.Eval (eval, runEval)
import APL.Parser (parseAPL)
import Test.QuickCheck (Arbitrary (arbitrary, shrink), Gen, Property, checkCoverage, cover, elements, frequency, oneof, property, sized, vectorOf)
import Test.QuickCheck.Gen (choose)

instance Arbitrary Exp where
  arbitrary = sized (genExp [])

  shrink (Add e1 e2) =
    e1 : e2 : [Add e1' e2 | e1' <- shrink e1] ++ [Add e1 e2' | e2' <- shrink e2]
  shrink (Sub e1 e2) =
    e1 : e2 : [Sub e1' e2 | e1' <- shrink e1] ++ [Sub e1 e2' | e2' <- shrink e2]
  shrink (Mul e1 e2) =
    e1 : e2 : [Mul e1' e2 | e1' <- shrink e1] ++ [Mul e1 e2' | e2' <- shrink e2]
  shrink (Div e1 e2) =
    e1 : e2 : [Div e1' e2 | e1' <- shrink e1] ++ [Div e1 e2' | e2' <- shrink e2]
  shrink (Pow e1 e2) =
    e1 : e2 : [Pow e1' e2 | e1' <- shrink e1] ++ [Pow e1 e2' | e2' <- shrink e2]
  shrink (Eql e1 e2) =
    e1 : e2 : [Eql e1' e2 | e1' <- shrink e1] ++ [Eql e1 e2' | e2' <- shrink e2]
  shrink (If cond e1 e2) =
    e1 : e2 : [If cond' e1 e2 | cond' <- shrink cond] ++ [If cond e1' e2 | e1' <- shrink e1] ++ [If cond e1 e2' | e2' <- shrink e2]
  shrink (Let x e1 e2) =
    e1 : [Let x e1' e2 | e1' <- shrink e1] ++ [Let x e1 e2' | e2' <- shrink e2]
  shrink (Lambda x e) =
    [Lambda x e' | e' <- shrink e]
  shrink (Apply e1 e2) =
    e1 : e2 : [Apply e1' e2 | e1' <- shrink e1] ++ [Apply e1 e2' | e2' <- shrink e2]
  shrink (TryCatch e1 e2) =
    e1 : e2 : [TryCatch e1' e2 | e1' <- shrink e1] ++ [TryCatch e1 e2' | e2' <- shrink e2]
  shrink _ = []

keywords :: [String]
keywords =
  [ "if",
    "then",
    "else",
    "true",
    "false",
    "let",
    "in",
    "try",
    "catch"
  ]

genDivZero :: Gen Exp
genDivZero = pure $ Div (CstInt 1) (CstInt 0)

genNegExp :: Gen Exp
genNegExp = pure $ Pow (CstInt 1) (CstInt (-1))

-- genVar :: Gen VName
-- genVar = oneof [vectorOf n arbitrary | n <- [2 .. 4]]

genVar :: Gen VName
genVar = do
  n <- choose (2, 4)
  alpha <- elements ['a' .. 'z']
  alphaNums <- vectorOf (n - 1) $ elements $ ['a' .. 'z'] ++ ['0' .. '9']

  let v = alpha : alphaNums
   in if v `elem` keywords
        then genVar
        else
          pure $ alpha : alphaNums

genExp :: [VName] -> Int -> Gen Exp
genExp env 0 = oneof [CstInt <$> arbitrary, CstBool <$> arbitrary, if null env then Var <$> genVar else Var <$> oneof [pure v | v <- env]]
genExp env size =
  frequency
    [ -- Domain errors
      (5, oneof [genDivZero, genNegExp]),
      (5, CstInt <$> arbitrary),
      (5, CstBool <$> arbitrary),
      ( 6,
        oneof
          [ Add <$> genExp env halfSize <*> genExp env halfSize,
            Sub <$> genExp env halfSize <*> genExp env halfSize,
            Mul <$> genExp env halfSize <*> genExp env halfSize,
            Div <$> genExp env halfSize <*> genExp env halfSize,
            Pow <$> genExp env halfSize <*> genExp env halfSize,
            Eql <$> genExp env halfSize <*> genExp env halfSize
          ]
      ),
      (5, If <$> genExp env thirdSize <*> genExp env thirdSize <*> genExp env thirdSize),
      (1, Var <$> genVar),
      (if null env then 0 else 5, Var <$> oneof [pure v | v <- env]),
      (5, do v <- genVar; exp1 <- genExp env halfSize; exp2 <- genExp (v : env) halfSize; pure $ Let v exp1 exp2),
      (5, do v <- genVar; exp1 <- genExp (v : env) halfSize; pure $ Lambda v exp1),
      (5, Apply <$> genExp env halfSize <*> genExp env halfSize),
      (5, TryCatch <$> genExp env halfSize <*> genExp env halfSize)
    ]
  where
    halfSize = size `div` 2
    thirdSize = size `div` 3

expCoverage :: Exp -> Property
expCoverage e =
  checkCoverage
    . cover 20 (any isDomainError (checkExp e)) "domain error"
    . cover 20 (not $ any isDomainError (checkExp e)) "no domain error"
    . cover 20 (any isTypeError (checkExp e)) "type error"
    . cover 20 (not $ any isTypeError (checkExp e)) "no type error"
    . cover 5 (any isVariableError (checkExp e)) "variable error"
    . cover 70 (not $ any isVariableError (checkExp e)) "no variable error"
    . cover 50 (or [2 <= n && n <= 4 | Var v <- subExp e, let n = length v]) "non-trivial variable"
    $ ()

parsePrinted :: Exp -> Bool
parsePrinted e =
  let s = printExp e
   in case parseAPL "" s of
        Left _ -> False
        Right e' -> e == e'

onlyCheckedErrors :: Exp -> Bool
onlyCheckedErrors e =
  let result = runEval $ eval e
   in case result of
        Left actualError -> actualError `elem` checkExp e
        Right _ -> True

properties :: [(String, Property)]
properties =
  [ ("expCoverage", property expCoverage),
    ("onlyCheckedErrors", property onlyCheckedErrors),
    ("parsePrinted", property parsePrinted)
  ]
