module APL.Tests where

import APL.AST (Exp (..), VName)
import APL.Eval
import Test.QuickCheck (Gen, elements, listOf, oneof, sample, sized)
import Test.QuickCheck.Arbitrary

genVar :: Gen VName
genVar = do
  alpha <- elements ['a' .. 'z']
  alphaNums <- listOf $ elements $ ['a' .. 'z'] ++ ['0' .. '9']
  pure $ alpha : alphaNums

genExp :: Int -> Gen Exp
genExp n =
  if n <= 1
    then oneof [Var <$> genVar, CstInt <$> (arbitrary :: Gen Integer), CstBool <$> (arbitrary :: Gen Bool)]
    else
      let half = (n - 1) `div` 2
          third = (n - 1) `div` 3
       in oneof
            [ Var <$> genVar,
              CstInt <$> (arbitrary :: Gen Integer),
              CstBool <$> (arbitrary :: Gen Bool),
              Lambda <$> genVar <*> genExp (n - 1),
              Apply <$> genExp half <*> genExp half,
              Let <$> genVar <*> genExp half <*> genExp half,
              If <$> genExp third <*> genExp third <*> genExp third,
              Add <$> genExp half <*> genExp half,
              Sub <$> genExp half <*> genExp half,
              Mul <$> genExp half <*> genExp half,
              Div <$> genExp half <*> genExp half,
              Pow <$> genExp half <*> genExp half,
              Eql <$> genExp half <*> genExp half,
              TryCatch <$> genExp half <*> genExp half
            ]

prop_integerAddAssoc :: Integer -> Integer -> Integer -> Bool
prop_integerAddAssoc x y z = x + (y + z) == (x + y) + z

prop_aplAddAssoc :: Exp -> Exp -> Exp -> Bool
prop_aplAddAssoc x y z = runEval (eval (Add x (Add y z))) == runEval (eval (Add (Add x y) z))

instance Arbitrary Exp where
  arbitrary = sized genExp
  shrink (Var x) = [Var x' | x' <- shrink x, not (null x')]
  shrink (CstInt x) = CstInt <$> shrink x
  shrink (CstBool x) = CstBool <$> shrink x
  shrink (Add x y) = [x, y] ++ [Add x' y | x' <- shrink x] ++ [Add x y' | y' <- shrink y]
  shrink (Sub x y) = [x, y] ++ [Sub x' y | x' <- shrink x] ++ [Sub x y' | y' <- shrink y]
  shrink (Mul x y) = [x, y] ++ [Mul x' y | x' <- shrink x] ++ [Mul x y' | y' <- shrink y]
  shrink (Div x y) = [x, y] ++ [Div x' y | x' <- shrink x] ++ [Div x y' | y' <- shrink y]
  shrink (Pow x y) = [x, y] ++ [Pow x' y | x' <- shrink x] ++ [Pow x y' | y' <- shrink y]
  shrink (Eql x y) = [x, y] ++ [Eql x' y | x' <- shrink x] ++ [Eql x y' | y' <- shrink y]
  shrink (If x y z) = [y, z] ++ [If x' y z | x' <- shrink x] ++ [If x y' z | y' <- shrink y] ++ [If x y z' | z' <- shrink z]
  shrink (Let x y z) = y : [Let x y' z | y' <- shrink y] ++ [Let x y z' | z' <- shrink z] ++ [Let x' y z | x' <- shrink x, not (null x')]
  shrink (Lambda x y) = y : [Lambda x' y | x' <- shrink x, not (null x')] ++ [Lambda x y' | y' <- shrink y]
  shrink (Apply x y) = [x, y] ++ [Apply x' y | x' <- shrink x] ++ [Apply x y' | y' <- shrink y]
  shrink (TryCatch x y) = [x, y] ++ [TryCatch x' y | x' <- shrink x] ++ [TryCatch x y' | y' <- shrink y]
