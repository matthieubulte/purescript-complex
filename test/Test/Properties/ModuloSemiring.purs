module Test.Properties.ModuloSemiring (verifyModuloSemiring) where

import Prelude
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.Properties.Semiring

verifyRemainder :: forall a. (Arbitrary a, ModuloSemiring a, Eq a, Show a) => a -> a -> Result
verifyRemainder x y =  x / y * y + (x `mod` y) == x
                        <?> "x / y * y + (x `mod` y) /= x. Pathological case is:"
                        ++ "\nx = " ++ show x
                        ++ "\ny = " ++ show y
                        ++ "\nx / y * y + (x `mod` y) = " ++ show (x / y * y + (x `mod` y))

verifyModuloSemiring :: forall a eff. (Arbitrary a, ModuloSemiring a, Eq a, Show a) => Gen a -> QC eff Unit
verifyModuloSemiring x = do
  verifySemiring x
  quickCheck (verifyRemainder :: a -> a -> Result)
