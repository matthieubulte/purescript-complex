module Test.Properties.DivisionRing (verifyDivisionRing) where

import Prelude
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.Properties.Ring
import Test.Properties.ModuloSemiring

verifyMulInverse :: forall a. (Arbitrary a, DivisionRing a, Eq a, Show a) => a -> Result
verifyMulInverse x =  (one / x) * x == one
                        <?> "(one / x) * x /= one. Pathological case is:"
                        ++ "\nx = " ++ show x
                        ++ "\n(one / x) * x = " ++ show ((one / x) * x)

verifyDivisionRing :: forall a eff. (Arbitrary a, DivisionRing a, Eq a, Show a) => Gen a -> QC eff Unit
verifyDivisionRing x = do
  verifyRing x
  verifyModuloSemiring x
  quickCheck (verifyMulInverse :: a -> Result)
