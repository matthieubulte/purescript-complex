module Test.Properties.Ring (verifyRing) where

import Prelude
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.Properties.Semiring

verifyAdditiveInverse :: forall a. (Arbitrary a, Ring a, Eq a, Show a) => a -> Result
verifyAdditiveInverse x = x - x == zero
                        <?> "x - x /= 0. Pathological case is:"
                        <> "\nx = " <> show x
                        <> "\nx - x = " <> show (x - x)

verifyRing :: forall a eff. (Arbitrary a, Ring a, Eq a, Show a) => Gen a -> QC eff Unit
verifyRing x = do
  verifySemiring x
  quickCheck (verifyAdditiveInverse :: a -> Result)
