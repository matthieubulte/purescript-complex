module Test.Properties.Num (verifyNum) where

import Prelude
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.Properties


verifyNum :: forall a eff. (Arbitrary a, Eq a, Semiring a, Show a) => Gen a -> QC eff Unit
verifyNum x = do
  quickCheck (commutative "(*)" (*) :: a -> a -> Result)
