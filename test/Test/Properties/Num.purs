module Test.Properties.Num (verifyNum) where

import Prelude
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.Properties
import Test.Properties.DivisionRing

verifyNum :: forall a eff. (Arbitrary a, Num a, Eq a, Show a) => Gen a -> QC eff Unit
verifyNum x = do
  verifyDivisionRing x
  quickCheck (commutative "(*)" (*) :: a -> a -> Result)
