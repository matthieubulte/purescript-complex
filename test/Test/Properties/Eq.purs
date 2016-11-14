module Test.Properties.Eq (verifyEq) where

import Prelude
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.Properties


verifyEquality :: forall a eff. (Arbitrary a, Eq a, Show a) => a -> Result
verifyEquality x = x == x
                <?> "x /= x. Pathological case is: " <> show x

verifyEq :: forall a eff. (Arbitrary a, Eq a, Show a) => Gen a -> QC eff Unit
verifyEq _ = quickCheck (verifyEquality :: a -> Result)
