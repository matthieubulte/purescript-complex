module Test.Data.Complex where

import Prelude hiding (eq)
import Data.Complex
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.Properties.Num
import Test.Properties.Eq

testComplex = do
  let z = (arbitrary :: Gen Complex)
  verifyEq z
  verifyNum z
