module Test.Data.Complex where

import Prelude hiding (eq)
import Data.Complex
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.Properties.Num
import Test.Properties.Eq

polarIsomorphism :: Complex -> Result
polarIsomorphism z = (outPolar <<< inPolar) z == z
                    <?> "outPolar . inPolar = id. Pathological case is:"
                    ++ "\nz = " ++ show z
                    ++ "\n(outPolar . inPolar) z = " ++ show ((outPolar <<< inPolar) z)

cartesianIsomorphism :: Complex -> Result
cartesianIsomorphism z = (outCartesian <<< inCartesian) z == z
                    <?> "outCartesian . inCartesian = id. Pathological case is:"
                    ++ "\nz = " ++ show z
                    ++ "\n(outCartesian . inCartesian) z = " ++ show ((outCartesian <<< inCartesian) z)

testComplex = do
  let z = (arbitrary :: Gen Complex)
  verifyEq z
  verifyNum z

  quickCheck polarIsomorphism
  quickCheck cartesianIsomorphism
