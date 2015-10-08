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

conjugatePreservesMagnitude :: Complex -> Result
conjugatePreservesMagnitude z = magnitude (conjugate z) == magnitude z
                             <?> "magnitude (conjugate z) /= magnitude z. Pathological case is:"
                             ++ "\nz = " ++ show z
                             ++ "\nconjugate z = " ++ show (conjugate z)
                             ++ "\nmagnitude z = " ++ show (magnitude z)
                             ++ "\nmagnitude (conjugate z) = " ++ show (magnitude (conjugate z))

conjugateIsInvolution :: Complex -> Result
conjugateIsInvolution z = conjugate (conjugate z) == z
                       <?> "conjugate (conjugate z) /= z. Pathological case is:"
                       ++ "\nz = " ++ show z
                       ++ "\nconjugate (conjugate z) = " ++ show (conjugate (conjugate z))

testComplex = do
  let z = (arbitrary :: Gen Complex)
  verifyEq z
  verifyNum z

  quickCheck polarIsomorphism
  quickCheck cartesianIsomorphism
  quickCheck conjugatePreservesMagnitude
  quickCheck conjugateIsInvolution
