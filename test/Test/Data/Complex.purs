module Test.Data.Complex where

import Prelude
import Data.Complex
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Laws.Data
import Data.Function (on)
import Data.Newtype (class Newtype, unwrap)
import Data.Ord (abs)
import Type.Proxy (Proxy(..))

-- | Metric spaces.
class Metric a where
  distance :: a -> a -> Number

instance metricNumber :: Metric Number where
  distance x y = abs (x - y)

instance metricComplex :: Metric Complex where
  distance x y = magnitude (x - y)

approxEq :: forall a. Metric a => a -> a -> Boolean
approxEq x y = distance x y < 0.00000001

newtype ApproxComplex = ApproxComplex Complex

derive instance newtypeApproxComplex :: Newtype ApproxComplex _

-- | This instance is not transitive, but this is what we want for the tests
-- | because e.g. floating point addition is not associative.
instance eqApproxComplex :: Eq ApproxComplex where
  eq a b = approxEq (unwrap a) (unwrap b)

derive newtype instance semiringApproxComplex :: Semiring ApproxComplex
derive newtype instance ringApproxComplex :: Ring ApproxComplex
derive newtype instance commutativeRingApproxComplex :: CommutativeRing ApproxComplex
derive newtype instance euclideanRingApproxComplex :: EuclideanRing ApproxComplex
derive newtype instance fieldApproxComplex :: Field ApproxComplex
derive newtype instance arbitraryApproxComplex :: Arbitrary ApproxComplex

polarIsomorphism :: Complex -> Result
polarIsomorphism z = (outPolar <<< inPolar) z `approxEq` z
                  <?> "outPolar . inPolar = id. Pathological case is:"
                  <> "\nz = " <> show z
                  <> "\n(outPolar . inPolar) z = " <> show ((outPolar <<< inPolar) z)

cartesianIsomorphism :: Complex -> Result
cartesianIsomorphism z = (outCartesian <<< inCartesian) z `approxEq` z
                      <?> "outCartesian . inCartesian = id. Pathological case is:"
                      <> "\nz = " <> show z
                      <> "\n(outCartesian . inCartesian) z = " <> show ((outCartesian <<< inCartesian) z)

conjugatePreservesMagnitude :: Complex -> Result
conjugatePreservesMagnitude z = magnitude (conjugate z) `approxEq` magnitude z
                             <?> "magnitude (conjugate z) /= magnitude z. Pathological case is:"
                             <> "\nz = " <> show z
                             <> "\nconjugate z = " <> show (conjugate z)
                             <> "\nmagnitude z = " <> show (magnitude z)
                             <> "\nmagnitude (conjugate z) = " <> show (magnitude (conjugate z))

conjugateIsInvolution :: Complex -> Result
conjugateIsInvolution z = conjugate (conjugate z) `approxEq` z
                       <?> "conjugate (conjugate z) /= z. Pathological case is:"
                       <> "\nz = " <> show z
                       <> "\nconjugate (conjugate z) = " <> show (conjugate (conjugate z))

testComplex = do
  let p = Proxy :: Proxy ApproxComplex
  checkEq p
  checkSemiring p
  checkRing p
  checkEuclideanRing p
  checkCommutativeRing p
  checkField p

  quickCheck polarIsomorphism
  quickCheck cartesianIsomorphism
  quickCheck conjugatePreservesMagnitude
  quickCheck conjugateIsInvolution
