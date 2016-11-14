module Test.Properties.Semiring (verifySemiring) where

import Prelude
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.Properties

verifyZero :: forall a. (Arbitrary a, Semiring a, Eq a, Show a) => a -> Result
verifyZero x = x + zero == x && zero + x == x
              <?> "x + 0 /= x. Pathological case is:"
              <> "\nx = " <> show x
              <> "\nx + 0 = " <> show (x + zero)

verifyOne :: forall a. (Arbitrary a, Semiring a, Eq a, Show a) => a -> Result
verifyOne x = x * one == x && one * x == x
              <?> "x * 1 /= x. Pathological case is:"
              <> "\nx = " <> show x
              <> "\nx * 1 = " <> show (x * one)

verifyMulLeftDistributive :: forall a. (Arbitrary a, Semiring a, Eq a, Show a) => a -> a -> a -> Result
verifyMulLeftDistributive x y z = x * (y + z) == x * y + x * z
                    <?> "x * (y + z) /= x * y + x * z. Pathological case is:"
                    <> "\nx = " <> show x
                    <> "\ny = " <> show y
                    <> "\nz = " <> show z
                    <> "\nx * (y + z) = " <> show (x * (y + z))
                    <> "\nx * y + x * z = " <> show (x * y + x * z)

verifyMulRightDistributive :: forall a. (Arbitrary a, Semiring a, Eq a, Show a) => a -> a -> a -> Result
verifyMulRightDistributive x y z = (x + y) * z == x * z + y * z
                    <?> "(x + y) * z == x * z + y * z. Pathological case is:"
                    <> "\nx = " <> show x
                    <> "\ny = " <> show y
                    <> "\nz = " <> show z
                    <> "\n(x + y) * z = " <> show ((x + y) * z)
                    <> "\nx * z + y * z = " <> show (x * z + y * z)

verifyAnnihiliation :: forall a. (Arbitrary a, Semiring a, Eq a, Show a) => a -> Result
verifyAnnihiliation x = x * zero == zero && zero * x == zero
                        <?> "x * 0 /= 0. Pathological case is:"
                        <> "\nx = " <> show x
                        <> "\nx * 0 = " <> show (x * zero)

verifySemiring :: forall a eff. (Arbitrary a, Semiring a, Eq a, Show a) => Gen a -> QC eff Unit
verifySemiring _ = do
  quickCheck (associative "(+)" (+) :: a -> a -> a -> Result)
  quickCheck (verifyZero :: a -> Result)
  quickCheck (commutative "(+)" (+) :: a -> a -> Result)
  quickCheck (associative "(*)" (*) :: a -> a -> a -> Result)
  quickCheck (verifyOne :: a -> Result)
  quickCheck (verifyMulLeftDistributive :: a -> a -> a -> Result)
  quickCheck (verifyMulRightDistributive :: a -> a -> a -> Result)
  quickCheck (verifyAnnihiliation :: a -> Result)
