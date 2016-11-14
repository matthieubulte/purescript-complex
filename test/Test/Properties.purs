module Test.Properties where

import Prelude
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary

equality :: forall a. (Arbitrary a, Eq a, Show a) => a -> Result
equality a = a == a
          <?> "a /= a. Pathological case is: " <> show a

associative :: forall a. (Arbitrary a, Eq a, Show a) => String -> (a -> a -> a) -> a -> a -> a -> Result
associative opStr op x y z = x `op` (y `op` z) == (x `op` y) `op` z
                          <?> opStr <> " is not associative. Pathological case is:"
                          <> "\nx = " <> show x
                          <> "\ny = " <> show y
                          <> "\nz = " <> show z
                          <> "\nx " <> opStr <> " (y " <> opStr <> " z) = " <> show (x `op` (y `op` z))
                          <> "\n(x " <> opStr <> " y) " <> opStr <> " z = " <> show ((x `op` y) `op` z)


commutative :: forall a. (Arbitrary a, Eq a, Show a) => String -> (a -> a -> a) -> a -> a -> Result
commutative opStr op x y = x `op` y == y `op` x
                          <?> opStr <> " is not commutative. Pathological case is:"
                          <> "\nx = " <> show x
                          <> "\ny = " <> show y
                          <> "\nx " <> opStr <> " y = " <> show (x `op` y)
                          <> "\ny " <> opStr <> " x = " <> show (y `op` x)

checkNum :: forall a eff. (Arbitrary a, Eq a, Semiring a, Show a) => Gen a -> QC eff Unit
checkNum _ = do
  quickCheck (commutative "(*)" (*) :: a -> a -> Result)
