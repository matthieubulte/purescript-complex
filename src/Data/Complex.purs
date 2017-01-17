module Data.Complex
  ( Complex ()
  , Cartesian (..)
  , Polar (..)
  , inPolar
  , outPolar
  , inCartesian
  , outCartesian
  , conjugate
  , magnitude
  ) where

import Math (abs, atan2, cos, sin, sqrt)
import Prelude
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen (uniform)


-- | An abstract complex type.
data Complex = Complex Number Number

-- | A view used to construct/destruct a complex number using its cartesian form,
-- | defining the complex number as its real and imaginary parts. The arguments
-- | of the constructor are:
-- |
-- | + The real part of the number.
-- | + The imaginary part of the number.
data Cartesian = Cartesian Number Number

-- | A view used to construct/destruct a complex number using its polar form,
-- | defining the complex numbers as the distance of the number to zero on the
-- | complex plane and the angle between the x-axis and the number. The arguments
-- | of the constructor are:
-- |
-- | + The angle with the x-axis of the complex plane.
-- | + The distance to zero.
data Polar = Polar Number Number

-- | Pattern match on a complex number using its polar form. Example:
-- |
-- | ```purescript
-- | case inPolar z of
-- |     (Polar angle magnitude) -> ...
-- | ```
inPolar :: Complex -> Polar
inPolar z@(Complex r i) = Polar (atan2 i r) (magnitude z)

-- | Construct a complex number from its polar form. Example: Create the number
-- | `z = i`
-- |
-- | ```purescript
-- | z :: Complex
-- | z = outPolar (Polar (pi / 2) 1)
-- | ```
outPolar :: Polar -> Complex
outPolar (Polar a m) = Complex (m * (cos a)) (m * (sin a))

-- | Pattern match on a complex number using its cartesian form. Example:
-- |
-- | ```purescript
-- | case inCartesian z of
-- |     (Cartesian realPart imaginaryPart) -> ...
-- | ```
inCartesian :: Complex -> Cartesian
inCartesian (Complex r i) = Cartesian r i

-- | Construct a complex number from its cartesian form. Example: Create the number
-- | `z = 5 + 3i`
-- |
-- | ```purescript
-- | z :: Complex
-- | z = outCartesian (Cartesian 5 3)
-- | ```
outCartesian :: Cartesian -> Complex
outCartesian (Cartesian r i) = Complex r i

-- | Creates the conjugate of a complex number, with equal real and magnitude but
-- | with an opposite imaginary part. Example:
-- |
-- | ```purescript
-- | z :: Complex
-- | z = outCartesian (Cartesian 5 3)
-- |
-- | z' :: Complex
-- | z' = conjugate z
-- |
-- | z'' :: Complex
-- | z'' = outCartesian (Cartesian 5 -3)
-- |
-- | z' == z''
-- | ```
conjugate :: Complex -> Complex
conjugate (Complex r i) = Complex r (-i)

-- | Calculates the magnitude of a complex number, which is the distance of the
-- | number on the complex plan to zero.
magnitude :: Complex -> Number
magnitude (Complex r i) = sqrt (r*r + i*i)

-- Instances
instance arbitraryComplex :: Arbitrary Complex where
  arbitrary = outCartesian <$> (Cartesian <$> uniform <*> uniform)

-- Number related instances
instance semiringComplex :: Semiring Complex where
  add (Complex r i) (Complex r' i') = Complex (r + r') (i + i')
  zero = Complex zero zero
  mul (Complex r i) (Complex r' i') = Complex (r*r' - i*i') (r*i' + r'*i)
  one = Complex one zero

instance ringComplex :: Ring Complex where
  sub (Complex r i) (Complex r' i') = Complex (r - r') (i - i')

instance commutativeRingComplex :: CommutativeRing Complex

instance euclideanRingComplex :: EuclideanRing Complex where
  degree _ = 1
  div (Complex r i) (Complex r' i') = Complex nr ni
    where
      nr = (r*r' + i*i') / d
      ni = (i*r' - r*i') / d
      d  = r'*r' + i'*i'
  mod _ _ = zero

instance fieldComplex :: Field Complex

instance eqComplex :: Eq Complex where
  eq (Complex r i) (Complex r' i') = approxEq r r' && approxEq i i'
    where
      epsilon      = 0.00000001
      approxEq a b = abs (a - b) < epsilon

instance showComplex :: Show Complex where
  show (Complex r i) = show r <> " + " <> show i <> "i"
