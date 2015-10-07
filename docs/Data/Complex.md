## Module Data.Complex

#### `Complex`

``` purescript
data Complex
```

An abstract complex type.

##### Instances
``` purescript
instance semiringComplex :: Semiring Complex
instance ringComplex :: Ring Complex
instance moduloSemiringComplex :: ModuloSemiring Complex
instance divisionRingComplex :: DivisionRing Complex
instance numComplex :: Num Complex
instance eqComplex :: Eq Complex
instance showComplex :: Show Complex
```

#### `Cartesian`

``` purescript
data Cartesian
  = Cartesian Number Number
```

A view used to construct/destruct a complex number using its cartesian form,
defining the complex number as its real and imaginary parts. The arguments
of the constructor are:

+ The real part of the number.
+ The imaginary part of the number.

#### `Polar`

``` purescript
data Polar
  = Polar Number Number
```

A view used to construct/destruct a complex number using its polar form,
defining the complex numbers as the distance of the number to zero on the
complex plane and the angle between the x-axis and the number. The arguments
of the constructor are:

+ The angle with the x-axis of the complex plane.
+ The distance to zero.

#### `inPolar`

``` purescript
inPolar :: Complex -> Polar
```

Pattern match on a complex number using its polar form. Example:

```purescript
case inPolar z of
    (Polar angle magnitude) -> ...
```

#### `outPolar`

``` purescript
outPolar :: Polar -> Complex
```

Construct a complex number from its polar form. Example: Create the number
`z = i`

```purescript
z :: Complex
z = outPolar (Polar (pi / 2) 1)
```

#### `inCartesian`

``` purescript
inCartesian :: Complex -> Cartesian
```

Pattern match on a complex number using its cartesian form. Example:

```purescript
case inCartesian z of
    (Cartesian realPart imaginaryPart) -> ...
```

#### `outCartesian`

``` purescript
outCartesian :: Cartesian -> Complex
```

Construct a complex number from its cartesian form. Example: Create the number
`z = 5 + 3i`

```purescript
z :: Complex
z = outCartesian (Cartesian 5 3)
```


