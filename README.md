# Type-level Rationals

See [Type-level literals](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#type-level-literals).

Via a `Rational`-like data kind built on GHC's `Nat`, a `KnownNat`-like kind
class, and arithmetic type families; This library implements type-level
constructed rational literals:
```haskell
{-# LANGUAGE DataKinds, TypeOperators #-}
import Data.Kind.Rat

data MyValue (r :: Rat) = MyValue Double

calculate :: KnownRat r => MyValue r -> Double
calculate (v @ (MyValue x)) = x / fromRational (ratVal v)

-- Known Rationals are constructed from known Naturals
aValue :: MyValue (3 % 5)
aValue = MyValue 12

>>> calculate aValue
20.0
```

Type families can be used to operate on type literals:
```haskell
f :: MyValue r -> MyValue (r / (1 % 2))
f (MyValue v) = (MyValue v)

>>> :k (/)
(/) :: Rat -> Rat -> Rat
```
