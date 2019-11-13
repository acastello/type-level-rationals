# Type-level Rationals

Through the `DataKinds` extension, GHC allows for type-level `Nat` and `Symbol`
literals, and although there is no way to get GHC to recognise type-level
fractional literals like
```haskell
Proxy :: Proxy 3.14
```

`DataKinds` generally promotes type constructors to kind constructors, such as
```haskell
(:%) :: a -> a -> Ratio a  -- gives way to Kind rationals

```

This package lifts `Nat` operations to handle `Rational`-like kinds.

## Define a stat with a Fractional tag
```haskell
{-# LANGUAGE DataKinds #-}
import Data.Kind.Rat

data Stat (denom :: Rat) = Stat Double

calcStat :: Stat denom -> Double
calcStat st@(Stat 
