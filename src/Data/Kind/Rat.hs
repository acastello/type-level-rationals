{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-| This module exports the functions and definitions required to define and use
type-level rationals:

> import Data.Kind.Rat
>
> data R (r :: Rat)
> myR :: R (3 ':% 2)

>>> ratVal
3

-}

module Lib
    ( Rat
    , type (%)
    , KnownRat
    , SomeRat (SomeRat)
    , ratVal
    , someRatVal
    , Ratio ((:%))
    ) where

import Control.Monad

import Data.Kind
import Data.Proxy
import GHC.Real (Ratio ((:%)))
import qualified GHC.Real as R ((%))

import GHC.TypeLits

-- | Type synonym for use in kind specs.
type Rat = Ratio Nat

-- | Type constructor synonym. Unlike the value operator, this doesn't reduce
-- the terms.
type n % d = n ':% d

-- | type family to abbreviate constraints from
--
-- > (KnownNat n, KnownNat d) => proxy (n ':% d)
--
-- to
--
-- > (KnownRat (n ':% d)) => proxy (n ':% d)
--
type family KnownRat r :: Constraint where
  KnownRat (n ':% m) = (KnownNat n, KnownNat m)

-- | Get rational value from type.
-- Note: 'ratVal' always reduces the resulting Rational.
ratVal :: KnownRat (n ':% d) => proxy (n ':% d) -> Rational
ratVal (_ :: proxy (n ':% d)) =
    natVal (Proxy :: Proxy n) R.% natVal (Proxy :: Proxy d)

-- | This type represents unknown type-level rationals.
data SomeRat = forall (r :: Rat). KnownRat r => SomeRat (Proxy r)

-- | Convert rational into an unknown type-level ratio.
someRatVal :: Rational -> Maybe SomeRat
someRatVal (n :% d) = liftM2 cast (someNatVal n) (someNatVal d)
  where cast (SomeNat (_ :: Proxy n)) (SomeNat (_ :: Proxy d)) =
            SomeRat (Proxy :: Proxy (n ':% d))

