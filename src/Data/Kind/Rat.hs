{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

{-| This module exports the functions and definitions required to define and use
type-level rationals:

> import Data.Kind.Rat
>
> data R (r :: Rat) = ConstR
> myR = ConstR :: R (3 ':% 2)

>>> ratVal myR
1.5
-}

module Data.Kind.Rat
    ( Rat (..)
    , KnownRat
    , ratVal
    , SomeRat (SomeRat)
    , someRatVal
    , type (%)
    , type (+)
    , type (-)
    , type (/)
    , type (*)
    ) where

import Control.Monad

import Data.Kind
import Data.Proxy
import qualified GHC.Real as R

import GHC.TypeLits hiding (type (*), type (+), type (-))
import qualified GHC.TypeLits as Ty

-- | Data definition akin to "Data.Ratio" for use in kind specs.You can use '%'
-- instead of '\':%' for constructing 'Rat's from 'Nat's
data Rat = Nat :% Nat

-- | Type class to abbreviate constraints from
--
-- > (KnownNat n, KnownNat d) => proxy (n ':% d)
--
-- to
--
-- > KnownRat r => proxy r
--
class KnownRat (r :: Rat) where
  -- | Get value from type.
  ratVal :: proxy r -> Rational

instance (KnownNat n, KnownNat d) => KnownRat (n ':% d) where
  ratVal (_ :: proxy (n ':% d)) =
    natVal (Proxy :: Proxy n) R.:% natVal (Proxy :: Proxy d)

-- | This type represents unknown type-level rationals.
data SomeRat = forall (r :: Rat). KnownRat r => SomeRat (Proxy r)

-- | Convert rational into an unknown type-level ratio.
someRatVal :: Rational -> Maybe SomeRat
someRatVal (n R.:% d) = liftM2 cast (someNatVal n) (someNatVal d)
  where cast (SomeNat (_ :: Proxy n)) (SomeNat (_ :: Proxy d)) =
            SomeRat (Proxy :: Proxy (n ':% d))

-- |
-- == Type-level operations to and between 'Rat's
-- | Type operator akin to the '%' for Ratio. Note: the type operator doesn't
-- reduce the resulting rational.
type family (n :: Nat) % (d :: Nat) :: Rat where
  n % d = n ':% d

-- | Addition between Rationals.
type family (r :: Rat) + (s :: Rat) :: Rat where
  (n0 ':% d0) + (n1 ':% d1) = (n0 Ty.* d1 Ty.+ n1 Ty.* d0) ':% (d0 Ty.* d1)
infixl 6 +

-- | Subtraction.
type family (r :: Rat) - (s :: Rat) :: Rat where
  (n0 ':% d0) - (n1 ':% d1) = (n0 Ty.* d1 Ty.- n1 Ty.* d0) ':% (d0 Ty.* d1)
infixl 6 -

-- | Division.
type family (r :: Rat) / (s :: Rat) :: Rat where
  (n0 ':% d0) / (n1 ':% d1) = (n0 Ty.* d1) ':% (d0 Ty.* n1)
infixl 7 /

-- | Product.
type family (r :: Rat) * (s :: Rat) :: Rat where
  (n0 ':% d0) * (n1 ':% d1) = (n0 Ty.* n1) ':% (d0 Ty.* d1)
infixl 7 *
