{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( module Lib
    ) where

import Control.Monad

import Data.Kind
import Data.Proxy
import GHC.Real

import GHC.TypeLits

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Rat = Ratio Nat

type family KnownRat r :: Constraint where
  KnownRat (n ':% m) = (KnownNat n, KnownNat m)

data SomeRat = forall (r :: Rat). KnownRat r => SomeRat (Proxy r)

ratVal :: KnownRat (n ':% d) => proxy (n ':% d) -> Rational
ratVal (_ :: proxy (n ':% d)) =
    natVal (Proxy :: Proxy n) :% natVal (Proxy :: Proxy d)

someRatVal :: Rational -> Maybe SomeRat
someRatVal (n :% d) = liftM2 cast (someNatVal n) (someNatVal d)
  where cast (SomeNat (_ :: Proxy n)) (SomeNat (_ :: Proxy d)) =
            SomeRat (Proxy :: Proxy (n ':% d))

