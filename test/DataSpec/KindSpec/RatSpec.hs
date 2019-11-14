{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DataSpec.KindSpec.RatSpec where

import Control.Monad

import Data.Kind.Rat
import Data.Proxy

import qualified GHC.Real as R (Ratio(..))

import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "ratVal" $ do
    it "gets values from Rational kinds" $ do
      ratVal (Proxy :: Proxy (1 ':% 2)) `shouldBe` (0.5)

    it "doesn't reduce an unreduced Rat" $ do
      ratVal (Proxy :: Proxy (5 ':% 5)) `shouldBe` (5 R.:% 5)

    context "when used with type families" $ do

      it "creates Rats from Nats" $ do
        (Proxy :: Proxy (4 % 3)) `shouldBe` (Proxy :: Proxy (4 ':% 3))

      it "calculates the resulting rational" $ do
        ratVal (Proxy :: Proxy ((2 % 3) / (3 % 2))) `shouldBe` (4/9)
        fromRational (ratVal (Proxy :: Proxy ((2 % 3) * (3 % 2))))
          `shouldBe` 1.0

  describe "someRatVal" $ do
    it "creates undetermined, expected type-level Rat" $ do
      case someRatVal 3.0 of
        Just (SomeRat p) -> ratVal p `shouldBe` 3.0
        Nothing -> expectationFailure "shouldn't be Nothing"

    it "doesn't yet support negatives" $ do
      case someRatVal (-1.0) of
        Just _ -> expectationFailure "shouldn't compute"
        Nothing -> return ()
