{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.QuickCheck.Helpers
  ( -- * Generation
    genOver,

    -- * Shrinking
    shrinkOver,
    neShrink,
    neShrinkOver,
    shrinkSuchThat,
  )
where

import Data.Coerce (Coercible, coerce)
import Data.Kind (Type)
import Test.QuickCheck (Arbitrary (arbitrary, shrink), Gen)

genOver ::
  forall (b :: Type) (a :: Type).
  (Arbitrary b, Coercible a b) =>
  Gen a
genOver = coerce <$> arbitrary @b

neShrink :: forall (a :: Type). (Arbitrary a) => a -> [a]
neShrink x = case shrink x of
  [] -> [x]
  xs -> xs

shrinkOver ::
  forall (b :: Type) (a :: Type).
  (Arbitrary b, Coercible a b) =>
  a -> [a]
shrinkOver = coerce . shrink @b . coerce

neShrinkOver ::
  forall (b :: Type) (a :: Type).
  (Arbitrary b, Coercible a b) =>
  a -> [a]
neShrinkOver x = case shrinkOver @b x of
  [] -> [x]
  xs -> xs

shrinkSuchThat ::
  forall (a :: Type).
  (Arbitrary a) =>
  (a -> Bool) ->
  a ->
  [a]
shrinkSuchThat cond = filter cond . shrink
