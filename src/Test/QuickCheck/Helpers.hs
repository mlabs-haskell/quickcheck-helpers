{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.QuickCheck.Helpers
  ( Shrink,
    neShrink,
    neShrinkOver,
    neShrinkWhen,
    runShrink,
    toShrink,
  )
where

import Data.Coerce (Coercible, coerce)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Test.QuickCheck (Arbitrary (arbitrary, shrink), oneof)

-- | @'Shrink' a ~ (a, 'Maybe' [a])@
data Shrink (a :: Type)
  = NonShrinking a
  | Shrinking (NonEmpty a)
  deriving stock (Eq, Show, Functor)

instance Applicative Shrink where
  pure = NonShrinking
  fs <*> xs = case fs of
    NonShrinking f -> case xs of
      NonShrinking x -> NonShrinking (f x)
      Shrinking xs' -> Shrinking (fmap f xs')
    Shrinking fs' -> case xs of
      NonShrinking x -> Shrinking (fmap ($ x) fs')
      Shrinking xs' -> Shrinking (fs' <*> xs')

-- | Extremely meta.
instance (Arbitrary a) => Arbitrary (Shrink a) where
  arbitrary = oneof [NonShrinking <$> arbitrary, Shrinking <$> arbitrary]
  shrink = \case
    NonShrinking _ -> []
    Shrinking xs -> Shrinking <$> shrink xs

runShrink ::
  forall (a :: Type).
  Shrink a ->
  [a]
runShrink = \case
  NonShrinking _ -> []
  Shrinking (y :| ys) -> y : ys

toShrink ::
  forall (a :: Type).
  (a -> Shrink a) ->
  a ->
  [a]
toShrink f x = runShrink (f x)

neShrink ::
  forall (a :: Type).
  (Arbitrary a) => a -> Shrink a
neShrink x = case shrink x of
  [] -> NonShrinking x
  (y : ys) -> Shrinking (y :| ys)

neShrinkOver ::
  forall (b :: Type) (a :: Type).
  (Arbitrary b, Coercible a b) =>
  a -> Shrink a
neShrinkOver x = case coerce . shrink @b . coerce $ x of
  [] -> NonShrinking x
  (y : ys) -> Shrinking (y :| ys)

neShrinkWhen ::
  forall (a :: Type).
  (Arbitrary a) =>
  (a -> Bool) ->
  a ->
  Shrink a
neShrinkWhen cond x = case filter cond . shrink $ x of
  [] -> NonShrinking x
  (y : ys) -> Shrinking (y :| ys)
