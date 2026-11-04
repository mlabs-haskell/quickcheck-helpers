{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Test.QuickCheck.Helpers
-- Description: Some functions to help define shrinkers.
-- Copyright: MLabs (C) 2026
-- License: MIT
-- Maintainer: koz@mlabs.city
--
-- = Why we need this
--
-- Consider the following data type:
--
-- > data Pair a = Pair a a
--
-- If we want to define an 'Arbitrary' instance for this type, the 'arbitrary'
-- method is straightforward:
--
-- > instance Arbitrary a => Arbitrary (Pair a) where
-- >     arbitrary = Pair <$> arbitrary <*> arbitrary
--
-- In particular, we can use the applicative syntax to describe 'arbitrary' over
-- 'Pair', which is essentially a product type, both clearly and concisely. One
-- would naturally then believe that 'shrink' would be quite similar:
--
-- > instance Arbitrary a => Arbitrary (Pair a) where
-- >     -- totally fine
-- >     arbitrary = Pair <$> arbitrary <*> arbitrary
-- >     -- very much not fine
-- >     shrink (Pair x y) = Pair <$> shrink x <*> shrink y
--
-- To see why, consider @x = Pair 0 6 :: Pair Int@. We expect that @x@ shrinks;
-- however, using our definition, this will not happen:
--
-- >>> shrink (Pair 0 6 :: Pair Int)
-- []
--
-- This stems from the behaviour of @[]@ as the non-determinism effect: once we
-- get an empty list, the entire computation is \'cut off\'. Thus, in a
-- definition like the one above, a non-shrinking value in /any/ part of a
-- @Pair@ term will block the entire shrink:
--
-- >>> shrink (Pair 6 0 :: Pair Int)
-- []
-- >>> shrink (Pair 0 0 :: Pair Int)
-- []
--
-- While this doesn't violate any of the implicit requirements of shrinking, it
-- is definitely suboptimal: we end up failing to shrink too soon, potentially
-- leaving us with very complex counter-examples.
--
-- The correct way to write a \'product shrinker\' would be something akin to
-- the following:
--
-- > instance Arbitrary a => Arbitrary (Pair a) where
-- >     arbitrary = Pair <$> arbitrary <*> arbitrary
-- >     shrink (Pair x y) = (Pair <$> pure x <*> shrink y) <|>
--                           (Pair <$> shrink x <*> pure y)
--
-- Thus, for an @n@-ary product type, we have to write @n@-many entries between
-- @<|>@, each with @n@-many fields, shrinking exactly one each time. This is
-- long, tedious, and error-prone, especially on larger products.
--
-- = Why not use @Generic@?
--
-- If @Pair@ was given an instance of @Generic@, we could avoid this tedium:
--
-- > data Pair a = Pair a a
-- >     deriving stock (Generic)
-- >
-- > instance Arbitrary a => Arbitrary (Pair a) where
-- >     arbitrary = Pair <$> arbitrary <*> arbitrary
-- >     shrink = genericShrink
--
-- However, setting aside the lack of code clarity and performance issues of
-- @Generic@, this method is not suitable for any type with \'internal
-- invariants\'. More specifically, 'genericShrink' is purely structural, and
-- will not maintain any invariant that isn't enforced by the type system
-- directly. For example, consider the following:
--
-- > data CountedList a = CountedList Int [a]
--
-- Here, the @Int@ field has to obey the following invariants:
--
-- * Must be non-negative; and
-- * Must be the same as the length of the @[a]@ field.
--
-- If we were to implement 'Arbitrary' for @CountedList@ using 'genericShrink'
-- for the shrinker, we would end up with nonsensical shrinks that violate these
-- invariants. This could cause test failures that would never reflect the
-- proper use of this type.
--
-- Thus, in any situation where we have invariants that aren't \'purely
-- structural\', this method is incorrect and cannot be used.
--
-- = Our solution
--
-- We want to ensure that for a product type, shrinking only fails if /all/
-- fields fail to shrink, rather than if /any/ fields fail to shrink. For this,
-- we have 'Shrink', which is essentially @[]@ with the added ability to
-- remember what term we asked it to shrink. This allows us to avoid the \'early
-- failure\' problem of @[]@ described earlier. We can use 'neShrink' to
-- \'promote\' regular shrinkers for fields into 'Shrink'. Once we're finished
-- with our product type, we can use 'runShrink' to \'return\' back to @[]@.
--
-- If we use 'Shrink' via 'neShrink' and 'runShrink' to define @Pair@'s
-- 'Arbitrary' instance, we get the following:
--
-- > data Pair a = Pair a a
-- >
-- > instance Arbitrary a => Arbitrary (Pair a) where
-- >     arbitrary = Pair <$> arbitrary <*> arbitrary
-- >     shrink (Pair x y) = 'runShrink' (Pair <$> 'neShrink' x <*> 'neShrink' y)
--
-- We now can have a similar level of clarity and conciseness to the 'arbitrary'
-- method, but /without/ the undesirable behaviour we saw before:
--
-- >>> shrink (Pair 0 6 :: Pair Int)
-- [Pair 0 5, Pair 0 3, Pair 0 0]
-- >>> shrink (Pair 6 0 :: Pair Int)
-- [Pair 5 0, Pair 3 0, Pair 0 0]
-- >>> shrink (Pair 0 0 :: Pair Int)
-- []
--
-- @since 1.0.0
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

-- | A \'non-empty\' shrinking effect. Unlike shrinkers implemented in @[]@,
-- 'Shrink' remembers what value it was asked to shrink, and can produce it if
-- no shrinks are available.
--
-- @since 1.0.0
data Shrink (a :: Type)
  = NonShrinking a
  | Shrinking (NonEmpty a)
  deriving stock
    ( -- | @since 1.0.0
      Eq,
      -- | @since 1.0.0
      Show,
      -- | @since 1.0.0
      Functor
    )

-- | @since 1.0.0
instance Applicative Shrink where
  pure = NonShrinking
  fs <*> xs = case fs of
    NonShrinking f -> case xs of
      NonShrinking x -> NonShrinking (f x)
      Shrinking xs' -> Shrinking (fmap f xs')
    Shrinking fs' -> case xs of
      NonShrinking x -> Shrinking (fmap ($ x) fs')
      Shrinking xs' -> Shrinking (fs' <*> xs')

-- | @since 1.0.0
instance (Arbitrary a) => Arbitrary (Shrink a) where
  arbitrary = oneof [NonShrinking <$> arbitrary, Shrinking <$> arbitrary]
  shrink = \case
    NonShrinking _ -> []
    Shrinking xs -> Shrinking <$> shrink xs

-- | Convert a 'Shrink' into an expected QuickCheck shrinker result.
-- Specifically, if a 'Shrink' has no valid shrink targets, 'runShrink' will
-- produce an empty list; otherwise, it will produce a list of targets.
--
-- @since 1.0.0
runShrink ::
  forall (a :: Type).
  Shrink a ->
  [a]
runShrink = \case
  NonShrinking _ -> []
  Shrinking (y :| ys) -> y : ys

-- | As 'runShrink', but instead of converting a 'Shrink', it will convert a
-- function producing a shrink into a valid implementation of the 'shrink' type
-- class method of 'Arbitrary'.
--
-- @since 1.0.0
toShrink ::
  forall (a :: Type).
  (a -> Shrink a) ->
  a ->
  [a]
toShrink f x = runShrink (f x)

-- | \'Promotes\' an implementation of 'shrink' into 'Shrink'.
--
-- @since 1.0.0
neShrink ::
  forall (a :: Type).
  (Arbitrary a) => a -> Shrink a
neShrink x = case shrink x of
  [] -> NonShrinking x
  (y : ys) -> Shrinking (y :| ys)

-- | As 'neShrink', but by way of a \'wrapper\' we can 'coerce' through. Useful
-- with modifiers, as it means you don't have to manually unwrap and re-wrap
-- them.
--
-- = Note
--
-- This function is ambiguously typed, and expects a type argument to determine
-- what \'wrapper\' to use. The type signature is designed to allow easy use of
-- @TypeApplications@ for this:
--
-- > -- Wraps a value into NonNegative for shrinking, then unwraps
-- > neShrinkOver @(NonNegative Integer) x
--
-- @since 1.0.0
neShrinkOver ::
  forall (b :: Type) (a :: Type).
  (Arbitrary b, Coercible a b) =>
  a -> Shrink a
neShrinkOver x = case coerce . shrink @b . coerce $ x of
  [] -> NonShrinking x
  (y : ys) -> Shrinking (y :| ys)

-- | As 'neShrink', but allows adding a condition that any shrunk results must
-- satisfy. This is needed because 'Shrink' cannot be an instance of
-- 'Alternative'.
--
-- @since 1.0.0
neShrinkWhen ::
  forall (a :: Type).
  (Arbitrary a) =>
  (a -> Bool) ->
  a ->
  Shrink a
neShrinkWhen cond x = case filter cond . shrink $ x of
  [] -> NonShrinking x
  (y : ys) -> Shrinking (y :| ys)
