{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad (guard)
import Data.Kind (Type)
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink),
    Gen,
    Property,
    forAll,
    forAllShrink,
    (===),
    pattern Fn,
  )
import Test.QuickCheck.Poly (A)
import Test.Shrink (neShrink, shrinkSuchThat)
import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests, testProperty)

main :: IO ()
main =
  defaultMain . adjustOption moreTests . testGroup "Tests" $
    [ testProperty "Shrink with <*>" propBadShrink,
      testProperty "neShrink with <*>" propGoodShrink,
      testProperty "shrinkSuchThat" propShrinkSuchThat
    ]
  where
    moreTests :: QuickCheckTests -> QuickCheckTests
    moreTests = max 10_000

-- Properties

propBadShrink :: Property
propBadShrink = forAll @(Pair Int) genPair $ \p ->
  [] === badShrink p

propGoodShrink :: Property
propGoodShrink = forAll @(Pair Int) genPair $ \p@(Pair x y) ->
  (Pair x <$> shrink y) === goodShrink p

propShrinkSuchThat :: Property
propShrinkSuchThat = forAllShrink arbitrary shrink $ \(Fn cond, x :: A) ->
  let expected = do
        x' <- shrink x
        guard (cond x')
        pure x'
      actual = shrinkSuchThat cond x
   in expected === actual

-- Helpers

data Pair (a :: Type) = Pair a a
  deriving stock (Eq, Show)

genPair ::
  forall (a :: Type).
  (Arbitrary a, Num a) => Gen (Pair a)
genPair = Pair 0 <$> arbitrary

badShrink ::
  forall (a :: Type).
  (Arbitrary a) => Pair a -> [Pair a]
badShrink (Pair x y) = Pair <$> shrink x <*> shrink y

goodShrink ::
  forall (a :: Type).
  (Arbitrary a) => Pair a -> [Pair a]
goodShrink (Pair x y) = Pair <$> neShrink x <*> shrink y
