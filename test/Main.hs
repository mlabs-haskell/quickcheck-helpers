{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink),
    Gen,
    Property,
    forAll,
    (.&&.),
    (===),
  )
import Test.QuickCheck.Helpers (Shrink, neShrink, runShrink)
import Test.QuickCheck.Poly (A)
import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests, testProperty)
import Test.Tasty.QuickCheck.Laws (Checkable)
import Test.Tasty.QuickCheck.Laws.Applicative (testApplicativeLaws1)

main :: IO ()
main =
  defaultMain . adjustOption moreTests . testGroup "Tests" $
    [ adjustOption fewerTests $ testApplicativeLaws1 (Proxy @Shrink) (Proxy @()) (Proxy @A) (const (==)),
      testProperty "Shrink with <*>" propBadShrink,
      testProperty "neShrink with <*> (left non-shrinking)" propGoodShrinkLeft,
      testProperty "neShrink with <*> (right non-shrinking)" propGoodShrinkRight
    ]
  where
    moreTests :: QuickCheckTests -> QuickCheckTests
    moreTests = max 10_000
    -- Needed because the test for Applicative's associativity takes forever
    -- otherwise
    fewerTests :: QuickCheckTests -> QuickCheckTests
    fewerTests x = x `quot` 8

-- Properties

propBadShrink :: Property
propBadShrink = forAll ((,) <$> genPairLeft @Int <*> genPairRight @Int) $ \(pLeft, pRight) ->
  [] === badShrink pLeft .&&. [] === badShrink pRight

propGoodShrinkLeft :: Property
propGoodShrinkLeft = forAll @(Pair Int) genPairLeft $ \p@(Pair x y) ->
  (Pair x <$> shrink y) === goodShrink p

propGoodShrinkRight :: Property
propGoodShrinkRight = forAll @(Pair Int) genPairRight $ \p@(Pair x y) ->
  (Pair <$> shrink x <*> pure y) === goodShrink p

-- Helpers

data Pair (a :: Type) = Pair a a
  deriving stock (Eq, Show)

genPairLeft ::
  forall (a :: Type).
  (Arbitrary a, Num a) => Gen (Pair a)
genPairLeft = Pair 0 <$> arbitrary

genPairRight ::
  forall (a :: Type).
  (Arbitrary a, Num a) => Gen (Pair a)
genPairRight = Pair <$> arbitrary <*> pure 0

badShrink ::
  forall (a :: Type).
  (Arbitrary a) => Pair a -> [Pair a]
badShrink (Pair x y) = Pair <$> shrink x <*> shrink y

goodShrink ::
  forall (a :: Type).
  (Arbitrary a) => Pair a -> [Pair a]
goodShrink = runShrink (\(Pair x y) -> Pair <$> neShrink x <*> neShrink y)

-- Ugh
deriving via Integer instance Checkable A
