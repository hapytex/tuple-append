{-# LANGUAGE CPP, StandaloneDeriving, UnicodeSyntax #-}

module Data.Tuple.AppendSpec where

import Data.Tuple.Append(TupleAddL((<++)), TupleAddR((++>)), TupleAppend((+++)), SequenceTuple(sequenceTupleA, sequenceTupleA_))

#if MIN_VERSION_ghc_prim(0,7,0)
import GHC.Tuple(Solo(Solo))
#endif

import Test.Hspec

#if MIN_VERSION_ghc_prim(0,7,0)
deriving instance Eq a ⇒ Eq (Solo a)
#endif

toAdd ∷ Int
toAdd = 1

tw :: Int
tw = 2

th ∷ Int
th = 3

fv :: Int
fv = 5

spec ∷ Spec
spec = do
  describe "TupleAddL" $ do
#if MIN_VERSION_ghc_prim(0,7,0)
    it "0-tuple" (toAdd <++ () `shouldBe` Solo 1)
    it "1-tuple" (toAdd <++ Solo 4 `shouldBe` (1, 4))
#endif
    it "2-tuple" (toAdd <++ (4, 2) `shouldBe` (1, 4, 2))
    it "3-tuple" (toAdd <++ (4, 2, 5) `shouldBe` (1, 4, 2, 5))
    it "4-tuple" (toAdd <++ (4, 2, 5, "foo") `shouldBe` (1, 4, 2, 5, "foo"))
    it "5-tuple" (toAdd <++ (4, 2, 5, "foo", "bar") `shouldBe` (1, 4, 2, 5, "foo", "bar"))
  describe "TupleAddR" $ do
#if MIN_VERSION_ghc_prim(0,7,0)
    it "0-tuple" (() ++> toAdd `shouldBe` Solo 1)
    it "1-tuple" (Solo 4 ++> toAdd `shouldBe` (4, 1))
#endif
    it "2-tuple" ((4, 2) ++> toAdd `shouldBe` (4, 2, 1))
    it "3-tuple" ((4, 2, 5) ++> toAdd `shouldBe` (4, 2, 5, 1))
    it "4-tuple" ((4, 2, 5, "foo") ++> toAdd `shouldBe` (4, 2, 5, "foo", 1))
    it "5-tuple" ((4, 2, 5, "foo", "bar") ++> toAdd `shouldBe` (4, 2, 5, "foo", "bar", 1))
  describe "TupleAppend" $ do
    it "0-tuple and 0-tuple" (() +++ () `shouldBe` ())
    it "0-tuple and 2-tuple" (() +++ (1, 4) `shouldBe` (1, 4))
    it "0-tuple and 3-tuple" (() +++ (1, 4, 2) `shouldBe` (1, 4, 2))
    it "0-tuple and 4-tuple" (() +++ (1, 4, 2, 5) `shouldBe` (1, 4, 2, 5))
    it "0-tuple and 5-tuple" (() +++ (1, 4, 2, 5, "foo") `shouldBe` (1, 4, 2, 5, "foo"))
    it "2-tuple and 0-tuple" ((1, 4) +++ () `shouldBe` (1, 4))
    it "2-tuple and 2-tuple" ((1, 4) +++ (2, 5) `shouldBe` (1, 4, 2, 5))
    it "2-tuple and 3-tuple" ((1, 4) +++ (2, 5, "foo") `shouldBe` (1, 4, 2, 5, "foo"))
    it "2-tuple and 4-tuple" ((1, 4) +++ (2, 5, "foo", "bar") `shouldBe` (1, 4, 2, 5, "foo", "bar"))
    it "2-tuple and 5-tuple" ((1, 4) +++ (2, 5, "foo", "bar", 3) `shouldBe` (1, 4, 2, 5, "foo", "bar", 3))
    it "3-tuple and 0-tuple" ((1, 4, 2) +++ () `shouldBe` (1, 4, 2))
    it "3-tuple and 2-tuple" ((1, 4, 2) +++ (5, "foo") `shouldBe` (1, 4, 2, 5, "foo"))
    it "3-tuple and 3-tuple" ((1, 4, 2) +++ (5, "foo", "bar") `shouldBe` (1, 4, 2, 5, "foo", "bar"))
    it "3-tuple and 4-tuple" ((1, 4, 2) +++ (5, "foo", "bar", 3) `shouldBe` (1, 4, 2, 5, "foo", "bar", 3))
    it "3-tuple and 5-tuple" ((1, 4, 2) +++ (5, "foo", "bar", 3, 0) `shouldBe` (1, 4, 2, 5, "foo", "bar", 3, 0))
    it "4-tuple and 0-tuple" ((1, 4, 2, 5) +++ () `shouldBe` (1, 4, 2, 5))
    it "4-tuple and 2-tuple" ((1, 4, 2, 5) +++ ("foo", "bar") `shouldBe` (1, 4, 2, 5, "foo", "bar"))
    it "4-tuple and 3-tuple" ((1, 4, 2, 5) +++ ("foo", "bar", 3) `shouldBe` (1, 4, 2, 5, "foo", "bar", 3))
    it "4-tuple and 4-tuple" ((1, 4, 2, 5) +++ ("foo", "bar", 3, 0) `shouldBe` (1, 4, 2, 5, "foo", "bar", 3, 0))
    it "4-tuple and 5-tuple" ((1, 4, 2, 5) +++ ("foo", "bar", 3, 0, "qux") `shouldBe` (1, 4, 2, 5, "foo", "bar", 3, 0, "qux"))
    it "5-tuple and 0-tuple" ((1, 4, 2, 5, "foo") +++ () `shouldBe` (1, 4, 2, 5, "foo"))
    it "5-tuple and 2-tuple" ((1, 4, 2, 5, "foo") +++ ("bar", 3) `shouldBe` (1, 4, 2, 5, "foo", "bar", 3))
    it "5-tuple and 3-tuple" ((1, 4, 2, 5, "foo") +++ ("bar", 3, 0) `shouldBe` (1, 4, 2, 5, "foo", "bar", 3, 0))
    it "5-tuple and 4-tuple" ((1, 4, 2, 5, "foo") +++ ("bar", 3, 0, "qux") `shouldBe` (1, 4, 2, 5, "foo", "bar", 3, 0, "qux"))
    it "5-tuple and 5-tuple" ((1, 4, 2, 5, "foo") +++ ("bar", 3, 0, "qux", 'h') `shouldBe` (1, 4, 2, 5, "foo", "bar", 3, 0, "qux", 'h'))
#if MIN_VERSION_ghc_prim(0,7,0)
    it "0-tuple and 1-tuple" (() +++ Solo 1 `shouldBe` Solo 1)
    it "1-tuple and 0-tuple" (Solo 1 +++ () `shouldBe` Solo 1)
    it "1-tuple and 1-tuple" (Solo 1 +++ Solo 4 `shouldBe` (1, 4))
    it "1-tuple and 2-tuple" (Solo 1 +++ (4, 2) `shouldBe` (1, 4, 2))
    it "1-tuple and 3-tuple" (Solo 1 +++ (4, 2, 5) `shouldBe` (1, 4, 2, 5))
    it "1-tuple and 4-tuple" (Solo 1 +++ (4, 2, 5, "foo") `shouldBe` (1, 4, 2, 5, "foo"))
    it "1-tuple and 5-tuple" (Solo 1 +++ (4, 2, 5, "foo", "bar") `shouldBe` (1, 4, 2, 5, "foo", "bar"))
    it "2-tuple and 1-tuple" ((1, 4) +++ Solo 2 `shouldBe` (1, 4, 2))
    it "3-tuple and 1-tuple" ((1, 4, 2) +++ Solo 5 `shouldBe` (1, 4, 2, 5))
    it "4-tuple and 1-tuple" ((1, 4, 2, 5) +++ Solo "foo" `shouldBe` (1, 4, 2, 5, "foo"))
    it "5-tuple and 1-tuple" ((1, 4, 2, 5, "foo") +++ Solo "bar" `shouldBe` (1, 4, 2, 5, "foo", "bar"))
#endif
  describe "SequenceTuple" $ do
#if MIN_VERSION_ghc_prim(0,7,0)
    describe "1-tuple" $ do
      it "Maybe-1" (sequenceTupleA (Solo (Just th)) `shouldBe` Just (Solo th))
      it "Maybe-2" (sequenceTupleA (Solo (Nothing :: Maybe Int)) `shouldBe` Nothing)
      it "Either-1" (sequenceTupleA (Solo (Left "a" :: Either String Int)) `shouldBe` Left "a")
      it "Either-2" (sequenceTupleA (Solo (Right th :: Either String Int)) `shouldBe` Right (Solo th))
#endif
    describe "2-tuple" $ do
      it "Maybe-1" (sequenceTupleA (Just th, Just fv) `shouldBe` Just (th, fv))
      it "Maybe-2" (sequenceTupleA (Nothing :: Maybe Int, Just fv) `shouldBe` Nothing)
      it "Either-1" (sequenceTupleA (Left "a" :: Either String String, Right tw :: Either String Int) `shouldBe` Left "a")
      it "Either-2" (sequenceTupleA (Right th :: Either String Int, Right fv) `shouldBe` Right (th, fv))
