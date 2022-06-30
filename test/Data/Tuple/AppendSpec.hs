{-# LANGUAGE CPP, StandaloneDeriving #-}

module Data.Tuple.AppendSpec where

import Data.Tuple.Append(TupleAddL((<++)), TupleAddR((++>)), TupleAppend((+++)))

#if MIN_VERSION_ghc_prim(0,7,0)
import GHC.Tuple(Solo(Solo))
#endif

import Test.Hspec

#if MIN_VERSION_ghc_prim(0,7,0)
deriving instance Eq a => Eq (Solo a)
#endif

toAdd :: Int
toAdd = 1

spec :: Spec
spec = do
  describe "TupleAddL" $ do
#if MIN_VERSION_ghc_prim(0,7,0)
    it "0-tuple" (toAdd <++ () `shouldBe` Solo 1)
    it "1-tuple" (toAdd <++ Solo 4 `shouldBe` (1, 4))
#endif
    it "2-tuple" (toAdd <++ (4, 2) `shouldBe` (1, 4, 2))
    it "3-tuple" (toAdd <++ (4, 2, 5) `shouldBe` (1, 4, 2, 5))
