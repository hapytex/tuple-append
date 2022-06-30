{-# LANGUAGE StandaloneDeriving #-}

module Data.Tuple.AppendSpec where

import Data.Tuple.Append(TupleAddL((<++)), TupleAddR((++>)), TupleAppend((+++)))
import GHC.Tuple(Solo(Solo))

import Test.Hspec

deriving instance Eq a => Eq (Solo a)

toAdd :: Int
toAdd = 1

spec :: Spec
spec = do
  describe "TupleAddL" $ do
    it "0-tuple" (toAdd <++ () `shouldBe` Solo 1)
    it "1-tuple" (toAdd <++ Solo 4 `shouldBe` (1, 4))
    it "2-tuple" (toAdd <++ (4, 2) `shouldBe` (1, 4, 2))
    it "3-tuple" (toAdd <++ (4, 2, 5) `shouldBe` (1, 4, 2, 5))
