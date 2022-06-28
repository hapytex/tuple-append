{-# LANGUAGE QuasiQuotes, Safe, UnboxedTuples #-}
{-# OPTIONS_GHC -fobject-code #-}

module Data.Tuple.Append.Unboxed where

import Data.Tuple.Append.TemplateHaskell(defineUnboxedTupleAppendFunctionsUpto)

[defineUnboxedTupleAppendFunctionsUpto|19|]

-- import GHC.Exts(Float#, Int#, Float(F#), Int(I#))

{-
append_if_fi :: (# a, b #) -> (# c, d #) -> (# a, b, c, d #)
append_if_fi (# a, b #) (# c, d #) = (# a, b, c, d #)
-}

-- import Language.Haskell.TH.Syntax(Type(ConT, VarT), mkName)

-- makeUnboxedTupleAppendFun (mkName "append_if_fi") [ VarT (mkName "a"), ConT ''Float# ] [ ConT ''Float#, ConT ''Int# ]

-- main = let (# a, b, c, d #) = append_if_fi (# 1, 4.0 #) (# 2.0, 5 #) in print (a, b, c, d)
