{-# LANGUAGE MagicHash, TemplateHaskell, UnboxedTuples #-}
{-# OPTIONS_GHC -fobject-code #-}

module Data.Tuple.Append.Example where

import Data.Tuple.Append.TemplateHaskell(makeUnboxedTupleAppendFun, makeBoxedTupleAppendFun)

-- import GHC.Exts(Float#, Int#, Float(F#), Int(I#))

import Language.Haskell.TH.Syntax(Type(ConT, VarT), mkName)

-- makeUnboxedTupleAppendFun (mkName "append_if_fi") [ VarT (mkName "a"), ConT ''Float# ] [ ConT ''Float#, ConT ''Int# ]
makeBoxedTupleAppendFun (mkName "append_0_fi") [ ] [ ]


-- main = let (# a, b, c, d #) = append_if_fi (# 1, 4.0# #) (# 2.0#, 5# #) in print (a, F# b, F# c, I# d)
