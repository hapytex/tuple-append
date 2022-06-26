{-# LANGUAGE MagicHash, UnboxedTuples, TemplateHaskell #-}
{-# OPTIONS_GHC -fobject-code #-}

module Data.Tuple.Append.Example where

import Data.Tuple.Append.TemplateHaskell(makeUnboxedTupleAppendFun)

import GHC.Exts(Int#, Float#)

import Language.Haskell.TH.Syntax(Type(ConT), mkName)

makeUnboxedTupleAppendFun (mkName "foo") [ ConT ''Int#, ConT ''Float# ] [ ConT ''Float#, ConT ''Int# ]
