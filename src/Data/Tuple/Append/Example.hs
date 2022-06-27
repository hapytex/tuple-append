{-# LANGUAGE MagicHash, TemplateHaskell, UnboxedTuples #-}
{-# OPTIONS_GHC -fobject-code #-}

module Data.Tuple.Append.Example where

import Data.Tuple.Append.TemplateHaskell(makeUnboxedTupleAppendFun)

import GHC.Exts(Float#, Int#)

import Language.Haskell.TH.Syntax(Type(ConT), mkName)

makeUnboxedTupleAppendFun (mkName "append_if_fi") [ ConT ''Int#, ConT ''Float# ] [ ConT ''Float#, ConT ''Int# ]
