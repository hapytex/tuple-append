{-# LANGUAGE CPP, MagicHash, TemplateHaskell, UnboxedTuples #-}
{-# OPTIONS_GHC -fobject-code #-}

module Data.Tuple.Append.Example where

#if MIN_VERSION_ghc_prim(0,7,0)
import Data.Tuple.Append.TemplateHaskell(makeBoxedTupleAppendFun, makeUnboxedTupleAppendFun)

import GHC.Exts(Float#, Int#)

import Language.Haskell.TH.Syntax(Type(ConT, VarT), mkName)

makeBoxedTupleAppendFun (mkName "append_if_f") [ ConT ''Int, ConT ''Float ] [ ConT ''Float ]
makeUnboxedTupleAppendFun (mkName "uappend_ix_f") [ ConT ''Int#, VarT (mkName "a")] [ConT ''Float# ]
#endif
