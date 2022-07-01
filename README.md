# tuple-append

[![Build Status of the package by GitHub actions](https://github.com/hapytex/tuple-append/actions/workflows/build-ci.yml/badge.svg)](https://github.com/hapytex/tuple-append/actions/workflows/build-ci.yml)
[![Hackage version badge](https://img.shields.io/hackage/v/tuple-append.svg)](https://hackage.haskell.org/package/tuple-append)

## Usage

The package defines three typeclasses: `TupleAddL`, `TupleAddR`, and `TupleAppend`, these are defined in the [`Data.Tuple.Append.Class` module](src/Data/Tuple/Append/Class.hs) and reexported in the [`Data.Tuple.Append` module](src/Data/Tuple/Append.hs). These export the functions `(<++) :: x -> (v₁, v₂, …, vₙ) -> (x, v₁, v₂, …, vₙ)`, `(++>) :: (v₁, v₂, …, vₙ) -> x -> (v₁, v₂, …, vₙ, x)` and `(+++) :: (u₁, u₂, …, uₘ) -> (v₁, v₂, …, vₙ) -> (u₁, u₂, …, uₘ, v₁, v₂, …, vₙ)` respectively. These functions thus add an element to the left side and the right side of tuple, or append two tuples together into a new tuple. The [`Data.Tuple.Append` module](src/Data/Tuple/Append.hs) creates instances for these typeclasses.

## Standard instances

For `TupleAddL` and `TupleAddR` it will define `(<++) :: x -> (v₁, v₂, …, vₙ) -> (x, v₁, v₂, …, vₙ)` and `(++>) :: (v₁, v₂, …, vₙ) -> x -> (x, v₁, v₂, …, vₙ, x)` for *0 &le; n &le; 61*. These will construct *n+1*-tuples so it can construct at most a 62-tuple, which is the maximum number of elements that can be stored in a tuple in GHC. This thus means that there are 62 instances for the `TupleAddL` and `TupleAddR` typeclasses for tuples.

For `(+++) :: (u₁, u₂, …, uₘ) -> (v₁, v₂, …, vₙ) -> (u₁, u₂, …, uₘ, v₁, v₂, …, vₙ)` it defines instances for *0 &le; m, n &le; 19*, it thus can append all possible tuples up to a 19-tuple. This is the maximum size that is defined for the [`Control.Lens.Tuple` module](https://hackage.haskell.org/package/lens-5.1.1/docs/Control-Lens-Tuple.html) for lenses on tuple items. We think this is reasonable: for the constructed tuple with size *k*, there are *k+1* instances, which means that 210 instances are defined in total.

Besides tuples `(<++) :: x -> [x] -> [x]`, `(++>) :: [x] -> x -> [x]` and `(+++) :: [u] -> [u] -> [u]` are also defined on lists, to make the functions more reusable.

For builds where one makes use of [`ghc-prim`](https://hackage.haskell.org/package/ghc-prim/) prior to version [`ghc-prim-0.7.0`](https://hackage.haskell.org/package/ghc-prim-0.7.0/), the [`Solo` data type](https://hackage.haskell.org/package/ghc-prim-0.7.0/docs/GHC-Tuple.html#t:Solo) (a tuple with one element) is not available. For these builds, it will thus not make instances with singleton tuples, and thus there are only 61 instances for `TupleAddL` and `TupleAddR` and 173 instances for `TupleAppend` that work with tuples. Whether the ["*unit type*" `()`](https://hackage.haskell.org/package/ghc-prim-0.7.0/docs/GHC-Tuple.html#t:-40--41-) is a tuple with no elements, and the `Solo` type is a tuple with exactly one element is debatable, but regardless, this package implemented instances for these.

## Generating (extra) functions and instances

One can create extra functions and typeclass instances to prepend and append tuples. While we think that the number of instances for *boxed* tuples is likely sufficient for all practical use cases, this might be more useful when one aims to construct such functions for example for *unboxed* tuples, or for boxed tuples with unboxed items. Such unboxed types can not be specified through a type *parameter*. Since there are several unboxed types, exhaustively creating functions for all such types would result in millions of functions, which would likely exhaust the memory of the compiler.

One can however make use of these functions to generate such functions such that it is unlikely to make mistakes, for example by swapping two variables that have the same type. For these one can make use of the `makeBoxedTupleAppendFun`, `makeUnboxedTupleAppendFun`, `makeBoxedAddLFun`, `makeUnboxedAddLFun`, `makeBoxedAddRFun`, and `makeUnboxedAddRFun` functions. These functions can be used when working with *template* Haskell to generate Haskell code. See for example the following example:

```haskell
{-# LANGUAGE MagicHash, TemplateHaskell, UnboxedTuples #-}
{-# OPTIONS_GHC -fobject-code #-}

module Data.Tuple.Append.Example where

import Data.Tuple.Append.TemplateHaskell(makeBoxedTupleAppendFun, makeUnboxedTupleAppendFun)

import GHC.Exts(Float#, Int#)

import Language.Haskell.TH.Syntax(Type(ConT, VarT), mkName)

makeBoxedTupleAppendFun (mkName "append_if_f") [ ConT ''Int, ConT ''Float ] [ ConT ''Float ]
makeUnboxedTupleAppendFun (mkName "uappend_ix_f") [ ConT ''Int#, VarT (mkName "a")] [ConT ''Float# ]
```

This will create a function named `append_if_f :: (Int, Float) -> (Float) -> (Int, Float, Float)` that appends a 2-tuple with an `Int` and a `Float` to a singleton tuple with a `Float` to a 3-tuple with an `Int`, a `Float` and another `Float`. Furthermore it creates a function named `uappend_ix_f :: (# Int#, a #) -> (# Float# #) -> (# Int#, a, Float# #)` that will append an unboxed tuple `(# Int#, a #)` with an `Int#` a type variable `a` and an unboxed tuple `(# Float# #)` with a `Float#` to an unboxed tuple `(# Int#, a, Float# #)`. This example can be found in the hidden [`Data.Tuple.Append.Example` module](src/Data/Tuple/Append/Example.hs).

## Package structure

The package contains three modules:

 - [`Data.Tuple.Append`](src/Data/Tuple/Append.hs) that re-exports the `TupleAddL`, `TupleAddR` and `TupleAppend` typeclasses together with instances for the tuples;
 - [`Data.Tuple.Append.Class`](src/Data/Tuple/Append/Class.hs) that defines the `TupleAddL`, `TupleAddR` and `TupleAppend` typeclasses together with instances for lists; and
 - [`Data.Tuple.Append.TemplateHaskell`](src/Data/Tuple/Append/TemplateHaskell.hs) that creates template Haskell expressions for function defintions and typeclass instances for boxed and unboxed tuples.

## `tuple-append` is *safe* Haskell

The package contains a module [`Data.Tuple.Append.TemplateHaskell`](src/Data/Tuple/Append/TemplateHaskell.hs) that defines routines to construct the instances for a large number of tuples. These are then used with *quasiquotation* to define instances in a safe module.

## Contribute

You can contribute by making a pull request on the [*GitHub repository*](https://github.com/hapytex/tuple-append).

You can contact the package maintainer by sending a mail to [`hapytexeu+gh@gmail.com`](mailto:hapytexeu+gh@gmail.com).
