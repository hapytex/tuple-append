# tuple-append

[![Build Status of the package by GitHub actions](https://github.com/hapytex/tuple-append/actions/workflows/build-ci.yml/badge.svg)](https://github.com/hapytex/tuple-append/actions/workflows/build-ci.yml)
[![Hackage version badge](https://img.shields.io/hackage/v/tuple-append.svg)](https://hackage.haskell.org/package/tuple-append)

## Usage

The package defines three typeclasses: `TupleAddL`, `TupleAddR`, and `TupleAppend`, these are defined in the [`Data.Tuple.Append.Class` module](src/Data/Tuple/Append/Class.hs) and reexported in the [`Data.Tuple.Append` module](src/Data/Tuple/Append.hs). These export the functions `(<++) :: x -> (v₁, v₂, …, vₙ) -> (x, v₁, v₂, …, vₙ)`, `(++>) :: (v₁, v₂, …, vₙ) -> x -> (x, v₁, v₂, …, vₙ, x)` and `(+++) :: (u₁, u₂, …, uₘ) -> (v₁, v₂, …, vₙ) -> (u₁, u₂, …, uₘ, v₁, v₂, …, vₙ)` respectively. These functions thus add an element to the left side and the right side of tuple, or append two tuples together into a new tuple. The [`Data.Tuple.Append` module](src/Data/Tuple/Append.hs) creates instances for these typeclasses.

For `TupleAddL` and `TupleAddR` it will define `(<++) :: x -> (v₁, v₂, …, vₙ) -> (x, v₁, v₂, …, vₙ)` and `(++>) :: (v₁, v₂, …, vₙ) -> x -> (x, v₁, v₂, …, vₙ, x)` for *2 &le; n &le; 61*. These will construct *n+1*-tuples so it can construct at most a 62-tuple, which is the maximum number of elements that can be stored in a tuple in GHC. This thus means that there are 60 instances for the `TupleAddL` and `TupleAddR` typeclasses for tuples.

For `(+++) :: (u₁, u₂, …, uₘ) -> (v₁, v₂, …, vₙ) -> (u₁, u₂, …, uₘ, v₁, v₂, …, vₙ)` it defines instances for *2 &le; m, n &le; 17* and *4 &le; m+n &le; 19*, it thus can append all possible tuples up to a 19-tuple. This is the maximum size that is defined for the [`Control.Lens.Tuple` module](https://hackage.haskell.org/package/lens-5.1.1/docs/Control-Lens-Tuple.html) for lenses on tuple items. We think this is reasonable: for the constructed tuple with size *k=m+n*, there are *k-3* instances, which means that 136 instances are defined in total.

Besides tuples `(<++) :: x -> [x] -> [x]`, `(++>) :: [x] -> x -> [x]` and `(+++) :: [u] -> [u] -> [u]` are also defined on lists, to make the functions more reusable.

## Generating (extra) functions and instances

It might []

```haskell
{-# LANGUAGE MagicHash, TemplateHaskell, UnboxedTuples #-}
{-# OPTIONS_GHC -fobject-code #-}

module Data.Tuple.Append.Example where

import Data.Tuple.Append.TemplateHaskell(makeUnboxedTupleAppendFun)

import GHC.Exts(Float#, Int#)

import Language.Haskell.TH.Syntax(Type(ConT), mkName)

makeUnboxedTupleAppendFun (mkName "append_if_fi") [ ConT ''Int#, ConT ''Float# ] [ ConT ''Float#, ConT ''Int# ]
```

This will a function named `append_if_fi :: (# Int#, Float# #) -> (# Float#, Int# #) -> (# Int#, Float#, Float#, Int# #)` that will append an unboxed tuple `(# Int#, Float# #)` with an `Int#` and `Float#` and an unboxed tuple `(# Float#, Int# #)` with a `Float#` and `Int#` to an unboxed tuple `(# Int#, Float#, Float#, Int# #)`.

## Package structure

The package contains three modules:

 - [`Data.Tuple.Append`](src/Data/Tuple/Append.hs) that re-exports the `TupleAddL`, `TupleAddR` and `TupleAppend` typeclasses together with instances for the tuples;
 - [`Data.Tuple.Append.Class`](src/Data/Tuple/Append/Class.hs) that defines the `TupleAddL`, `TupleAddR` and `TupleAppend` typeclasses together with instances for lists; and
 - [`Data.Tuple.Append.TemplateHaskell`](src/Data/Tuple/Append/TemplateHaskell.hs) that creates template Haskell expressions for function defintions and typeclass instances for boxed and unboxed tuples.

## `tuple-append` is *safe* Haskell

The package contains a module [`Data.Tuple.Append.TemplateHaskell`](src/Data/Tuple/Append/TemplateHaskell.hs) that defines routines to construct the instances for a large number of tuples. These are then used with *quasiquotation* to define safe instances.

## Contribute

You can contribute by making a pull request on the [*GitHub repository*](https://github.com/hapytex/tuple-append).

You can contact the package maintainer by sending a mail to [`hapytexeu+gh@gmail.com`](mailto:hapytexeu+gh@gmail.com).

