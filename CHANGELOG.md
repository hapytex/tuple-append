# `tuple-append` changelog

For a full list of changes, see the history on [*GitHub*](https://github.com/hapytex/tuple-append).

## Version 0.1.2.0

Convert the module to `UnicodeSyntax`, and add a section about the laziness of `(<++)`, `(+++)` and `(++>)`.

## Version 0.1.1.0

If the `base` package is at least `base-4.9.0.0`, it also implements `TupleAddL`, `TupleAddR` and `TupleAppend` for the `NonEmpty` data type of the `Data.List.NonEmpty` module.

## Version 0.1.0.0

A first intial version that allows constructing arbitrary prepend and append functions and typeclass instances. Instances for `TupleAddL` and `TupleAddR` are defined to construct tuples with a length up to 62, and it is possible to construct any tuple by appending tuples for a size up to 19. Template Haskell and quasiquoters are avaiable to construct functions to prepend and append tuples for both boxed and unboxed tuples.
