{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, Safe #-}

module Data.Tuple.Append.Class where

class TupleAddL a b c | a b -> c where
  (<++) :: a -> b -> c

class TupleAddR a b c | a b -> c where
  (++>) :: b -> a -> c

class TupleAppend a b c | a b -> c where
  (+++) :: a -> b -> c

