{-# LANGUAGE FunctionalDependencies, Safe #-}

module Data.Tuple.Append.Class where

-- | A typeclass mainly used to construct a tuple with one element extra. That element is added at the left side of the tuple.
-- The typeclass is also used for a small amount of extra datatypes to make it more convenient.
class TupleAddL a b c | a b -> c where
  (<++) :: a -> b -> c

-- | A typeclass mainly used to construct a tuple with one element extra. That element is added at the right side of the tuple.
-- The typeclass is also used for a small amount of extra data types to make it more convenient.
class TupleAddR a b c | a b -> c where
  (++>) :: b -> a -> c

-- | A typeclass mainly used to append two tuples together into a tuple that contains as many elements as the sum of the number of
-- elements of the two given tuples. The typeclass is also used for a small amount of extra data types to make it more convenient.
class TupleAppend a b c | a b -> c where
  (+++) :: a -> b -> c
