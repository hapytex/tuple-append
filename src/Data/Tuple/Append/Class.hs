{-# LANGUAGE FlexibleInstances, FunctionalDependencies, Safe #-}

{-|
Module      : Data.Tuple.Append.Class
Description : A module that defines typeclasses to prepend and append items and tuples into new tuples.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module that defines typeclasses to prepend and append items and tuples into new tuples.
-}
module Data.Tuple.Append.Class (
  -- * Add an element to a tuple
    TupleAddL((<++)), TupleAddR((++>))
  -- * Append two tuples
  , TupleAppend((+++))
  ) where

-- | A typeclass mainly used to construct a tuple with one element extra. That element is added at the left side of the tuple.
-- The typeclass is also used for a small amount of extra datatypes to make it more convenient.
class TupleAddL a b c | a b -> c, a c -> b, b c -> a where
  infixr 5 <++
  -- | Construct a new tuple by adding the first parameter as first item in the tuple.
  (<++)
    :: a  -- ^ The item to prepend at the left side of the tuple.
    -> b  -- ^ The tuple containing the rest of the elements.
    -> c  -- ^ A tuple that has one element more than the given tuple: the given item that is prepended at the left side.


-- | A typeclass mainly used to construct a tuple with one element extra. That element is added at the right side of the tuple.
-- The typeclass is also used for a small amount of extra data types to make it more convenient.
class TupleAddR a b c | a b -> c, a c -> b, b c -> a where
  infixl 5 ++>
  -- | Construct a new tuple by adding the second parameter as last item in the tuple.
  (++>)
    :: b  -- ^ The tuple containing the rest of the elements.
    -> a  -- ^ The item to append at the right side of the tuple.
    -> c  -- ^ A tuple that has one element more than the given tuple: the given item that is appended at the right side.


-- | A typeclass mainly used to append two tuples together into a tuple that contains as many elements as the sum of the number of
-- elements of the two given tuples. The typeclass is also used for a small amount of extra data types to make it more convenient.
class TupleAppend a b c | a b -> c, a c -> b, b c -> a where
  infixr 5 +++
  -- | Construct a new tuple that contains the elements of the two given tuples.
  (+++)
    :: a  -- ^ The first tuple to append.
    -> b  -- ^ The second tuple to append.
    -> c  -- ^ A tuple that contains the items of the first and the second tuple.

instance TupleAddL a [a] [a] where
  (<++) = (:)

instance TupleAddR a [a] [a] where
  xs ++> x = xs ++ [x]

instance TupleAppend [a] [a] [a] where
  (+++) = (++)
