{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Data.Tuple.Append.Class
-- Description : A module that defines typeclasses to prepend and append items and tuples into new tuples.
-- Maintainer  : hapytexeu+gh@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- A module that defines typeclasses to prepend and append items and tuples into new tuples.
module Data.Tuple.Append.Class
  ( -- * Add an element to a tuple
    TupleAddL ((<++)),
    TupleAddR ((++>)),

    -- * Append two tuples
    TupleAppend ((+++)),

    -- * Lifting tuples of applicatives
    SequenceTuple (sequenceTupleA, sequenceTupleA_),

    -- * Folding elements in a tuple
    TupleFold(foldlTuple, foldrTuple, foldMapTuple)
  )
where

import Data.Foldable (sequenceA_)
import Data.Functor (($>))
#if MIN_VERSION_base(4,9,0)
import Data.List.NonEmpty(NonEmpty((:|)), (<|))
#endif
#if MIN_VERSION_base(4,11,0)
#elif MIN_VERSION_base(4,9,0)
import Data.Semigroup((<>))
#endif
import Data.Monoid(Dual(Dual, getDual), Endo(Endo, appEndo))

-- | A typeclass mainly used to construct a tuple with one element extra. That element is added at the left side of the tuple.
-- The typeclass is also used for a small amount of extra datatypes to make it more convenient.
class TupleAddL x 𝐯 x𝐯 | x 𝐯 -> x𝐯, x𝐯 -> x, x𝐯 -> 𝐯 where
  infixr 5 <++

  -- | Construct a new tuple by adding the first parameter as first item in the tuple.
  (<++) ::
    -- | The item to prepend at the left side of the tuple.
    x ->
    -- | The tuple containing the rest of the elements.
    𝐯 ->
    -- | A tuple that has one element more than the given tuple: the given item that is prepended at the left side.
    x𝐯

-- | A typeclass mainly used to construct a tuple with one element extra. That element is added at the right side of the tuple.
-- The typeclass is also used for a small amount of extra data types to make it more convenient.
class TupleAddR 𝐯 x 𝐯x | 𝐯 x -> 𝐯x, 𝐯x -> 𝐯, 𝐯x -> x where
  infixl 5 ++>

  -- | Construct a new tuple by adding the second parameter as last item in the tuple.
  (++>) ::
    -- | The tuple containing the rest of the elements.
    𝐯 ->
    -- | The item to append at the right side of the tuple.
    x ->
    -- | A tuple that has one element more than the given tuple: the given item that is appended at the right side.
    𝐯x

-- | A typeclass mainly used to append two tuples together into a tuple that contains as many elements as the sum of the number of
-- elements of the two given tuples. The typeclass is also used for a small amount of extra data types to make it more convenient.
class TupleAppend 𝐮 𝐯 𝐮𝐯 | 𝐮 𝐯 -> 𝐮𝐯, 𝐮 𝐮𝐯 -> 𝐯, 𝐯 𝐮𝐯 -> 𝐮 where
  infixr 5 +++

  -- | Construct a new tuple that contains the elements of the two given tuples.
  (+++) ::
    -- | The first tuple to append.
    𝐮 ->
    -- | The second tuple to append.
    𝐯 ->
    -- | A tuple that contains the items of the first and the second tuple.
    𝐮𝐯

-- | A typeclass to process a tuple of 'Applicative' elements to an 'Applicative' of a tuple. While a 2-tuple
-- has a 'sequenceA' function, that function sees the tuples as a collection of /one/ element: the second item.
-- This 'SequenceTuple' typeclass considers this a collection of /n/ elements for an /n/-tuple and thus
-- runs over all elements of the tuple.
class Applicative f => SequenceTuple f f𝐮 𝐮 | f𝐮 -> f 𝐮, f f𝐮 -> 𝐮, f 𝐮 -> f𝐮 where
  -- | Sequence the elements of the tuple. For an /n/ tuple @sequenceTupleA (v₁, v₂, …, vₙ)@ is equivalent to:
  -- @(,,…,) <$> v₁ <*> v₂ <*> … <*> vₙ@.
  sequenceTupleA ::
    -- | The tuple with applicative elements.
    f𝐮 ->
    -- | An applicative tuple thas has sequenced over the elements of the tuple.
    f 𝐮
  default sequenceTupleA :: (Traversable t, 𝐮 ~ t b, f𝐮 ~ t (f b)) => f𝐮 -> f 𝐮
  sequenceTupleA = sequenceA

  -- | Sequence the elements of the tuple, and return the unit. For an /n/ tuple @sequenceTupleA_ (v₁, v₂, …, vₙ)@
  -- is equivalent to: @v₁ *> (v₂ *> (… *> (vₙ *> pure ())))@.
  sequenceTupleA_ ::
    -- | The tuple of applicatives to sequence.
    f𝐮 ->
    -- | An applicative for the unit type.
    f ()
  sequenceTupleA_ x = sequenceTupleA x $> ()

  {-# MINIMAL sequenceTupleA #-}

class TupleFold v 𝐯 | 𝐯 -> v where
  -- | Fold any tuple left-to-right with the given folding function that folds a second element, the first value for the
  -- accumulator and the tuple to fold, so:
  --
  -- @foldlTuple f z (v₁, v₂, …, vₙ) == (…((z `f` v₁) `f` v₂) `f` …) `f` vₙ@
  --
  foldlTuple ::
    -- | The "folding function" that takes the acculator thus far and an element from the tuple, and produces a new accumulator.
    (a -> v -> a) ->
    -- | The initial value for the accumulator to use.
    a ->
    -- | The tuple that we "fold".
    𝐯 ->
    -- | The result of the folding process.
    a
  foldlTuple f z t = appEndo (getDual (foldMapTuple (Dual . Endo . flip f) t)) z

  -- | Fold any tuple right-to-left with the given folding function that folds a second element, the first value for the
  -- accumulator  and the tuple to fold, so:
  --
  -- @foldrTuple f z (v₁, v₂, …, vₙ) == f v₁ (f v₂ (… (f vₙ z) …)))@
  --
  foldrTuple ::
    -- | The "folding function" that takes an element from the tuple, the accumulator, and produces a new accumulator.
    (v -> a -> a) ->
    -- | The initial value for the accumulator to use.
    a ->
    -- | The tuple that we "fold".
    𝐯 ->
    -- | The result of the folding process.
    a
  foldrTuple f z t = appEndo (foldMapTuple (Endo . f) t) z

  -- | Map the items in the tuple to a value of a 'Monoid' type and then fold these through the 'Monoid' instance.
  --
  -- @foldMapTuple f z (v₁, v₂, …, vₙ) == f v₁ <> (f v₂ <> (… (f vₙ <> mempty) …)))@
  --
  foldMapTuple :: Monoid m =>
    -- | The mapping function that maps the elements of the tuple to a value of a 'Monoid' type.
    (v -> m) ->
    -- | The tuple to "fold".
    𝐯 ->
    -- | The result of the folding process.
    m
  foldMapTuple f = foldrTuple (mappend . f) mempty

  {-# MINIMAL foldMapTuple | foldrTuple #-}

instance Applicative f => SequenceTuple f [f a] [a] where
  sequenceTupleA = sequenceA
  sequenceTupleA_ = sequenceA_

instance TupleFold x [x] where
  foldlTuple = foldl
  foldrTuple = foldr
  foldMapTuple = foldMap

instance TupleAddL x [x] [x] where
  (<++) = (:)

instance TupleAddR [x] x [x] where
  xs ++> x = xs ++ [x]

instance TupleAppend [u] [u] [u] where
  (+++) = (++)

#if MIN_VERSION_base(4,9,0)
instance TupleAddL x (NonEmpty x) (NonEmpty x) where
  (<++) = (<|)

instance TupleAddR (NonEmpty x) x (NonEmpty x) where
  ~(x :| xs) ++> xn = x :| (xs ++> xn)

instance TupleAppend (NonEmpty x) (NonEmpty x) (NonEmpty x) where
  (+++) = (<>)

instance Applicative f => SequenceTuple f (NonEmpty (f a)) (NonEmpty a) where
  sequenceTupleA = sequenceA
  sequenceTupleA_ = sequenceA_

instance TupleFold x (NonEmpty x) where
  foldlTuple = foldl
  foldrTuple = foldr
  foldMapTuple = foldMap
#endif
