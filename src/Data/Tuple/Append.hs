{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans -Werror #-}

-- |
-- Module      : Data.Tuple.Append
-- Description : A module that contains typeclasses to prepend and append items and tuples into new tuples together with the corresponding instances.
-- Maintainer  : hapytexeu+gh@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- A module that contains typeclasses to prepend and append items and tuples into new tuples together with the corresponding instances.
module Data.Tuple.Append
  ( -- * Add an element to a tuple
    TupleAddL ((<++)),
    TupleAddR ((++>)),

    -- * Append two tuples
    TupleAppend ((+++)),

    -- * Sequence a tuple
    SequenceTuple (sequenceTupleA, sequenceTupleA_),
    -- * Folding a tuple

    FoldTuple (foldlTuple, foldrTuple, foldMapTuple)
  )
where

import Data.Tuple.Append.Class (SequenceTuple (sequenceTupleA, sequenceTupleA_), TupleAddL ((<++)), TupleAddR ((++>)), TupleAppend ((+++)), FoldTuple(foldlTuple, foldrTuple, foldMapTuple))
import Data.Tuple.Append.TemplateHaskell (defineSequenceTupleUpTo, defineTupleAddUpto, defineTupleAppendUpto, defineFoldTupleUpTo)

[defineTupleAddUpto|61|]

[defineTupleAppendUpto|19|]

[defineSequenceTupleUpTo|62|]

[defineFoldTupleUpTo|62|]
