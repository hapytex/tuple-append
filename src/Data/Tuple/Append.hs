{-# LANGUAGE FlexibleInstances, MagicHash, MultiParamTypeClasses, QuasiQuotes, Safe, UnboxedTuples #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans -Werror #-}

{-|
Module      : Data.Tuple.Append
Description : A module that contains typeclasses to prepend and append items and tuples into new tuples together with the corresponding instances.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module that contains typeclasses to prepend and append items and tuples into new tuples together with the corresponding instances.
-}

module Data.Tuple.Append(
    -- * Add an element to a tuple
    TupleAddL((<++)), TupleAddR((++>))
    -- * Append two tuples
  , TupleAppend((+++))
  ) where

import Data.Tuple.Append.Class(TupleAddL((<++)), TupleAddR((++>)), TupleAppend((+++)))
import Data.Tuple.Append.TemplateHaskell(defineTupleAddUpto, defineTupleAppendUpto)

[defineTupleAddUpto|61|]
[defineTupleAppendUpto|19|]
