{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, Safe, QuasiQuotes #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans -Werror #-}

module Data.Tuple.Append(TupleAddL((<++)), TupleAddR((++>)), TupleAppend((+++))) where

import Data.Tuple.Append.Class(TupleAddL((<++)), TupleAddR((++>)), TupleAppend((+++)))
import Data.Tuple.Append.TemplateHaskell(defineTupleAddUpto, defineTupleAppendUpto)

[defineTupleAddUpto|61|]
[defineTupleAppendUpto|16|]

instance TupleAddL a [a] [a] where
  (<++) = (:)

instance TupleAddR a [a] [a] where
  xs ++> x = xs ++ [x]

instance TupleAppend [a] [a] [a] where
  (+++) = (++)
