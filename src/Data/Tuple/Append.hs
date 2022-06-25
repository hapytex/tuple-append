{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, Safe, QuasiQuotes #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans -Werror #-}

{-|
Module      : Data.Tuple.Append
Description : A module that contains typeclasses to prepend and append items and tuples into new tuples together with the corresponding instances.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module that contains typeclasses to prepend and append items and tuples into new tuples together with the corresponding instances.
-}

module Data.Tuple.Append(TupleAddL((<++)), TupleAddR((++>)), TupleAppend((+++))) where

import Data.Tuple.Append.Class(TupleAddL((<++)), TupleAddR((++>)), TupleAppend((+++)))
import Data.Tuple.Append.TemplateHaskell(defineTupleAddUpto, defineTupleAppendUpto)

[defineTupleAddUpto|61|]
[defineTupleAppendUpto|16|]
