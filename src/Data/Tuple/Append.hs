{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, Safe, QuasiQuotes #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans -Werror #-}

module Data.Tuple.Append(TupleAddL((<++)), TupleAddR((++>))) where

import Data.Tuple.Append.Class(TupleAddL((<++)), TupleAddR((++>)))
import Data.Tuple.Append.TemplateHaskell(defineTupleAddUpto, defineTupleAppendUpto)

[defineTupleAddUpto|61|]
[defineTupleAppendUpto|16|]
