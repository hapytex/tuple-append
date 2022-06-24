{-# LANGUAGE Safe, TemplateHaskellQuotes #-}

module Data.Tuple.Append.TemplateHaskell (
    defineTupleAppendUpto
  , tupleAdd
  ) where

import Control.Monad(replicateM)
import Data.Tuple.Append.Class(TupleAddL((<++)), TupleAddR((++>)))

import Language.Haskell.TH.Quote(QuasiQuoter(QuasiQuoter))
import Language.Haskell.TH.Syntax(Body(NormalB), Clause(Clause), Dec(FunD, InstanceD), Exp(TupE, VarE), Name, Pat(TildeP, TupP, VarP), Q, Type(AppT, ConT, TupleT, VarT), mkName)

_varZZ' :: Name
_varZZ' = mkName "zZ"

_varZZ :: Type
_varZZ = VarT _varZZ'

_patZZ :: Pat
_patZZ = VarP _varZZ'

_varNames :: [Name]
_varNames = map mkName (replicateM 2 ['a' .. 'z'])

_tupleVar' :: Int -> [Name] -> Type
_tupleVar' n ns = foldl AppT (TupleT n) (map VarT (take n ns))

_tupleVar :: Int -> Type
_tupleVar = (`_tupleVar'` _varNames)

_tupleP :: Int -> Pat
_tupleP n = TildeP (TupP (map VarP (take n _varNames)))

_tupleB :: [Name] -> Body
_tupleB = NormalB . TupE . map (Just . VarE)

tupleAdd :: Int -> Q [Dec]
tupleAdd n = return [
    InstanceD Nothing [] (ConT ''TupleAddL `AppT` _varZZ `AppT` _tupleVar n `AppT` (_tupleVar' (n+1) varN)) [FunD '(<++) [Clause [ _patZZ, _tupleP n ] (_tupleB varN) []]]
  , InstanceD Nothing [] (ConT ''TupleAddR `AppT` _varZZ `AppT` _tupleVar n `AppT` (_tupleVar' (n+1) varN')) [FunD '(++>) [Clause [ _tupleP n, _patZZ ] (_tupleB varN') []]]
  ]
    where varN = _varZZ' : take n _varNames
          varN' = take n _varNames ++ [_varZZ']

defineTupleAppendUpto :: QuasiQuoter
defineTupleAppendUpto = QuasiQuoter undefined undefined undefined (tupleAdd . read)
