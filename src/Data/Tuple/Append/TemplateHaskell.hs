{-# LANGUAGE Safe, TemplateHaskellQuotes #-}

module Data.Tuple.Append.TemplateHaskell (
    defineTupleAddUpto, defineTupleAppendUpto
  , tupleAdd, tupleAppend, tupleAppendFor
  ) where

import Control.Monad(replicateM)
import Data.Tuple.Append.Class(TupleAddL((<++)), TupleAddR((++>)), TupleAppend((+++)))

import Language.Haskell.TH.Quote(QuasiQuoter(QuasiQuoter))
import Language.Haskell.TH.Syntax(Body(NormalB), Clause(Clause), Dec(FunD, InstanceD), Exp(TupE, VarE), Name, Pat(TildeP, TupP, VarP), Type(AppT, ConT, TupleT, VarT), mkName)

_varZZ' :: Name
_varZZ' = mkName "zZ"

_varZZ :: Type
_varZZ = VarT _varZZ'

_patZZ :: Pat
_patZZ = VarP _varZZ'

_varNames :: [Name]
_varNames = map (mkName . ('_' :)) (replicateM 2 ['a' .. 'z'])

_tupleVar' :: Int -> [Name] -> Type
_tupleVar' n ns = foldl AppT (TupleT n) (map VarT (take n ns))

_tupleVar :: Int -> Type
_tupleVar = (`_tupleVar'` _varNames)

tupleP' :: [Name] -> Pat
tupleP' = TildeP . TupP . map VarP

_tupleP :: Int -> Pat
_tupleP = tupleP' . (`take` _varNames)

_tupleB :: [Name] -> Body
_tupleB = NormalB . TupE . map (Just . VarE)

tupleAppend :: Int -> Int -> Dec
tupleAppend n m = InstanceD Nothing [] (ConT ''TupleAppend `AppT` _tupleVar n `AppT` _tupleVar' m extras `AppT` _tupleVar (n+m)) [FunD '(+++) [Clause [ _tupleP n, tupleP' extras ] (_tupleB (take (n+m) _varNames)) []]]
  where extras = take m (drop n _varNames)

tupleAppendFor :: Int -> [Dec]
tupleAppendFor n = [tupleAppend m (n-m) | m <- [2 .. n - 2]]

tupleAdd :: Int -> [Dec]
tupleAdd n = [
    InstanceD Nothing [] (ConT ''TupleAddL `AppT` _varZZ `AppT` _tupleVar n `AppT` (_tupleVar' (n+1) varN)) [FunD '(<++) [Clause [ _patZZ, _tupleP n ] (_tupleB varN) []]]
  , InstanceD Nothing [] (ConT ''TupleAddR `AppT` _varZZ `AppT` _tupleVar n `AppT` (_tupleVar' (n+1) varN')) [FunD '(++>) [Clause [ _tupleP n, _patZZ ] (_tupleB varN') []]]
  ]
  where varN = _varZZ' : take n _varNames
        varN' = take n _varNames ++ [_varZZ']

defineTupleAddUpto :: QuasiQuoter
defineTupleAddUpto = QuasiQuoter undefined undefined undefined (return . (tupleAdd =<<) . enumFromTo 2 . read)

defineTupleAppendUpto :: QuasiQuoter
defineTupleAppendUpto = QuasiQuoter undefined undefined undefined (return . (tupleAppendFor =<<) . enumFromTo 2 . read)
