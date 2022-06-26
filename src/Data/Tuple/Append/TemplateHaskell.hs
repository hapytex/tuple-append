{-# LANGUAGE CPP, Safe, TemplateHaskellQuotes #-}

{-|
Module      : Data.Tuple.Append.TemplateHaskell
Description : A module that defines template Haskell expressions to define typeclass instances to prepend and append tuples.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module hat defines template Haskell expressions to define typeclass instances to prepend and append tuples.
-}

module Data.Tuple.Append.TemplateHaskell (
    -- * Quasiquoters for typeclass instances
    defineTupleAddUpto, defineTupleAppendUpto
    -- * Functions to construct typeclass instance declarations
  , tupleAdd, tupleAppend, tupleAppendFor
  ) where

import Control.Monad((<=<))

import Data.Char(chr, ord)
import Data.Tuple.Append.Class(TupleAddL((<++)), TupleAddR((++>)), TupleAppend((+++)))

import Language.Haskell.TH.Quote(QuasiQuoter(QuasiQuoter))
import Language.Haskell.TH.Syntax(Body(NormalB), Clause(Clause), Dec(FunD, InstanceD, SigD), Exp(TupE, UnboxedTupE, VarE), Name, Pat(TildeP, TupP, UnboxedTupP, VarP), Type(AppT, ArrowT, ConT, TupleT, UnboxedTupleT, VarT), mkName)

_varZZ' :: Name
_varZZ' = mkName "x"

_varZZ :: Type
_varZZ = VarT _varZZ'

_patZZ :: Pat
_patZZ = VarP _varZZ'

_varNames :: [Name]
_varNames = map (mkName . ('e' :) . map (chr . (8272 +) . ord) . show) [1 :: Int ..]

_tupleVar' :: Int -> [Name] -> Type
_tupleVar' n ns = foldl AppT (TupleT n) (map VarT (take n ns))

_utupleVar' :: Int -> [Name] -> Type
_utupleVar' n ns = foldl AppT (UnboxedTupleT n) (map VarT (take n ns))

_tupleVar :: Int -> Type
_tupleVar = (`_tupleVar'` _varNames)

_tupleP'' :: ([Pat] -> Pat) -> [Name] -> Pat
_tupleP'' f = TildeP . f . map VarP

_tupleP' :: [Name] -> Pat
_tupleP' = _tupleP'' TupP

_utupleP' :: [Name] -> Pat
_utupleP' = _tupleP'' UnboxedTupP

_tupleP :: Int -> Pat
_tupleP = _tupleP' . (`take` _varNames)

_utupleP :: Int -> Pat
_utupleP = _utupleP' . (`take` _varNames)

#if MIN_VERSION_template_haskell(2,16,0)
_tupleB' :: ([Maybe Exp] -> Exp) -> [Name] -> Body
_tupleB' f = NormalB . f . map (Just . VarE)
#else
_tupleB' :: ([Exp] -> Exp) -> [Name] -> Body
_tupleB' f = NormalB . f . map VarE
#endif

_clause :: [Pat] -> Body -> Name -> Dec
_clause ps b = (`FunD` [Clause ps b []])

#if MIN_VERSION_template_haskell(2,16,0)
_appendClause :: ([Pat] -> Pat) -> ([Maybe Exp] -> Exp) -> Int -> Int -> Name -> Dec
#else
_appendClause :: ([Pat] -> Pat) -> ([Exp] -> Exp) -> Int -> Int -> Name -> Dec
#endif
_appendClause fp fe m n = _clause [ _tupleP'' fp (take m _varNames), _tupleP'' fp (take n (drop m _varNames))] (_tupleB' fe (take (m+n) _varNames))

#if MIN_VERSION_template_haskell(2,16,0)
_addLClause :: ([Pat] -> Pat) -> ([Maybe Exp] -> Exp) -> Int -> Name -> Dec
#else
_addLClause :: ([Pat] -> Pat) -> ([Exp] -> Exp) -> Int -> Name -> Dec
#endif
_addLClause fp fe n = _clause [ _patZZ, _tupleP'' fp vars] (_tupleB' fe (_varZZ' : vars))
  where vars = take n _varNames

#if MIN_VERSION_template_haskell(2,16,0)
_addRClause :: ([Pat] -> Pat) -> ([Maybe Exp] -> Exp) -> Int -> Name -> Dec
#else
_addRClause :: ([Pat] -> Pat) -> ([Exp] -> Exp) -> Int -> Name -> Dec
#endif
_addRClause fp fe n = _clause [_tupleP'' fp vars, _patZZ] (_tupleB' fe (vars ++ [_varZZ']))
  where vars = take n _varNames

boxedAppendClause :: Int -> Int -> Name -> Dec
boxedAppendClause = _appendClause TupP TupE

unboxedAppendClause :: Int -> Int -> Name -> Dec
unboxedAppendClause = _appendClause UnboxedTupP UnboxedTupE

boxedAddLClause :: Int -> Name -> Dec
boxedAddLClause = _addLClause TupP TupE

unboxedAddLClause :: Int -> Name -> Dec
unboxedAddLClause = _addLClause UnboxedTupP UnboxedTupE

boxedAddRClause :: Int -> Name -> Dec
boxedAddRClause = _addRClause TupP TupE

unboxedAddRClause :: Int -> Name -> Dec
unboxedAddRClause = _addRClause UnboxedTupP UnboxedTupE

_tupleB :: [Name] -> Body
_tupleB = _tupleB' TupE

_utupleB :: [Name] -> Body
_utupleB = _tupleB' UnboxedTupE

_arr :: Type -> Type -> Type
_arr l r = ArrowT `AppT` l `AppT` r

_tupType :: [Type] -> Type
_tupType ns = foldl AppT (TupleT (length ns)) ns

tupleAppendFun :: Name -> [Type] -> [Type] -> [Dec]
tupleAppendFun nm l r = [
    SigD nm (_tupType l `_arr` (_tupType r `_arr` _tupType (l ++ r)))
  , boxedAppendClause (length l) (length r) nm
  ]

unboxedTupleAppendFun :: Name -> [Type] -> [Type] -> [Dec]
unboxedTupleAppendFun nm l r = [
    SigD nm (_arr tl (_arr tr tlr))  -- TODO: unboxed type
  , unboxedAppendClause (length l) (length r) nm
  ]
  where tl = _tupType l
        tr = _tupType r
        tlr = _tupType (l ++ r)

_simpleInstance :: Name -> Name -> Type -> Type -> Type -> (Name -> Dec) -> Dec
_simpleInstance tc f tca tcb tcc d = InstanceD Nothing [] (ConT tc `AppT` tca `AppT` tcb `AppT` tcc) [d f]

_simpleInstanceAppend :: Type -> Type -> Type -> (Name -> Dec) -> Dec
_simpleInstanceAppend = _simpleInstance ''TupleAppend '(+++)

_simpleInstanceAddL :: Type -> Type -> Type -> (Name -> Dec) -> Dec
_simpleInstanceAddL = _simpleInstance ''TupleAddL '(<++)

_simpleInstanceAddR :: Type -> Type -> Type -> (Name -> Dec) -> Dec
_simpleInstanceAddR = _simpleInstance ''TupleAddR '(++>)

-- | Define a typeclass instance for 'TupleAppend' where it appens tuples with /m/ and /n/ items with /m/ and /n/ the parameters of the function.
tupleAppend
  :: Int  -- ^ The length /m/ of the first tuple.
  -> Int  -- ^ The length /n/ of the second tuple.
  -> Dec  -- ^ An instance of the 'TupleAppend' typeclass that appends tuples with lengths /m/ and /n/ to a tuple with length /m+n/.
tupleAppend m n = _simpleInstanceAppend (_tupleVar m) (_tupleVar' n (drop m _varNames)) (_tupleVar (m+n)) (boxedAppendClause m n)

-- | Define typeclass instances for 'TupleAppend' that will append any tuple of at least size two with any tuple of at least size two such that the sum is the given number.
tupleAppendFor
  :: Int  -- ^ The given number /l/ for which typeclass instances of 'TupleAppend' will be made with /m/ and /n/ such that /l=m+n/.
  -> [Dec]  -- ^ A list of typelcass instances for the 'TupleAppend' typeclass.
tupleAppendFor n = [tupleAppend m (n-m) | m <- [2 .. n - 2]]

-- | Define typeclass instances for 'TupleAddL' and 'TupleAddR' for a tuple with /n/ elements and an item to construct a tuple with /n+1/ elements where the item is added at the left or the right side.
tupleAdd
  :: Int  -- ^ The given length /n/ of the tuples to prepend and append with an element.
  -> [Dec]  -- ^ A list of two type instance declarations that contains typeclass instances for 'TupleAddL' and 'TupleAddR'.
tupleAdd n = [
    InstanceD Nothing [] (ConT ''TupleAddL `AppT` _varZZ `AppT` _tupleVar n `AppT` _tupleVar' (n+1) varN) [FunD '(<++) [Clause [ _patZZ, _tupleP n ] (_tupleB varN) []]]
  , InstanceD Nothing [] (ConT ''TupleAddR `AppT` _varZZ `AppT` _tupleVar n `AppT` _tupleVar' (n+1) varN') [FunD '(++>) [Clause [ _tupleP n, _patZZ ] (_tupleB varN') []]]
  ]
  where varN = _varZZ' : take n _varNames
        varN' = take n _varNames ++ [_varZZ']

_errorQuasiQuoter :: a
_errorQuasiQuoter = error "The quasi quoter can only be used to define declarations"

-- | A 'QuasiQuoter' that constructs instances for 'TupleAddL' and 'TupleAddR' for tuples up to length /n/ where /n/ is read as text input for the quasi quoter.
defineTupleAddUpto
  :: QuasiQuoter  -- ^ A 'QuasiQuoter' that will construct typeclass instance declarations.
defineTupleAddUpto = QuasiQuoter _errorQuasiQuoter _errorQuasiQuoter _errorQuasiQuoter (pure . (tupleAdd <=< enumFromTo 2 . read))

-- | A 'QuasiQuoter' that constructs instances for 'TupleAppend' for tuples up to length /n/ where /n/ is read as text input for the quasi quoter. For a single /n/ it thus will construct /n-4/ instances for each tuple length.
defineTupleAppendUpto
  :: QuasiQuoter  -- ^ A 'QuasiQuoter' that will construct typeclass instance declarations.
defineTupleAppendUpto = QuasiQuoter _errorQuasiQuoter _errorQuasiQuoter _errorQuasiQuoter (pure . (tupleAppendFor <=< enumFromTo 4 . read))
