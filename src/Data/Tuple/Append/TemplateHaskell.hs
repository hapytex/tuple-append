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

import Control.Monad((<=<), replicateM)
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
#if MIN_VERSION_template_haskell(2,16,0)
_tupleB = NormalB . TupE . map (Just . VarE)
#else
_tupleB = NormalB . TupE . map VarE
#endif

-- | Define a typeclass instance for 'TupleAppend' where it appens tuples with /m/ and /n/ items with /m/ and /n/ the parameters of the function.
tupleAppend
  :: Int  -- ^ The length /m/ of the first tuple.
  -> Int  -- ^ The length /n/ of the second tuple.
  -> Dec  -- ^ An instance of the 'TupleAppend' typeclass that appends tuples with lengths /m/ and /n/ to a tuple with length /m+n/.
tupleAppend m n = InstanceD Nothing [] (ConT ''TupleAppend `AppT` _tupleVar m `AppT` _tupleVar' n extras `AppT` _tupleVar (m+n)) [FunD '(+++) [Clause [ _tupleP m, tupleP' extras ] (_tupleB (take (m+n) _varNames)) []]]
  where extras = take n (drop m _varNames)

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
