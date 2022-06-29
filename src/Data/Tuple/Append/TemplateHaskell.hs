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
    -- * Quasiquoters for unboxed tuples
  , defineUnboxedTupleAppendFunctionsUpto
    -- * Functions to construct typeclass instance declarations
  , tupleAddL, tupleAddR, tupleAdd, tupleAppend, tupleAppendFor
    -- * Function declarations
  , boxedTupleAddLFun, boxedTupleAddRFun, boxedTupleAppendFun
  , unboxedTupleAddLFun, unboxedTupleAddRFun, unboxedTupleAppendFun
    -- * Function builders (for template Haskell)
  , makeBoxedTupleAddLFun, makeBoxedTupleAddRFun, makeBoxedTupleAppendFun
  , makeUnboxedTupleAddLFun, makeUnboxedTupleAddRFun, makeUnboxedTupleAppendFun
    -- * Create a function clause
    -- ** Boxed tuples
  , boxedAddLClause, boxedAddRClause, boxedAppendClause
    -- ** Unboxed tuples
  , unboxedAddLClause, unboxedAddRClause, unboxedAppendClause
  ) where

import Control.Monad((<=<))

import Data.Char(chr, ord)
import Data.Tuple.Append.Class(TupleAddL((<++)), TupleAddR((++>)), TupleAppend((+++)))

import Language.Haskell.TH.Lib(DecsQ)
import Language.Haskell.TH.Quote(QuasiQuoter(QuasiQuoter))
import Language.Haskell.TH.Syntax(
    Body(NormalB), Clause(Clause), Dec(FunD, InstanceD, SigD), Exp(TupE, UnboxedTupE, VarE), Name, Pat(TildeP, TupP, UnboxedTupP, VarP), Q, Type(AppT, ArrowT, ConT, TupleT, UnboxedTupleT, VarT)
  , mkName
  )

_nameZZ :: Name
_nameZZ = mkName "x"

_varZZ :: Type
_varZZ = VarT _nameZZ

_patZZ :: Pat
_patZZ = VarP _nameZZ

_varNames :: Char -> [Name]
_varNames c = map (mkName . (c :) . map (chr . (8272 +) . ord) . show) [1 :: Int ..]

_uNames :: [Name]
_uNames = _varNames 'u'

_vNames :: [Name]
_vNames = _varNames 'v'

_tupleVar' :: Int -> [Name] -> Type
_tupleVar' n ns = foldl AppT (TupleT n) (map VarT (take n ns))

_utupleVar' :: Int -> [Name] -> Type
_utupleVar' n ns = foldl AppT (UnboxedTupleT n) (map VarT (take n ns))

_tupleP'' :: ([Pat] -> Pat) -> [Name] -> Pat
_tupleP'' = (. map VarP)

_tupleP' :: [Name] -> Pat
_tupleP' = _tupleP'' (TildeP . TupP)

_utupleP' :: [Name] -> Pat
_utupleP' = _tupleP'' UnboxedTupP

_tupleRange :: Int -> [Int]
#if MIN_VERSION_ghc_prim(0,7,0)
_tupleRange = enumFromTo 0  -- 0 .. n
#else
_tupleRange = (0 :) . enumFromTo 2  -- 0 and 2 .. n
#endif

_tupleCheck :: Int -> Bool
#if MIN_VERSION_ghc_prim(0,7,0)
_tupleCheck = (0 <=)
#else
_tupleCheck 0 = True
_tupleCheck n = 2 <= n
#endif

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
_appendClause fp fe m n = _clause [ _tupleP'' fp um, _tupleP'' fp vn] (_tupleB' fe (um ++ vn))
  where um = take m _uNames
        vn = take n _vNames

#if MIN_VERSION_template_haskell(2,16,0)
_addLClause :: ([Pat] -> Pat) -> ([Maybe Exp] -> Exp) -> Int -> Name -> Dec
#else
_addLClause :: ([Pat] -> Pat) -> ([Exp] -> Exp) -> Int -> Name -> Dec
#endif
_addLClause fp fe n = _clause [ _patZZ, _tupleP'' fp vars] (_tupleB' fe (_nameZZ : vars))
  where vars = take n _vNames

#if MIN_VERSION_template_haskell(2,16,0)
_addRClause :: ([Pat] -> Pat) -> ([Maybe Exp] -> Exp) -> Int -> Name -> Dec
#else
_addRClause :: ([Pat] -> Pat) -> ([Exp] -> Exp) -> Int -> Name -> Dec
#endif
_addRClause fp fe n = _clause [_tupleP'' fp vars, _patZZ] (_tupleB' fe (vars ++> _nameZZ))
  where vars = take n _vNames

-- | Create a function declaration to append two boxed tuples together in a new boxed tuple. This only contains a declaration for the /body/ of the function, not a type signature.
boxedAppendClause
  :: Int  -- ^ The number of items for the first boxed tuple parameter.
  -> Int  -- ^ The number of items for the second boxed tuple parameter.
  -> Name  -- ^ The name of the function to define.
  -> Dec  -- ^ A function declaration that only contains the body of the function.
boxedAppendClause = _appendClause TupP TupE

-- | Create a function declaration to append two unboxed tuples together in a new unboxed tuple. This only contains a declaration for the /body/ of the function, not a type signature.
unboxedAppendClause
  :: Int  -- ^ The number of items for the first unboxed tuple parameter.
  -> Int  -- ^ The number of items for the second unboxed tuple parameter.
  -> Name  -- ^ The name of the function to define.
  -> Dec  -- ^ A function declaration that only contains the body of the function.
unboxedAppendClause = _appendClause UnboxedTupP UnboxedTupE

-- | Create a function declaration to add an item to the left side of a boxed tuple in a new boxed tuple. This only contains a declaration for the /body/ of the function, not a type signature.
boxedAddLClause
  :: Int  -- The number of items of the boxed tuple to add an item to.
  -> Name  -- ^ The name of the function to define.
  -> Dec  -- ^ A function declaration that only contains the body of the function.
boxedAddLClause = _addLClause TupP TupE

-- | Create a function declaration to add an item to the left side of an unboxed tuple in a new unboxed tuple. This only contains a declaration for the /body/ of the function, not a type signature.
unboxedAddLClause
  :: Int  -- The number of items of the unboxed tuple to add an item to.
  -> Name  -- ^ The name of the function to define.
  -> Dec  -- ^ A function declaration that only contains the body of the function.
unboxedAddLClause = _addLClause UnboxedTupP UnboxedTupE

-- | Create a function declaration to add an item to the right side of a boxed tuple in a new boxed tuple. This only contains a declaration for the /body/ of the function, not a type signature.
boxedAddRClause
  :: Int  -- The number of items of the boxed tuple to add an item to.
  -> Name  -- ^ The name of the function to define.
  -> Dec  -- ^ A function declaration that only contains the body of the function.
boxedAddRClause = _addRClause TupP TupE

-- | Create a function declaration to add an item to the right side of an unboxed tuple in a new unboxed tuple. This only contains a declaration for the /body/ of the function, not a type signature.
unboxedAddRClause
  :: Int  -- The number of items of the unboxed tuple to add an item to.
  -> Name  -- ^ The name of the function to define.
  -> Dec  -- ^ A function declaration that only contains the body of the function.
unboxedAddRClause = _addRClause UnboxedTupP UnboxedTupE

_tupleB :: [Name] -> Body
_tupleB = _tupleB' TupE

_utupleB :: [Name] -> Body
_utupleB = _tupleB' UnboxedTupE

_arr :: Type -> Type -> Type
_arr l r = ArrowT `AppT` l `AppT` r

_tupType :: [Type] -> Type
_tupType ns = foldl AppT (TupleT (length ns)) ns

_utupType :: [Type] -> Type
_utupType ns = foldl AppT (UnboxedTupleT (length ns)) ns

_signature :: Name -> Type -> Type -> Type -> Dec
_signature nm ta tb tc = SigD nm (ta `_arr` (tb `_arr` tc))

-- | Create a function declaration with signature to append a boxed tuple with the types of the first list with a boxed tuple with the types of the second list. This will contain two 'Dec' items: one for the signature and one for the function declaration itself.
boxedTupleAppendFun
  :: Name  -- ^ The name of the function to construct.
  -> [Type]  -- ^ The types of the first boxed tuple, should contain at least two elements.
  -> [Type]  -- ^ The types of the second boxed tuple, should contain at least two elements.
  -> [Dec]  -- ^ A list that contains two 'Dec' objects: one for the function signature declaration, and one for the function declaration.
boxedTupleAppendFun nm l r = [
    _signature nm (_tupType l) (_tupType r) (_tupType (l ++ r))
  , boxedAppendClause (length l) (length r) nm
  ]

-- | Create a function declaration with signature to append an unboxed tuple with the types of the first list with an unboxed tuple with the types of the second list. This will contain two 'Dec' items: one for the signature and one for the function declaration itself.
unboxedTupleAppendFun
  :: Name  -- ^ The name of the function to construct.
  -> [Type]  -- ^ The types of the first boxed tuple, should contain at least two elements, all types can be lifted or unlifted types or type variables.
  -> [Type]  -- ^ The types of the second boxed tuple, should contain at least two elements, all types can be lifted or unlifted types or type variables.
  -> [Dec]  -- ^ A list that contains two 'Dec' objects: one for the function signature declaration, and one for the function declaration.
unboxedTupleAppendFun nm l r = [
    _signature nm (_utupType l) (_utupType r) (_utupType (l ++ r))
  , unboxedAppendClause (length l) (length r) nm
  ]

-- | Create a function declaration with signature to add an item with a given type to the left side of a boxed tuple with the types of the given list. This will contain two 'Dec' items: one for the signature and one for the function declaration itself.
boxedTupleAddLFun
  :: Name  -- ^ The name of the function to construct.
  -> Type  -- ^ The type of the item to add to the tuple.
  -> [Type]  -- ^ The types of the boxed tuple, should contain at least two elements.
  -> [Dec]  -- ^ A list that contains two 'Dec' objects: one for the function signature declaration, and one for the function declaration.
boxedTupleAddLFun nm t ts = [
    _signature nm t (_tupType ts) (_tupType (t : ts))
  , boxedAddLClause (length ts) nm
  ]

-- | Create a function declaration with signature to add an item with a given type to the left side of an unboxed tuple with the types of the given list. This will contain two 'Dec' items: one for the signature and one for the function declaration itself.
unboxedTupleAddLFun
  :: Name  -- ^ The name of the function to construct.
  -> Type  -- ^ The type of the item to add to the tuple, this can be a lifted or unlifted type or a type variable.
  -> [Type]  -- ^ The types of the boxed tuple, should contain at least two elements, all types can be lifted or unlifted types or type variables.
  -> [Dec]  -- ^ A list that contains two 'Dec' objects: one for the function signature declaration, and one for the function declaration.
unboxedTupleAddLFun nm t ts = [
    _signature nm t (_utupType ts) (_utupType (t : ts))
  , unboxedAddLClause (length ts) nm
  ]

-- | Create a function declaration with signature to add an item with a given type to the right side of a boxed tuple with the types of the given list. This will contain two 'Dec' items: one for the signature and one for the function declaration itself.
boxedTupleAddRFun
  :: Name  -- ^ The name of the function to construct.
  -> [Type]  -- ^ The types of the boxed tuple, should contain at least two elements.
  -> Type  -- ^ The type of the item to add to the tuple.
  -> [Dec]  -- ^ A list that contains two 'Dec' objects: one for the function signature declaration, and one for the function declaration.
boxedTupleAddRFun nm ts t = [
    _signature nm (_tupType ts) t (_tupType (ts ++> t))
  , boxedAddRClause (length ts) nm
  ]

-- | Create a function declaration with signature to add an item with a given type to the right side of an unboxed tuple with the types of the given list. This will contain two 'Dec' items: one for the signature and one for the function declaration itself.
unboxedTupleAddRFun
  :: Name  -- ^ The name of the function to construct.
  -> [Type]  -- ^ The types of the boxed tuple, should contain at least two elements, all types can be lifted or unlifted types or type variables.
  -> Type  -- ^ The type of the item to add to the tuple, this can be a lifted or unlifted type or a type variable.
  -> [Dec]  -- ^ A list that contains two 'Dec' objects: one for the function signature declaration, and one for the function declaration.
unboxedTupleAddRFun nm ts t = [
    _signature nm (_utupType ts) t (_utupType (ts ++> t))
  , unboxedAddRClause (length ts) nm
  ]

-- | Create a function declaration with signature to append a boxed tuple with the types of the first list with a boxed tuple with the types of the second list. This function can be used with template Haskell.
makeBoxedTupleAppendFun
  :: Name  -- ^ The name of the function to construct.
  -> [Type]  -- ^ The types of the first boxed tuple, should contain at least two elements.
  -> [Type]  -- ^ The types of the second boxed tuple, should contain at least two elements.
  -> DecsQ  -- ^ A builder to construct the declaration of the signature and a body of the function to append the tuples.
makeBoxedTupleAppendFun nm l = pure . boxedTupleAppendFun nm l

-- | Create a function declaration with signature to append an unboxed tuple with the types of the first list with an unboxed tuple with the types of the second list. This function can be used with template Haskell.
makeUnboxedTupleAppendFun
  :: Name  -- ^ The name of the function to construct.
  -> [Type]  -- ^ The types of the first boxed tuple, should contain at least two elements, all types can be lifted or unlifted types or type variables.
  -> [Type]  -- ^ The types of the second boxed tuple, should contain at least two elements, all types can be lifted or unlifted types or type variables.
  -> DecsQ  -- ^ A builder to construct the declaration of the signature and a body of the function to append the tuples.
makeUnboxedTupleAppendFun nm l = pure . unboxedTupleAppendFun nm l

-- | Create a function declaration with signature to add an item with a given type to the left side of a boxed tuple with the types of the given list. This function can be used with template Haskell.
makeBoxedTupleAddLFun
  :: Name  -- ^ The name of the function to construct.
  -> Type  -- ^ The type of the item to add to the tuple.
  -> [Type]  -- ^ The types of the boxed tuple, should contain at least two elements.
  -> DecsQ  -- ^ A builder to construct the declaration of the signature and a body of the function to add an element at the left side of a tuple.
makeBoxedTupleAddLFun nm t = pure . boxedTupleAddLFun nm t

-- | Create a function declaration with signature to add an item with a given type to the left side of an unboxed tuple with the types of the given list. This function can be used with template Haskell.
makeUnboxedTupleAddLFun
  :: Name  -- ^ The name of the function to construct.
  -> Type  -- ^ The type of the item to add to the tuple, this can be a lifted or unlifted type or a type variable.
  -> [Type]  -- ^ The types of the boxed tuple, should contain at least two elements, all types can be lifted or unlifted types or type variables.
  -> DecsQ  -- ^ A builder to construct the declaration of the signature and a body of the function to add an element at the left side of a tuple.
makeUnboxedTupleAddLFun nm t = pure . unboxedTupleAddLFun nm t

-- | Create a function declaration with signature to add an item with a given type to the right side of a boxed tuple with the types of the given list. This function can be used with template Haskell.
makeBoxedTupleAddRFun
  :: Name  -- ^ The name of the function to construct.
  -> [Type]  -- ^ The types of the boxed tuple, should contain at least two elements.
  -> Type  -- ^ The type of the item to add to the tuple.
  -> DecsQ  -- ^ A builder to construct the declaration of the signature and a body of the function to add an element at the right side of a tuple.
makeBoxedTupleAddRFun nm ts = pure . boxedTupleAddRFun nm ts

-- | Create a function declaration with signature to add an item with a given type to the right side of an unboxed tuple with the types of the given list. This function can be used with template Haskell.
makeUnboxedTupleAddRFun
  :: Name  -- ^ The name of the function to construct.
  -> [Type]  -- ^ The types of the boxed tuple, should contain at least two elements, all types can be lifted or unlifted types or type variables.
  -> Type  -- ^ The type of the item to add to the tuple, this can be a lifted or unlifted type or a type variable.
  -> DecsQ  -- ^ A builder to construct the declaration of the signature and a body of the function to add an element at the right side of a tuple.
makeUnboxedTupleAddRFun nm ts = pure . unboxedTupleAddRFun nm ts

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
tupleAppend m n = _simpleInstanceAppend (_tupleVar' m _uNames) (_tupleVar' n _vNames) (_tupleVar' (m+n) (take m _uNames ++ _vNames)) (boxedAppendClause m n)

-- | Define typeclass instances for 'TupleAppend' that will append any tuple of at least size two with any tuple of at least size two such that the sum is the given number.
tupleAppendFor
  :: Int  -- ^ The given number /l/ for which typeclass instances of 'TupleAppend' will be made with /m/ and /n/ such that /l=m+n/.
  -> [Dec]  -- ^ A list of typelcass instances for the 'TupleAppend' typeclass.
tupleAppendFor l = [tupleAppend m n | m <- _tupleRange l, let n = l - m, _tupleCheck n ]

-- | Define a typeclass instance for 'TupleAddL' for a tuple with /n/ elements and an item to construct a tuple with /n+1/ elements where the item is added at the left side.
tupleAddL
  :: Int  -- ^ The given length /n/ of the tuples to prepend and append with an element.
  -> Dec  -- ^ A type instance declaration for an instance of the 'TupleAddL' typeclass for an /n/-tuple.
tupleAddL n = _simpleInstanceAddL _varZZ (_tupleVar' n _vNames) (_tupleVar' (n+1) (_nameZZ : _vNames)) (boxedAddLClause n)

-- | Define a typeclass instance for 'TupleAddR' for a tuple with /n/ elements and an item to construct a tuple with /n+1/ elements where the item is added at the right side.
tupleAddR
  :: Int  -- ^ The given length /n/ of the tuples to prepend and append with an element.
  -> Dec  -- ^ A type instance declaration for an instance of the 'TupleAddR' typeclass for an /n/-tuple.
tupleAddR n = _simpleInstanceAddR (_tupleVar' n _vNames) _varZZ (_tupleVar' (n+1) (take n _vNames ++> _nameZZ)) (boxedAddRClause n)

-- | Define typeclass instances for 'TupleAddL' and 'TupleAddR' for a tuple with /n/ elements and an item to construct a tuple with /n+1/ elements where the item is added at the left or the right side.
tupleAdd
  :: Int  -- ^ The given length /n/ of the tuples to prepend and append with an element.
  -> [Dec]  -- ^ A list of two type instance declarations that contains typeclass instances for 'TupleAddL' and 'TupleAddR'.
tupleAdd n
  | _tupleCheck n && _tupleCheck (n+1) = [tupleAddL n, tupleAddR n]
  | otherwise = []

_errorQuasiQuoter :: a -> Q b
_errorQuasiQuoter = const (fail "The quasi quoter can only be used to define declarations")

-- | A 'QuasiQuoter' that constructs instances for 'TupleAddL' and 'TupleAddR' for tuples up to length /n/ where /n/ is read as text input for the quasi quoter.
defineTupleAddUpto
  :: QuasiQuoter  -- ^ A 'QuasiQuoter' that will construct typeclass instance declarations.
defineTupleAddUpto = QuasiQuoter _errorQuasiQuoter _errorQuasiQuoter _errorQuasiQuoter (pure . (tupleAdd <=< enumFromTo 0 . read))

-- | A 'QuasiQuoter' that constructs instances for 'TupleAppend' for tuples up to length /n/ where /n/ is read as text input for the quasi quoter. For a single /n/ it thus will construct /n-4/ instances for each tuple length.
defineTupleAppendUpto
  :: QuasiQuoter  -- ^ A 'QuasiQuoter' that will construct typeclass instance declarations.
defineTupleAppendUpto = QuasiQuoter _errorQuasiQuoter _errorQuasiQuoter _errorQuasiQuoter (pure . (tupleAppendFor <=< enumFromTo 0 . read))

-- | A 'QuasiQuoter' that constructs instances for 'TupleAppend' for tuples up to length /n/ where /n/ is read as text input for the quasi quoter. For a single /n/ it thus will construct /n-4/ instances for each tuple length.
defineUnboxedTupleAppendFunctionsUpto
  :: QuasiQuoter  -- ^ A 'QuasiQuoter' that will construct typeclass instance declarations.
defineUnboxedTupleAppendFunctionsUpto = QuasiQuoter _errorQuasiQuoter _errorQuasiQuoter _errorQuasiQuoter (_unboxedTupleConcats . read)

_unboxedTupleConcats :: Int -> DecsQ
_unboxedTupleConcats r = pure [ u | m <- [r-2, r-3 .. 2], n <- [r-m-2, r-m-3 .. 2], u <- unboxedTupleAppendFun (mkName ("unboxedAppend_" ++ show m ++ "_" ++ show n)) (map VarT (take m _uNames)) (map VarT (take n _vNames)) ]
