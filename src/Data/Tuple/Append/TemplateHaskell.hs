{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

{- |
 Module      : Data.Tuple.Append.TemplateHaskell
 Description : A module that defines template Haskell expressions to define typeclass instances to prepend and append tuples.
 Maintainer  : hapytexeu+gh@gmail.com
 Stability   : experimental
 Portability : POSIX

 A module hat defines template Haskell expressions to define typeclass instances to prepend and append tuples.
-}
module Data.Tuple.Append.TemplateHaskell (
    -- * Quasiquoters for typeclass instances
    defineTupleAddUpto,
    defineTupleAppendUpto,
    defineSequenceTupleUpTo,
    defineFoldTupleUpTo,

    -- * Quasiquoters for unboxed tuples
    defineUnboxedTupleAppendFunctionsUpto,

    -- * Functions to construct typeclass instance declarations
    tupleAddL,
    tupleAddR,
    tupleAdd,
    tupleAppend,
    tupleAppendFor,
    sequenceTuple,
    sequenceTupleFor,

    -- * Function declarations
    boxedTupleAddLFun,
    boxedTupleAddRFun,
    boxedTupleAppendFun,
    unboxedTupleAddLFun,
    unboxedTupleAddRFun,
    unboxedTupleAppendFun,

    -- * Function builders (for template Haskell)
    makeBoxedTupleAddLFun,
    makeBoxedTupleAddRFun,
    makeBoxedTupleAppendFun,
    makeUnboxedTupleAddLFun,
    makeUnboxedTupleAddRFun,
    makeUnboxedTupleAppendFun,

    -- * Create a function clause

    -- ** Boxed tuples
    boxedAddLClause,
    boxedAddRClause,
    boxedAppendClause,
    sequenceClauseA,
    sequenceClauseA_,

    -- ** Unboxed tuples
    unboxedAddLClause,
    unboxedAddRClause,
    unboxedAppendClause,
) where

#if MIN_VERSION_base(4,8,0)
#else
import Data.Monoid((<>))
#endif
import Control.Monad ((<=<))
import Data.Char (chr, ord)
import Data.Tuple.Append.Class (FoldTuple (foldMapTuple, foldlTuple, foldrTuple), SequenceTuple (sequenceTupleA, sequenceTupleA_), TupleAddL ((<++)), TupleAddR ((++>)), TupleAppend ((+++)))
import Language.Haskell.TH.Lib (DecsQ)
import Language.Haskell.TH.Quote (QuasiQuoter (QuasiQuoter))
import Language.Haskell.TH.Syntax (
    Body (NormalB),
    Clause (Clause),
    Cxt,
    Dec (FunD, InstanceD, SigD),
    Exp (AppE, ConE, TupE, UnboxedTupE, VarE),
    Name,
    Pat (TildeP, TupP, UnboxedTupP, VarP),
    Q,
    Type (AppT, ArrowT, ConT, TupleT, UnboxedTupleT, VarT),
    mkName,
    tupleDataName,
 )

_nameZZ :: Name
_nameZZ = mkName "x"

_varZZ :: Type
_varZZ = VarT _nameZZ

_expZZ :: Exp
_expZZ = VarE _nameZZ

_patZZ :: Pat
_patZZ = VarP _nameZZ

_nameFF :: Name
_nameFF = mkName "f"

_patFF :: Pat
_patFF = VarP _nameFF

_varFF :: Type
_varFF = VarT _nameFF

_expFF :: Exp
_expFF = VarE _nameFF

_varNames :: Char -> [Name]
_varNames c = map (mkName . (c :) . map (chr . (0x2050 +) . ord) . show) [1 :: Int ..]

_uNames :: [Name]
_uNames = _varNames 'u'

_vNames :: [Name]
_vNames = _varNames 'v'

_tupleVar' :: Int -> [Name] -> Type
_tupleVar' n ns = foldl AppT (TupleT n) (map VarT (take n ns))

_tupleVar'' :: Int -> Type -> [Name] -> Type
_tupleVar'' n f ns = foldl AppT (TupleT n) (map ((f `AppT`) . VarT) (take n ns))

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

_sequenceExprA :: Int -> [Name] -> Exp
_sequenceExprA n xs = foldl (flip ($)) (ConE (tupleDataName n)) exps'
  where
    exps = map (AppE . VarE) ('(<$>) : repeat '(<*>)) :: [Exp -> Exp]
    exps' = zipWith (\f x y -> f y `AppE` VarE x) exps xs :: [Exp -> Exp]

_sequenceExprA_ :: [Name] -> Exp
_sequenceExprA_ = foldr ((AppE . AppE (VarE '(*>))) . VarE) (VarE 'pure `AppE` ConE '())

-- | Produce a function for sequencing a tuple of a given length of tuples with a given name.
sequenceClauseA ::
    -- | The given number /n/ for which a function is produced that sequences /n/-tuples.
    Int ->
    -- | The name for the function that is generated.
    Name ->
    -- | A declaration for a function that sequences the tuple.
    Dec
sequenceClauseA n = _clause [_tupleP' vn] (NormalB (_sequenceExprA n vn))
  where
    vn = take n _vNames

-- | Produce a function for sequencing a tuple of a given length of tuples and return the unit value with a given name.
sequenceClauseA_ ::
    -- | The given number /n/ for which a function is produced that sequences /n/-tuples.
    Int ->
    -- | The name for the function that is generated.
    Name ->
    -- | A declaration for a function that sequences the tuple and returns the unit value.
    Dec
sequenceClauseA_ n = _clause [_tupleP' vn] (NormalB (_sequenceExprA_ vn))
  where
    vn = take n _vNames

#if MIN_VERSION_template_haskell(2,16,0)
_appendClause :: ([Pat] -> Pat) -> ([Maybe Exp] -> Exp) -> Int -> Int -> Name -> Dec
#else
_appendClause :: ([Pat] -> Pat) -> ([Exp] -> Exp) -> Int -> Int -> Name -> Dec
#endif
_appendClause fp fe m n = _clause [_tupleP'' fp um, _tupleP'' fp vn] (_tupleB' fe (um ++ vn))
  where
    um = take m _uNames
    vn = take n _vNames

#if MIN_VERSION_template_haskell(2,16,0)
_addLClause :: ([Pat] -> Pat) -> ([Maybe Exp] -> Exp) -> Int -> Name -> Dec
#else
_addLClause :: ([Pat] -> Pat) -> ([Exp] -> Exp) -> Int -> Name -> Dec
#endif
_addLClause fp fe n = _clause [_patZZ, _tupleP'' fp vars] (_tupleB' fe (_nameZZ : vars))
  where
    vars = take n _vNames

#if MIN_VERSION_template_haskell(2,16,0)
_addRClause :: ([Pat] -> Pat) -> ([Maybe Exp] -> Exp) -> Int -> Name -> Dec
#else
_addRClause :: ([Pat] -> Pat) -> ([Exp] -> Exp) -> Int -> Name -> Dec
#endif
_addRClause fp fe n = _clause [_tupleP'' fp vars, _patZZ] (_tupleB' fe (vars ++> _nameZZ))
  where
    vars = take n _vNames

_foldLClause :: ([Pat] -> Pat) -> Int -> Name -> Dec
_foldLClause fp n = _clause [_patFF, _patZZ, _tupleP'' fp vars] (NormalB (foldl (\x₁ x₂ -> _expFF `AppE` x₁ `AppE` x₂) _expZZ (map VarE vars)))
  where
    vars = take n _vNames

_foldRClause :: ([Pat] -> Pat) -> Int -> Name -> Dec
_foldRClause fp n = _clause [_patFF, _patZZ, _tupleP'' fp vars] (NormalB (foldr ((\x₁ x₂ -> _expFF `AppE` x₁ `AppE` x₂) . VarE) _expZZ vars))
  where
    vars = take n _vNames

_foldMapClause :: ([Pat] -> Pat) -> Int -> Name -> Dec
_foldMapClause fp n = _clause [_patFF, _tupleP'' fp vars] (NormalB (foldr1 (\x₁ x₂ -> VarE '(<>) `AppE` x₁ `AppE` x₂) (map (AppE _expFF . VarE) vars)))
  where
    vars = take n _vNames

-- | Create a function declaration to append two boxed tuples together in a new boxed tuple. This only contains a declaration for the /body/ of the function, not a type signature.
boxedAppendClause ::
    -- | The number of items for the first boxed tuple parameter.
    Int ->
    -- | The number of items for the second boxed tuple parameter.
    Int ->
    -- | The name of the function to define.
    Name ->
    -- | A function declaration that only contains the body of the function.
    Dec
boxedAppendClause = _appendClause (TildeP . TupP) TupE

-- | Create a function declaration to append two unboxed tuples together in a new unboxed tuple. This only contains a declaration for the /body/ of the function, not a type signature.
unboxedAppendClause ::
    -- | The number of items for the first unboxed tuple parameter.
    Int ->
    -- | The number of items for the second unboxed tuple parameter.
    Int ->
    -- | The name of the function to define.
    Name ->
    -- | A function declaration that only contains the body of the function.
    Dec
unboxedAppendClause = _appendClause UnboxedTupP UnboxedTupE

-- | Create a function declaration to add an item to the left side of a boxed tuple in a new boxed tuple. This only contains a declaration for the /body/ of the function, not a type signature.
boxedAddLClause ::
    -- | The number of items of the boxed tuple to add an item to.
    Int ->
    -- | The name of the function to define.
    Name ->
    -- | A function declaration that only contains the body of the function.
    Dec
boxedAddLClause = _addLClause (TildeP . TupP) TupE

-- | Create a function declaration to add an item to the left side of an unboxed tuple in a new unboxed tuple. This only contains a declaration for the /body/ of the function, not a type signature.
unboxedAddLClause ::
    -- | The number of items of the unboxed tuple to add an item to.
    Int ->
    -- | The name of the function to define.
    Name ->
    -- | A function declaration that only contains the body of the function.
    Dec
unboxedAddLClause = _addLClause UnboxedTupP UnboxedTupE

-- | Create a function declaration to add an item to the right side of a boxed tuple in a new boxed tuple. This only contains a declaration for the /body/ of the function, not a type signature.
boxedAddRClause ::
    -- | The number of items of the boxed tuple to add an item to.
    Int ->
    -- | The name of the function to define.
    Name ->
    -- | A function declaration that only contains the body of the function.
    Dec
boxedAddRClause = _addRClause (TildeP . TupP) TupE

-- | Create a function declaration to add an item to the right side of an unboxed tuple in a new unboxed tuple. This only contains a declaration for the /body/ of the function, not a type signature.
unboxedAddRClause ::
    -- | The number of items of the unboxed tuple to add an item to.
    Int ->
    -- | The name of the function to define.
    Name ->
    -- | A function declaration that only contains the body of the function.
    Dec
unboxedAddRClause = _addRClause UnboxedTupP UnboxedTupE

-- | Create a function declaration to fold a boxed tuple left-to-right. This only contains a declaration for the /body/ of the function, not a type signature.
boxedFoldLClause ::
    -- | The number of items of the boxed tuple to fold.
    Int ->
    -- | The name of the function to define.
    Name ->
    -- | A function declaration that only contains the body of the function.
    Dec
boxedFoldLClause = _foldLClause (TildeP . TupP)

-- | Create a function declaration to fold a boxed tuple right-to-left. This only contains a declaration for the /body/ of the function, not a type signature.
boxedFoldRClause ::
    -- | The number of items of the boxed tuple to fold.
    Int ->
    -- | The name of the function to define.
    Name ->
    -- | A function declaration that only contains the body of the function.
    Dec
boxedFoldRClause = _foldRClause (TildeP . TupP)

-- | Create a function declaration to 'foldMap' a boxed tuple. This only contains a declaration for the /body/ of the function, not a type signature.
boxedFoldMapClause ::
    -- | The number of items of the boxed tuple to 'foldMap'.
    Int ->
    -- | The name of the function to define.
    Name ->
    -- | A function declaration that only contains the body of the function.
    Dec
boxedFoldMapClause = _foldMapClause (TildeP . TupP)

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
boxedTupleAppendFun ::
    -- | The name of the function to construct.
    Name ->
    -- | The types of the first boxed tuple, all types should be lifted types.
    [Type] ->
    -- | The types of the second boxed tuple, all types should be lifted types.
    [Type] ->
    -- | A list that contains two 'Dec' objects: one for the function signature declaration, and one for the function declaration.
    [Dec]
boxedTupleAppendFun nm l r =
    [ _signature nm (_tupType l) (_tupType r) (_tupType (l ++ r))
    , boxedAppendClause (length l) (length r) nm
    ]

-- | Create a function declaration with signature to append an unboxed tuple with the types of the first list with an unboxed tuple with the types of the second list. This will contain two 'Dec' items: one for the signature and one for the function declaration itself.
unboxedTupleAppendFun ::
    -- | The name of the function to construct.
    Name ->
    -- | The types of the first boxed tuple, all types can be lifted or unlifted types or type variables.
    [Type] ->
    -- | The types of the second boxed tuple, all types can be lifted or unlifted types or type variables.
    [Type] ->
    -- | A list that contains two 'Dec' objects: one for the function signature declaration, and one for the function declaration.
    [Dec]
unboxedTupleAppendFun nm l r =
    [ _signature nm (_utupType l) (_utupType r) (_utupType (l ++ r))
    , unboxedAppendClause (length l) (length r) nm
    ]

-- | Create a function declaration with signature to add an item with a given type to the left side of a boxed tuple with the types of the given list. This will contain two 'Dec' items: one for the signature and one for the function declaration itself.
boxedTupleAddLFun ::
    -- | The name of the function to construct.
    Name ->
    -- | The type of the item to add to the tuple, should be a lifted type.
    Type ->
    -- | The types of the boxed tuple, all items should be lifted types.
    [Type] ->
    -- | A list that contains two 'Dec' objects: one for the function signature declaration, and one for the function declaration.
    [Dec]
boxedTupleAddLFun nm t ts =
    [ _signature nm t (_tupType ts) (_tupType (t : ts))
    , boxedAddLClause (length ts) nm
    ]

-- | Create a function declaration with signature to add an item with a given type to the left side of an unboxed tuple with the types of the given list. This will contain two 'Dec' items: one for the signature and one for the function declaration itself.
unboxedTupleAddLFun ::
    -- | The name of the function to construct.
    Name ->
    -- | The type of the item to add to the tuple, this can be a lifted or unlifted type or a type variable.
    Type ->
    -- | The types of the boxed tuple, all types can be lifted or unlifted types or type variables.
    [Type] ->
    -- | A list that contains two 'Dec' objects: one for the function signature declaration, and one for the function declaration.
    [Dec]
unboxedTupleAddLFun nm t ts =
    [ _signature nm t (_utupType ts) (_utupType (t : ts))
    , unboxedAddLClause (length ts) nm
    ]

-- | Create a function declaration with signature to add an item with a given type to the right side of a boxed tuple with the types of the given list. This will contain two 'Dec' items: one for the signature and one for the function declaration itself.
boxedTupleAddRFun ::
    -- | The name of the function to construct.
    Name ->
    -- | The types of the boxed tuple, all types should be lifted types.
    [Type] ->
    -- | The type of the item to add to the tuple, should be a lifted type.
    Type ->
    -- | A list that contains two 'Dec' objects: one for the function signature declaration, and one for the function declaration.
    [Dec]
boxedTupleAddRFun nm ts t =
    [ _signature nm (_tupType ts) t (_tupType (ts ++> t))
    , boxedAddRClause (length ts) nm
    ]

-- | Create a function declaration with signature to add an item with a given type to the right side of an unboxed tuple with the types of the given list. This will contain two 'Dec' items: one for the signature and one for the function declaration itself.
unboxedTupleAddRFun ::
    -- | The name of the function to construct.
    Name ->
    -- | The types of the boxed tuple, all types can be lifted or unlifted types or type variables.
    [Type] ->
    -- | The type of the item to add to the tuple, this can be a lifted or unlifted type or a type variable.
    Type ->
    -- | A list that contains two 'Dec' objects: one for the function signature declaration, and one for the function declaration.
    [Dec]
unboxedTupleAddRFun nm ts t =
    [ _signature nm (_utupType ts) t (_utupType (ts ++> t))
    , unboxedAddRClause (length ts) nm
    ]

-- | Create a function declaration with signature to append a boxed tuple with the types of the first list with a boxed tuple with the types of the second list. This function can be used with template Haskell.
makeBoxedTupleAppendFun ::
    -- | The name of the function to construct.
    Name ->
    -- | The types of the first boxed tuple, all types should be lifted types.
    [Type] ->
    -- | The types of the second boxed tuple, all types should be lifted types.
    [Type] ->
    -- | A builder to construct the declaration of the signature and a body of the function to append the tuples.
    DecsQ
makeBoxedTupleAppendFun nm l = pure . boxedTupleAppendFun nm l

-- | Create a function declaration with signature to append an unboxed tuple with the types of the first list with an unboxed tuple with the types of the second list. This function can be used with template Haskell.
makeUnboxedTupleAppendFun ::
    -- | The name of the function to construct.
    Name ->
    -- | The types of the first boxed tuple, all types can be lifted or unlifted types or type variables.
    [Type] ->
    -- | The types of the second boxed tuple, all types can be lifted or unlifted types or type variables.
    [Type] ->
    -- | A builder to construct the declaration of the signature and a body of the function to append the tuples.
    DecsQ
makeUnboxedTupleAppendFun nm l = pure . unboxedTupleAppendFun nm l

-- | Create a function declaration with signature to add an item with a given type to the left side of a boxed tuple with the types of the given list. This function can be used with template Haskell.
makeBoxedTupleAddLFun ::
    -- | The name of the function to construct.
    Name ->
    -- | The type of the item to add to the tuple, should be a lifted type.
    Type ->
    -- | The types of the boxed tuple, all types should be lifted types.
    [Type] ->
    -- | A builder to construct the declaration of the signature and a body of the function to add an element at the left side of a tuple.
    DecsQ
makeBoxedTupleAddLFun nm t = pure . boxedTupleAddLFun nm t

-- | Create a function declaration with signature to add an item with a given type to the left side of an unboxed tuple with the types of the given list. This function can be used with template Haskell.
makeUnboxedTupleAddLFun ::
    -- | The name of the function to construct.
    Name ->
    -- | The type of the item to add to the tuple, this can be a lifted or unlifted type or a type variable.
    Type ->
    -- | The types of the boxed tuple, all types can be lifted or unlifted types or type variables.
    [Type] ->
    -- | A builder to construct the declaration of the signature and a body of the function to add an element at the left side of a tuple.
    DecsQ
makeUnboxedTupleAddLFun nm t = pure . unboxedTupleAddLFun nm t

-- | Create a function declaration with signature to add an item with a given type to the right side of a boxed tuple with the types of the given list. This function can be used with template Haskell.
makeBoxedTupleAddRFun ::
    -- | The name of the function to construct.
    Name ->
    -- | The types of the boxed tuple, all types should be lifted types.
    [Type] ->
    -- | The type of the item to add to the tuple, should be a lifted type.
    Type ->
    -- | A builder to construct the declaration of the signature and a body of the function to add an element at the right side of a tuple.
    DecsQ
makeBoxedTupleAddRFun nm ts = pure . boxedTupleAddRFun nm ts

-- | Create a function declaration with signature to add an item with a given type to the right side of an unboxed tuple with the types of the given list. This function can be used with template Haskell.
makeUnboxedTupleAddRFun ::
    -- | The name of the function to construct.
    Name ->
    -- | The types of the boxed tuple, all types can be lifted or unlifted types or type variables.
    [Type] ->
    -- | The type of the item to add to the tuple, this can be a lifted or unlifted type or a type variable.
    Type ->
    -- | A builder to construct the declaration of the signature and a body of the function to add an element at the right side of a tuple.
    DecsQ
makeUnboxedTupleAddRFun nm ts = pure . unboxedTupleAddRFun nm ts

_simpleInstance'' :: Cxt -> Name -> Type -> Type -> Type -> [Dec] -> Dec
_simpleInstance'' cxt tc tca tcb tcc = InstanceD Nothing cxt (ConT tc `AppT` tca `AppT` tcb `AppT` tcc)

_simpleInstance' :: Name -> Type -> Type -> Type -> [Dec] -> Dec
_simpleInstance' = _simpleInstance'' []

_simpleInstance :: Name -> Name -> Type -> Type -> Type -> (Name -> Dec) -> Dec
_simpleInstance tc f tca tcb tcc d = _simpleInstance' tc tca tcb tcc [d f]

_simpleInstanceLift :: Type -> Type -> Type -> (Name -> Dec) -> Dec
_simpleInstanceLift = _simpleInstance ''TupleAppend '(+++)

_simpleInstanceAppend :: Type -> Type -> Type -> (Name -> Dec) -> Dec
_simpleInstanceAppend = _simpleInstance ''TupleAppend '(+++)

_simpleInstanceAddL :: Type -> Type -> Type -> (Name -> Dec) -> Dec
_simpleInstanceAddL = _simpleInstance ''TupleAddL '(<++)

_simpleInstanceAddR :: Type -> Type -> Type -> (Name -> Dec) -> Dec
_simpleInstanceAddR = _simpleInstance ''TupleAddR '(++>)

_simpleInstanceFold :: Type -> Type -> [Dec] -> Dec
_simpleInstanceFold 𝐯 vₖ = InstanceD Nothing [] (ConT ''FoldTuple `AppT` 𝐯 `AppT` vₖ)

_simpleSequenceInstance :: Type -> Type -> [Dec] -> Dec
_simpleSequenceInstance = _simpleInstance'' [ConT ''Prelude.Applicative `AppT` _varFF] ''SequenceTuple _varFF

-- | Define a typeclass instance for 'SequenceTuple' where it sequences a tuple of length /n/.
sequenceTuple ::
    -- | The given length /n/ for the tuple to be sequenced.
    Int ->
    -- | The instance declaraion for that tuple.
    Dec
sequenceTuple n = _simpleSequenceInstance (_tupleVar'' n _varFF _vNames) (_tupleVar' n _vNames) [sequenceClauseA n 'sequenceTupleA, sequenceClauseA_ n 'sequenceTupleA_]

-- | Define a typeclass instance for 'TupleAppend' where it appends tuples with /m/ and /n/ items with /m/ and /n/ the parameters of the function.
tupleAppend ::
    -- | The length /m/ of the first tuple.
    Int ->
    -- | The length /n/ of the second tuple.
    Int ->
    -- | An instance of the 'TupleAppend' typeclass that appends tuples with lengths /m/ and /n/ to a tuple with length /m+n/.
    Dec
tupleAppend m n = _simpleInstanceAppend (_tupleVar' m _uNames) (_tupleVar' n _vNames) (_tupleVar' (m + n) (take m _uNames ++ _vNames)) (boxedAppendClause m n)

-- | Define typeclass instances for 'TupleAppend' that will append any tuple of at least size two with any tuple of at least size two such that the sum is the given number.
tupleAppendFor ::
    -- | The given number /l/ for which typeclass instances of 'TupleAppend' will be made with /m/ and /n/ such that /l=m+n/.
    Int ->
    -- | A list of typeclass instances for the 'TupleAppend' typeclass.
    [Dec]
tupleAppendFor l = [tupleAppend m n | m <- _tupleRange l, let n = l - m, _tupleCheck n]

-- | Define a typeclass instance for the 'SequenceTuple' typeclass that will sequence over a tuple for the given length.
sequenceTupleFor ::
    -- | The given number /n/ that specifies the *arity* of the tuple for which to construct an instance. Will return an empty list if the number is invalid.
    Int ->
    -- | A list of typeclass instances for the 'SequenceTuple' typeclass.
    [Dec]
sequenceTupleFor n = [sequenceTuple n | _tupleCheck n]

-- | Define a typeclass instance for the 'FoldTuple' typeclass that will fold over a tuple of given length.
foldTupleFor ::
    -- | The given number /n/ that specifies the *arity* of the tuple for which to construct an instance. Will return an empty list if the number is invalid.
    Int ->
    -- | A list of typeclass instances for the 'FoldTuple' typeclass.
    [Dec]
foldTupleFor n = [foldTuple n | _tupleCheck n]

-- | Define a typeclass instance for 'TupleAddL' for a tuple with /n/ elements and an item to construct a tuple with /n+1/ elements where the item is added at the left side.
tupleAddL ::
    -- | The given length /n/ of the tuples to prepend an element.
    Int ->
    -- | A type instance declaration for an instance of the 'TupleAddL' typeclass for an /n/-tuple.
    Dec
tupleAddL n = _simpleInstanceAddL _varZZ (_tupleVar' n _vNames) (_tupleVar' (n + 1) (_nameZZ : _vNames)) (boxedAddLClause n)

-- | Define a typeclass instance for 'TupleAddR' for a tuple with /n/ elements and an item to construct a tuple with /n+1/ elements where the item is added at the right side.
tupleAddR ::
    -- | The given length /n/ of the tuples to append with an element.
    Int ->
    -- | A type instance declaration for an instance of the 'TupleAddR' typeclass for an /n/-tuple.
    Dec
tupleAddR n = _simpleInstanceAddR (_tupleVar' n _vNames) _varZZ (_tupleVar' (n + 1) (take n _vNames ++> _nameZZ)) (boxedAddRClause n)

-- | Define a typeclass instance for 'FoldTuple' for a tuple with /n/ elements that is folded with an arbitrary "fold" function.
foldTuple ::
    -- | The given length /n/ of the tuples to fold.
    Int ->
    -- | A type instance declaration for an instance of the 'FoldTuple' typeclass for an /n/-tuple.
    Dec
foldTuple n = _simpleInstanceFold _varZZ (_tupleVar' n (repeat _nameZZ)) [boxedFoldLClause n 'foldlTuple, boxedFoldRClause n 'foldrTuple, boxedFoldMapClause n 'foldMapTuple]

-- | Define typeclass instances for 'TupleAddL' and 'TupleAddR' for a tuple with /n/ elements and an item to construct a tuple with /n+1/ elements where the item is added at the left or the right side.
tupleAdd ::
    -- | The given length /n/ of the tuples to prepend and append with an element.
    Int ->
    -- | A list of two type instance declarations that contains typeclass instances for 'TupleAddL' and 'TupleAddR'.
    [Dec]
tupleAdd n
    | _tupleCheck n && _tupleCheck (n + 1) = [tupleAddL n, tupleAddR n]
    | otherwise = []

_errorQuasiQuoter :: a -> Q b
_errorQuasiQuoter = const (fail "The quasi quoter can only be used to define declarations")

-- | A 'QuasiQuoter' that constructs instances for 'TupleAddL' and 'TupleAddR' for tuples up to length /n/ where /n/ is read as text input for the quasi quoter.
defineTupleAddUpto ::
    -- | A 'QuasiQuoter' that will construct typeclass instance declarations.
    QuasiQuoter
defineTupleAddUpto = QuasiQuoter _errorQuasiQuoter _errorQuasiQuoter _errorQuasiQuoter (_defineTupleAddUpTo . read)

_defineTupleAddUpTo :: Int -> DecsQ
_defineTupleAddUpTo n = pure (map tupleAddL ns ++ map tupleAddR ns)
  where
    ns = reverse (filter (_tupleCheck . succ) (_tupleRange n))

-- | A 'QuasiQuoter' that constructs instances for 'TupleAppend' for tuples up to length /n/ where /n/ is read as text input for the quasi quoter. For a single /n/ it thus will construct /n+1/ instances for each tuple length.
defineTupleAppendUpto ::
    -- | A 'QuasiQuoter' that will construct typeclass instance declarations.
    QuasiQuoter
defineTupleAppendUpto = QuasiQuoter _errorQuasiQuoter _errorQuasiQuoter _errorQuasiQuoter (pure . (tupleAppendFor <=< enumFromTo 0 . read))

-- | A 'QuasiQuoter' that constructs instances for the 'SequenceTuple' typeclass for tuples up to a length /n/ where /n/ is read as text input for the quasi quoter. For a single /n/ it will thus construct /n/ instances in total.
defineSequenceTupleUpTo ::
    -- | A 'QuasiQuoter' that will construct typeclass instance declarations.
    QuasiQuoter
defineSequenceTupleUpTo = QuasiQuoter _errorQuasiQuoter _errorQuasiQuoter _errorQuasiQuoter (pure . (sequenceTupleFor <=< enumFromTo 1 . read))

-- | A 'QuasiQuoter' that constructs instances for the 'FoldTuple' typeclass for tuples up to a length /n/ where /n/ is read as text input for the quasi quoter. For a single /n/ it will thus construct /n+1/ instances in total.
defineFoldTupleUpTo ::
    -- | A 'QuasiQuoter' that will construct typeclass instance declarations.
    QuasiQuoter
defineFoldTupleUpTo = QuasiQuoter _errorQuasiQuoter _errorQuasiQuoter _errorQuasiQuoter (pure . (foldTupleFor <=< enumFromTo 1 . read))

-- | A 'QuasiQuoter' that constructs function declarations with the name @unboxedAppend_i_j@ with /i/ and /j/ the number of items of the unboxed tuples. The items sum up to at most /n/ where /n/ is read as text input for the quasi quoter. For a single /n/ it thus will construct /n+1/ instances for each tuple length.
defineUnboxedTupleAppendFunctionsUpto ::
    -- | A 'QuasiQuoter' that will construct function definitions.
    QuasiQuoter
defineUnboxedTupleAppendFunctionsUpto = QuasiQuoter _errorQuasiQuoter _errorQuasiQuoter _errorQuasiQuoter (_unboxedTupleConcats . read)

_unboxedTupleConcats :: Int -> DecsQ
_unboxedTupleConcats r = pure [u | m <- [r - 2, r - 3 .. 2], n <- [r - m - 2, r - m - 3 .. 2], u <- unboxedTupleAppendFun (mkName ("unboxedAppend_" ++ show m ++ "_" ++ show n)) (map VarT (take m _uNames)) (map VarT (take n _vNames))]
