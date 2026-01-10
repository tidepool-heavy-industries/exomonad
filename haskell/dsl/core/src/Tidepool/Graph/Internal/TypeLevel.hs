{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Shared type-level utilities for graph validation and manipulation.
--
-- This module consolidates type families that were previously duplicated across:
-- - Tidepool.Graph.Generic
-- - Tidepool.Graph.Validate.RecordStructure
-- - Tidepool.Graph.Validate.ForkBarrier
-- - Tidepool.Graph.Edges
-- - Tidepool.Graph.Execute
--
-- These utilities provide:
-- - Boolean logic (If, And, Or, Not)
-- - List operations (Append, Elem, Filter)
-- - Symbol operations (equality, membership)
-- - Type-level introspection (FieldNames, FieldsWithNames)
--
-- All type families are closed and should be imported qualified or with explicit lists.
module Tidepool.Graph.Internal.TypeLevel
  ( -- * Boolean Logic
    If
  , And
  , Or
  , Not

    -- * List Operations
  , Append
  , Elem
  , ElemSymbol
  , ElemType
  , AllIn
  , AnyElemSymbol
  , FilterNotInSymbols

    -- * Symbol Operations
  , type (==)
  , AppendSymbols

    -- * Field Introspection
  , FieldNames
  , FieldsWithNames
  , FieldNamesFromPairs

    -- * Maybe Operations
  , OrMaybe
  , IfMaybe
  , AppendMaybeType

    -- * Specialized Operations
  , AppendQuads
  ) where

import Data.Kind (Constraint, Type)
import GHC.Generics
import GHC.TypeLits (Symbol)

-- ════════════════════════════════════════════════════════════════════════════
-- BOOLEAN LOGIC
-- ════════════════════════════════════════════════════════════════════════════

-- | Type-level if-then-else.
type If :: Bool -> k -> k -> k
type family If cond t f where
  If 'True  t _ = t
  If 'False _ f = f

-- | Type-level boolean AND.
type And :: Bool -> Bool -> Bool
type family And a b where
  And 'True 'True = 'True
  And _     _     = 'False

-- | Type-level boolean OR.
type Or :: Bool -> Bool -> Bool
type family Or a b where
  Or 'False 'False = 'False
  Or _      _      = 'True

-- | Type-level boolean NOT.
type Not :: Bool -> Bool
type family Not b where
  Not 'True  = 'False
  Not 'False = 'True

-- ════════════════════════════════════════════════════════════════════════════
-- LIST OPERATIONS
-- ════════════════════════════════════════════════════════════════════════════

-- | Append two type-level lists.
type Append :: [k] -> [k] -> [k]
type family Append xs ys where
  Append '[]       ys = ys
  Append (x ': xs) ys = x ': Append xs ys

-- | Check if an element is in a list (polymorphic).
type Elem :: k -> [k] -> Bool
type family Elem x xs where
  Elem _ '[]       = 'False
  Elem x (x ': _)  = 'True
  Elem x (_ ': xs) = Elem x xs

-- | Check if a Symbol is in a list of Symbols.
type ElemSymbol :: Symbol -> [Symbol] -> Bool
type family ElemSymbol x xs where
  ElemSymbol _ '[]       = 'False
  ElemSymbol x (x ': _)  = 'True
  ElemSymbol x (_ ': xs) = ElemSymbol x xs

-- | Check if a Type is in a list of Types.
type ElemType :: Type -> [Type] -> Bool
type family ElemType x xs where
  ElemType _ '[]       = 'False
  ElemType x (x ': _)  = 'True
  ElemType x (_ ': xs) = ElemType x xs

-- | Check if all types in first list are in second list.
type AllIn :: [Type] -> [Type] -> Bool
type family AllIn xs ys where
  AllIn '[]       _  = 'True
  AllIn (x ': xs) ys = And (ElemType x ys) (AllIn xs ys)

-- | Check if any symbol in first list is in second list.
type AnyElemSymbol :: [Symbol] -> [Symbol] -> Bool
type family AnyElemSymbol xs ys where
  AnyElemSymbol '[]       _  = 'False
  AnyElemSymbol (x ': xs) ys = If (ElemSymbol x ys) 'True (AnyElemSymbol xs ys)

-- | Filter symbols not in the given list.
type FilterNotInSymbols :: [Symbol] -> [Symbol] -> [Symbol]
type family FilterNotInSymbols xs ys where
  FilterNotInSymbols '[]       _  = '[]
  FilterNotInSymbols (x ': xs) ys =
    If (ElemSymbol x ys)
       (FilterNotInSymbols xs ys)
       (x ': FilterNotInSymbols xs ys)

-- ════════════════════════════════════════════════════════════════════════════
-- SYMBOL OPERATIONS
-- ════════════════════════════════════════════════════════════════════════════

-- | Symbol equality.
type (==) :: Symbol -> Symbol -> Bool
type family a == b where
  a == a = 'True
  _ == _ = 'False

-- | Append symbol lists.
type AppendSymbols :: [Symbol] -> [Symbol] -> [Symbol]
type family AppendSymbols xs ys where
  AppendSymbols '[]       ys = ys
  AppendSymbols (x ': xs) ys = x ': AppendSymbols xs ys

-- ════════════════════════════════════════════════════════════════════════════
-- FIELD INTROSPECTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Extract field names from a Generic representation.
type FieldNames :: (Type -> Type) -> [Symbol]
type family FieldNames rep where
  FieldNames (M1 D _ inner) = FieldNames inner
  FieldNames (M1 C _ inner) = FieldNames inner
  FieldNames (l :*: r) = Append (FieldNames l) (FieldNames r)
  FieldNames (M1 S ('MetaSel ('Just name) _ _ _) _) = '[name]
  FieldNames _ = '[]

-- | Extract field names and types from a Generic representation.
type FieldsWithNames :: (Type -> Type) -> [(Symbol, Type)]
type family FieldsWithNames rep where
  FieldsWithNames (M1 D _ inner) = FieldsWithNames inner
  FieldsWithNames (M1 C _ inner) = FieldsWithNames inner
  FieldsWithNames (l :*: r) = Append (FieldsWithNames l) (FieldsWithNames r)
  FieldsWithNames (M1 S ('MetaSel ('Just name) _ _ _) (K1 _ fieldType)) =
    '[ '(name, fieldType) ]
  FieldsWithNames _ = '[]

-- | Extract just names from a list of (name, type) pairs.
type FieldNamesFromPairs :: [(Symbol, Type)] -> [Symbol]
type family FieldNamesFromPairs pairs where
  FieldNamesFromPairs '[] = '[]
  FieldNamesFromPairs ( '(name, _) ': rest) = name ': FieldNamesFromPairs rest

-- ════════════════════════════════════════════════════════════════════════════
-- MAYBE OPERATIONS
-- ════════════════════════════════════════════════════════════════════════════

-- | First Just value, or second if first is Nothing.
type OrMaybe :: Maybe k -> Maybe k -> Maybe k
type family OrMaybe m1 m2 where
  OrMaybe ('Just x) _  = 'Just x
  OrMaybe 'Nothing  m2 = m2

-- | Conditional Maybe selection.
type IfMaybe :: Bool -> Maybe k -> Maybe k -> Maybe k
type family IfMaybe cond t f where
  IfMaybe 'True  t _ = t
  IfMaybe 'False _ f = f

-- | Append Maybe type to list.
type AppendMaybeType :: Maybe Type -> [Type] -> [Type]
type family AppendMaybeType m xs where
  AppendMaybeType 'Nothing  xs = xs
  AppendMaybeType ('Just x) xs = x ': xs

-- ════════════════════════════════════════════════════════════════════════════
-- SPECIALIZED OPERATIONS
-- ════════════════════════════════════════════════════════════════════════════

-- | Append 4-tuple lists (for dead goto tracking in validation).
type AppendQuads :: [(Symbol, Symbol, Type, Maybe Type)] -> [(Symbol, Symbol, Type, Maybe Type)] -> [(Symbol, Symbol, Type, Maybe Type)]
type family AppendQuads xs ys where
  AppendQuads '[]       ys = ys
  AppendQuads (x ': xs) ys = x ': AppendQuads xs ys
