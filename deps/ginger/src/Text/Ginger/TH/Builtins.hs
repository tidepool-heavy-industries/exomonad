{-# LANGUAGE OverloadedStrings #-}
-- | Built-in names that are always available in ginger templates.
-- These should not be required as fields in the context type.
module Text.Ginger.TH.Builtins
  ( builtinNames
  , isBuiltin
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

-- | Names that are always available in defaultScope.
-- Extracted from Text.Ginger.Run.defaultScope.
builtinNames :: Set Text
builtinNames = Set.fromList
  [ -- Functions
    "raw"
  , "abs"
  , "any"
  , "all"
  , "apply"
  , "capitalize"
  , "ceil"
  , "center"
  , "compose"
  , "concat"
  , "contains"
  , "d"
  , "date"
  , "dateformat"
  , "default"
  , "dictsort"
  , "difference"
  , "divisibleby"
  , "e"
  , "eq"
  , "equals"
  , "equalto"
  , "escape"
  , "eval"
  , "even"
  , "filesizeformat"
  , "filter"
  , "floor"
  , "format"
  , "ge"
  , "gt"
  , "greater"
  , "greaterthan"
  , "greaterEquals"
  , "int"
  , "int_ratio"
  , "is_lt"
  , "iterable"
  , "join"
  , "json"
  , "length"
  , "le"
  , "less"
  , "lessthan"
  , "lessEquals"
  , "lt"
  , "map"
  , "modulo"
  , "not"
  , "ne"
  , "nequals"
  , "num"
  , "odd"
  , "partial"
  , "printf"
  , "product"
  , "ratio"
  , "replace"
  , "reverse"
  , "round"
  , "show"
  , "slice"
  , "sort"
  , "split"
  , "str"
  , "sum"
  , "truncate"
  , "urlencode"
  , "upper"
  , "lower"
  , "throw"
  , "zip"
  , "zipwith"
    -- Jinja2 compatibility filters
  , "first"
  , "last"
  , "max"
  , "min"
  , "title"
  , "trim"
    -- Tests/predicates
  , "in"
  , "escaped"
  , "regex"
    -- Type-checking predicates (is_ prefix for "x is foo" syntax)
  , "is_mapping"
  , "is_none"
  , "is_number"
  , "is_sequence"
  , "is_string"
  , "is_callable"
    -- Case predicates (is_ prefix for "x is foo" syntax)
  , "is_lower"
  , "is_upper"
    -- Boolean/null literals (parsed as VarE in some contexts)
  , "true"
  , "True"
  , "false"
  , "False"
  , "null"
  , "none"
  , "None"
  ]

-- | Check if a name is a built-in (shouldn't require a context field).
isBuiltin :: Text -> Bool
isBuiltin = (`Set.member` builtinNames)
