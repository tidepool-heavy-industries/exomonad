{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
-- | Types for compile-time template type checking.
module Text.Ginger.TH.Types
  ( -- * Schema representation
    Schema(..)
    -- * Access paths extracted from templates
  , AccessPath(..)
  , PathSegment(..)
    -- * Narrowing context
  , NarrowedPath(..)
  , toNarrowedPath
  , isNarrowedBy
    -- * Validation results
  , ValidationError(..)
    -- * Typed template wrapper
  , TypedTemplate(..)
    -- * Template dependencies
  , TemplateDependency(..)
  , DepRelation(..)
  , DepLocation(..)
  , flattenDeps
  , absolutePaths
  , relativePaths
    -- * Context type metadata
  , TemplateContextInfo(..)
  ) where

import Data.Text (Text)
import Data.Data (Data, Typeable)
import Data.Set (Set)
import qualified Data.Set as Set
import Language.Haskell.TH.Syntax (Lift)
import Text.Parsec.Pos (SourcePos)

import Text.Ginger.AST (Template)

-- | Schema representation for compile-time type analysis.
-- Represents the structure of Haskell types that can be used as template contexts.
data Schema
  = RecordSchema [(Text, Schema)]
    -- ^ Product type (single-constructor record) with named fields
  | SumSchema [(Text, [(Text, Schema)])]
    -- ^ Sum type with multiple constructors. Each entry is
    -- @(constructorName, fields)@.
    --
    -- Valid accesses:
    --
    -- * @.constructorName@ - always valid, returns the constructor's inner value
    --   if active, else undefined (for use with @is defined@ guards)
    -- * @.fieldName@ - valid only if field exists in ALL constructors
    --   (unless guarded by @is defined@)
  | ListSchema Schema
    -- ^ List or Vector type, element schema
  | ScalarSchema
    -- ^ Leaf type (Text, Int, Bool, Scientific, etc.)
  | RecursiveRef Text
    -- ^ Reference to a recursive type. Used to handle recursive data structures.
    -- The Text is the type name for resolution during validation.
  | OpaqueSchema Text
    -- ^ Type that schema generation doesn't understand (e.g., non-record sum types,
    -- Either, tuples). The Text is a description of why it's opaque.
    -- This allows types to exist in the tree without causing compile errors,
    -- as long as templates don't actually access through them.
  deriving (Show, Eq, Data, Typeable, Lift)

-- | A path segment in a variable access chain.
data PathSegment
  = StaticKey Text
    -- ^ Static field access: @.fieldName@ or @[\"literal\"]@
  | DynamicKey
    -- ^ Dynamic key access: @[expr]@ where expr is not a literal.
    -- This is always a compile error in v1.
  deriving (Show, Eq, Ord, Data, Typeable)

-- | A variable access path extracted from a template.
-- Represents expressions like @{{ user.profile.name }}@.
data AccessPath = AccessPath
  { apRoot :: Text
    -- ^ Top-level variable name (e.g., \"user\")
  , apPath :: [PathSegment]
    -- ^ Chain of accesses (e.g., [StaticKey \"profile\", StaticKey \"name\"])
  , apSourcePos :: SourcePos
    -- ^ Source position for error messages
  , apNarrowed :: Set NarrowedPath
    -- ^ Paths that have been narrowed (guarded by @is defined@) at this point
  , apIsExistenceCheck :: Bool
    -- ^ True if this path is inside an @is defined@ check.
    -- For existence checks, we validate only the prefix (all but last segment)
    -- because the final segment is what we're checking for existence.
  } deriving (Show, Eq, Data, Typeable)

-- | A path that has been narrowed by an @is defined@ check.
-- Used to track which accesses are guarded and can safely access
-- sum type fields that only exist in some constructors.
data NarrowedPath = NarrowedPath
  { npRoot :: Text
    -- ^ Top-level variable name
  , npPath :: [PathSegment]
    -- ^ Path segments (without source position)
  } deriving (Show, Eq, Ord, Data, Typeable)

-- | Convert an AccessPath to a NarrowedPath (drops source position and other context).
toNarrowedPath :: AccessPath -> NarrowedPath
toNarrowedPath (AccessPath root path _ _ _) = NarrowedPath root path

-- | Check if an AccessPath is narrowed by one of the paths in the narrowing context.
-- An access is considered narrowed if:
--   1. Its exact path exists in the narrowed set, OR
--   2. Any prefix of its path exists in the narrowed set
--
-- For example, if @user.profile@ is narrowed (guarded by @is defined@),
-- then @user.profile.name@ is also considered narrowed because if
-- @user.profile@ exists, accessing @.name@ on it is safe.
isNarrowedBy :: AccessPath -> Bool
isNarrowedBy (AccessPath root path _ narrowed _) =
  any (isNarrowingPrefix root path) (Set.toList narrowed)

-- | Check if a NarrowedPath is a prefix of (or equal to) the given access.
-- A narrowed path is a prefix if:
--   1. The roots match, AND
--   2. The narrowed path segments are a prefix of the access path segments
isNarrowingPrefix :: Text -> [PathSegment] -> NarrowedPath -> Bool
isNarrowingPrefix accessRoot accessPath (NarrowedPath narrowedRoot narrowedPath) =
  accessRoot == narrowedRoot && narrowedPath `isPrefixOfPath` accessPath

-- | Check if the first list is a prefix of the second.
isPrefixOfPath :: Eq a => [a] -> [a] -> Bool
isPrefixOfPath [] _ = True
isPrefixOfPath _ [] = False
isPrefixOfPath (x:xs) (y:ys) = x == y && isPrefixOfPath xs ys

-- | Validation error for a single access path.
data ValidationError
  = FieldNotFound AccessPath Text
    -- ^ Field doesn't exist. Second arg is the missing field name.
  | FieldNotInAllConstructors AccessPath Text
    -- ^ For sum types: field exists in some but not all constructors.
  | DynamicAccessNotAllowed AccessPath
    -- ^ Dynamic key access @[expr]@ is not allowed.
  | AccessOnScalar AccessPath
    -- ^ Tried to access a field on a scalar type.
  | UnknownType AccessPath Text
    -- ^ Encountered a type we can't analyze.
  | AccessOnOpaqueType AccessPath Text
    -- ^ Tried to access a field on a type that schema generation couldn't understand.
    -- The Text describes why the type is opaque (e.g., "non-record sum type").
  deriving (Show, Eq, Data, Typeable)

-- | How a template dependency is related to its parent.
data DepRelation
  = DepIncluded   -- ^ @{% include \"file\" %}@
  | DepExtended   -- ^ @{% extends \"file\" %}@
  deriving (Show, Eq, Data, Typeable, Lift)

-- | Location of an include/extends directive in source.
-- Used for error messages and documentation.
data DepLocation = DepLocation
  { depLocFile :: FilePath
    -- ^ File containing the directive
  , depLocLine :: Int
    -- ^ Line number (1-based)
  , depLocColumn :: Int
    -- ^ Column number (1-based)
  } deriving (Show, Eq, Data, Typeable, Lift)

-- | A template dependency tree node.
-- The root node represents the main template file; children represent
-- included/extended templates, recursively.
data TemplateDependency = TemplateDependency
  { depAbsolutePath :: FilePath
    -- ^ Full filesystem path (canonicalized)
  , depRelativePath :: FilePath
    -- ^ Path as specified in the template or include directive
  , depRelation :: Maybe DepRelation
    -- ^ How this file is included (Nothing for root template)
  , depIncludeLocation :: Maybe DepLocation
    -- ^ Where the include/extends directive appears (Nothing for root)
  , depChildren :: [TemplateDependency]
    -- ^ Child dependencies (includes and extends from this template)
  } deriving (Show, Eq, Data, Typeable, Lift)

-- | Metadata about the context type used by a template.
-- Useful for documentation generation.
data TemplateContextInfo = TemplateContextInfo
  { tciTypeName :: String
    -- ^ Simple type name: @\"MyContext\"@
  , tciModuleName :: Maybe String
    -- ^ Module containing the type: @Just \"MyModule.Types\"@ or @Nothing@ if local
  , tciFullyQualified :: String
    -- ^ Fully qualified name: @\"MyModule.Types.MyContext\"@
  } deriving (Show, Eq, Data, Typeable, Lift)

-- | A template that has been type-checked against a specific context type.
-- The type parameter @a@ is the required context type.
-- The type parameter @p@ is the source position type (usually 'SourcePos').
data TypedTemplate a p = TypedTemplate
  { unTypedTemplate :: Template p
    -- ^ The parsed template AST
  , templateDependencyTree :: TemplateDependency
    -- ^ Dependency tree (root node is the main template)
  , templateContextInfo :: TemplateContextInfo
    -- ^ Metadata about the context type (for documentation)
  , templateAccessedFields :: [String]
    -- ^ Fields accessed in the template: @[\"user.name\", \"user.email\"]@
  } deriving (Show)

-- | Flatten a dependency tree to a list (pre-order traversal).
flattenDeps :: TemplateDependency -> [TemplateDependency]
flattenDeps dep = dep : concatMap flattenDeps (depChildren dep)

-- | Get all absolute file paths this template depends on.
-- Useful for file watching and hot-reload systems.
absolutePaths :: TypedTemplate a p -> [FilePath]
absolutePaths = map depAbsolutePath . flattenDeps . templateDependencyTree

-- | Get all relative file paths this template depends on.
-- These are the paths as specified in the template source.
relativePaths :: TypedTemplate a p -> [FilePath]
relativePaths = map depRelativePath . flattenDeps . templateDependencyTree
