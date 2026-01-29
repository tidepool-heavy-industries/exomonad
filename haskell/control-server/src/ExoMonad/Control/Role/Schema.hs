{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Schema mode for Role Tools.
--
-- This module defines the 'AsSchema' mode and the generic machinery to
-- reify a tool record into a list of 'MCPToolInfo'.
module ExoMonad.Control.Role.Schema
  ( -- * Schema Mode
    AsSchema
  , SchemaBuilder(..)
  
    -- * Reification
  , reifyTools
  , GReifyTools(..)
  , camelToSnake
    -- * Construction
  , toolSchema
  , hookSchema
  , ConstructSchema(..)
  ) where

import Data.Aeson (Value(..))
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.Char (isUpper, isLower, toLower)
import Data.Maybe (fromMaybe)

import ExoMonad.Graph.Generic (GraphMode(..), type (:-), type (:@))
import ExoMonad.Graph.Types (Tool, Hook, Description)
import ExoMonad.Graph.MCPReify (MCPToolInfo(..))
import ExoMonad.Schema (HasJSONSchema(..), schemaToValue, jsonSchema)

-- ════════════════════════════════════════════════════════════════════════════
-- SCHEMA MODE
-- ════════════════════════════════════════════════════════════════════════════

-- | Schema mode: fields contain schema building blocks.
data AsSchema

-- | Value held in fields during schema construction.
data SchemaBuilder = SchemaBuilder
  { sbDescription :: Maybe Text
  , sbInputSchema :: Value
  }

instance GraphMode AsSchema where
  type AsSchema :- nodeDef = SchemaNode nodeDef

-- | Wrapper to preserve type info for inference.
newtype SchemaNode nodeDef = SchemaNode { unSchemaNode :: SchemaBuilder }

-- ════════════════════════════════════════════════════════════════════════════
-- REIFICATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Reify a tool record to a list of MCPToolInfo.
--
-- Uses the record field names as tool names (converted to snake_case).
reifyTools :: forall a. (Generic a, GReifyTools (Rep a)) => a -> [MCPToolInfo]
reifyTools record = gReifyTools (from record)

-- | Class for generic reification.
class GReifyTools f where
  gReifyTools :: f p -> [MCPToolInfo]

-- Unwrap metadata
instance GReifyTools f => GReifyTools (M1 D c f) where
  gReifyTools (M1 x) = gReifyTools x

instance GReifyTools f => GReifyTools (M1 C c f) where
  gReifyTools (M1 x) = gReifyTools x

-- Product: concatenate lists
instance (GReifyTools l, GReifyTools r) => GReifyTools (l :*: r) where
  gReifyTools (l :*: r) = gReifyTools l ++ gReifyTools r

-- Named Field: this is a tool or a sub-record
instance (KnownSymbol name, ReifyField field) => GReifyTools (M1 S ('MetaSel ('Just name) su ss ds) (K1 i field)) where
  gReifyTools (M1 (K1 x)) = reifyField (T.pack $ symbolVal (Proxy @name)) x

-- | Class for reifying a field value (Leaf or Sub-record).
class ReifyField a where
  reifyField :: Text -> a -> [MCPToolInfo]

-- | Leaf: SchemaNode (Tool)
instance {-# OVERLAPPING #-} ReifyField (SchemaNode nodeDef) where
  reifyField name (SchemaNode builder) =
    [ MCPToolInfo
        { mtdName = camelToSnake name
        , mtdDescription = fromMaybe "" builder.sbDescription
        , mtdInputSchema = builder.sbInputSchema
        , mtdEntryName = camelToSnake name -- Same as name for now
        , mtdRoles = [] -- To be filled by role context if needed
        }
    ]

-- | Sub-record: Recursively reify
-- Overlappable instance for nested records that support reification
instance {-# OVERLAPPABLE #-} (Generic a, GReifyTools (Rep a)) => ReifyField a where
  reifyField _ record = reifyTools record

-- | Helper: CamelCase to snake_case
camelToSnake :: Text -> Text
camelToSnake = T.pack . go . T.unpack
  where
    go [] = []
    go (c:cs) = toLower c : go' c cs
    
    go' _ [] = []
    go' prev (c:cs)
      | isUpper c =
          if isLower prev || (isUpper prev && (not (null cs) && isLower (head cs)))
          then '_' : toLower c : go' c cs
          else toLower c : go' c cs
      | otherwise = c : go' c cs

-- ════════════════════════════════════════════════════════════════════════════
-- CONSTRUCTION HELPERS (Smart Constructors)
-- ════════════════════════════════════════════════════════════════════════════

-- | Type class to construct SchemaBuilder from type-level info.
--
-- This allows us to write `toolSchema` and have it infer the schema/description
-- from the type signature `AsSchema :- Tool I O :@ Description D`.
class ConstructSchema nodeDef where
  constructSchema :: SchemaNode nodeDef

instance (HasJSONSchema input) => ConstructSchema (Tool input output) where
  constructSchema = SchemaNode $ SchemaBuilder
    { sbDescription = Nothing
    , sbInputSchema = schemaToValue (jsonSchema @input)
    }

instance ConstructSchema (Hook input output) where
  constructSchema = SchemaNode $ SchemaBuilder
    { sbDescription = Nothing
    , sbInputSchema = Null
    }

instance (KnownSymbol desc, ConstructSchema node) => ConstructSchema (node :@ Description desc) where
  constructSchema = 
    let SchemaNode base = constructSchema @node
    in SchemaNode $ base { sbDescription = Just (T.pack $ symbolVal (Proxy @desc)) }

-- | Value-level helper to construct the field value.
--
-- Usage:
-- myTool = toolSchema
toolSchema :: forall nodeDef. ConstructSchema nodeDef => SchemaNode nodeDef
toolSchema = constructSchema @nodeDef

-- | Value-level helper to construct hook fields.
hookSchema :: forall nodeDef. ConstructSchema nodeDef => SchemaNode nodeDef
hookSchema = constructSchema @nodeDef

