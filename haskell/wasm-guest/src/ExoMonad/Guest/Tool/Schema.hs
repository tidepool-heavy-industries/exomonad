{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ExoMonad.Guest.Tool.Schema

  ( JsonSchema (..),
    genericToolSchema,
    genericToolSchemaWith,
  )
where

import Data.Aeson (Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Char (isUpper, toLower)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import GHC.TypeLits (KnownSymbol, symbolVal)

-- | Typeclass for types that can be represented as JSON Schema.
class JsonSchema a where
  toSchema :: Value
  default toSchema :: (Generic a, GJsonSchema (Rep a)) => Value
  toSchema = genericToolSchema @a

instance JsonSchema Text where
  toSchema = object ["type" .= ("string" :: Text)]


instance JsonSchema String where
  toSchema = object ["type" .= ("string" :: Text)]

instance JsonSchema Int where
  toSchema = object ["type" .= ("integer" :: Text)]

instance JsonSchema Integer where
  toSchema = object ["type" .= ("integer" :: Text)]

instance JsonSchema Bool where
  toSchema = object ["type" .= ("boolean" :: Text)]

instance JsonSchema Double where
  toSchema = object ["type" .= ("number" :: Text)]

instance (JsonSchema a) => JsonSchema [a] where
  toSchema =
    object
      [ "type" .= ("array" :: Text),
        "items" .= toSchema @a
      ]

instance (JsonSchema a) => JsonSchema (Maybe a) where
  toSchema = toSchema @a

-- | Check if a type is optional (Maybe).
class IsOptional a where
  isOptional :: Bool

instance IsOptional (Maybe a) where
  isOptional = True

instance {-# OVERLAPPABLE #-} IsOptional a where
  isOptional = False

-- | Derive a JSON schema for a Generic record.
-- Strips common prefixes and converts to snake_case.
genericToolSchema :: forall a. (Generic a, GJsonSchema (Rep a)) => Value
genericToolSchema = genericToolSchemaWith @a []

-- | Derive a JSON schema with explicit field descriptions.
--
-- The descriptions list should use snake_case keys (the same ones that appear in the schema).
genericToolSchemaWith :: forall a. (Generic a, GJsonSchema (Rep a)) => [(Text, Text)] -> Value
genericToolSchemaWith descs =
  case gToSchema (Proxy @(Rep a)) descs of
    SchemaObject props reqs ->
      object $
        [ "type" .= ("object" :: Text),
          "properties" .= props,
          "required" .= reqs
        ]
    SchemaEnum vals ->
      object
        [ "type" .= ("string" :: Text),
          "enum" .= vals
        ]
    SchemaSimple val -> val

-- | Result of generic schema derivation.
data GSchemaResult
  = SchemaObject Value [Text]
  | SchemaEnum [Text]
  | SchemaSimple Value

-- | Generic implementation of JSON Schema derivation.
class GJsonSchema f where
  gToSchema :: Proxy f -> [(Text, Text)] -> GSchemaResult

-- Datatype metadata: unwrap
instance (GJsonSchema f) => GJsonSchema (D1 d f) where
  gToSchema _ descs = gToSchema (Proxy @f) descs

-- Constructor metadata: handle enum (U1) or delegate to fields
instance {-# OVERLAPPING #-} (Constructor c) => GJsonSchema (C1 c U1) where
  gToSchema _ _ =
    let name = conName (undefined :: C1 c U1 p)
     in SchemaEnum [T.pack $ camelToSnake name]

instance (GJsonSchema f) => GJsonSchema (C1 c f) where
  gToSchema _ descs = gToSchema (Proxy @f) descs

-- Product: combine both sides (for records)
instance (GJsonSchema left, GJsonSchema right) => GJsonSchema (left :*: right) where
  gToSchema _ descs =
    let r1 = gToSchema (Proxy @left) descs
        r2 = gToSchema (Proxy @right) descs
     in case (r1, r2) of
          (SchemaObject p1 req1, SchemaObject p2 req2) ->
            SchemaObject (mergeObjects p1 p2) (req1 ++ req2)
          _ -> r1 -- Should not happen for valid records

-- Sum: combine both sides (for enums)
instance (GJsonSchema left, GJsonSchema right) => GJsonSchema (left :+: right) where
  gToSchema _ descs =
    let r1 = gToSchema (Proxy @left) descs
        r2 = gToSchema (Proxy @right) descs
     in case (r1, r2) of
          (SchemaEnum e1, SchemaEnum e2) -> SchemaEnum (e1 ++ e2)
          _ -> r1 -- Should not happen for simple enums

-- Selector (field): extract schema
instance (Selector s, JsonSchema a, IsOptional a) => GJsonSchema (S1 s (K1 i a)) where
  gToSchema _ descs =
    let rawName = selName (undefined :: S1 s (K1 i a) p)
        name = T.pack $ camelToSnake $ stripPrefix rawName
        schema = toSchema @a
        -- Add description if provided
        finalSchema = case lookup name descs of
          Just d -> mergeObjects schema (object [AesonKey.fromText "description" .= d])
          Nothing -> schema
        properties = object [AesonKey.fromText name .= finalSchema]
        required = if isOptional @a then [] else [name]
     in SchemaObject properties required




-- | Merge two Aeson objects.
mergeObjects :: Value -> Value -> Value
mergeObjects (Aeson.Object a) (Aeson.Object b) = Aeson.Object (a <> b)
mergeObjects a _ = a

-- | Strip common field prefixes (e.g., "fpTitle" -> "Title").
-- Strips lowercase prefix until the first uppercase letter.
stripPrefix :: String -> String
stripPrefix s =
  case break isUpper s of
    (_, []) -> s -- No uppercase, return original
    (_, upper) -> upper -- Found uppercase, return from there

-- | Convert camelCase to snake_case.
camelToSnake :: String -> String
camelToSnake [] = []
camelToSnake (c : cs) = toLower c : go cs
  where
    go [] = []
    go (x : xs)
      | isUpper x = '_' : toLower x : go xs
      | otherwise = x : go xs

