{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module ExoMonad.Schema.TH
  ( -- * Main Derivation Functions
    deriveMCPType,
    deriveMCPTypeWith,
    deriveMCPEnum,

    -- * Field Mapping DSL (Blessed Operators)
    FieldMapping (..),
    (??), -- ^ RECOMMENDED: Auto-rename + description
    omit, -- ^ Exclude field from JSON/Schema

    -- * Field Mapping (Advanced - rarely needed)
    FieldMappingPartial,
    (~>), -- ^ Explicit key rename (only for non-standard keys)
    (?), -- ^ Add description to explicit mapping

    -- * Options
    MCPOptions (..),
    defaultMCPOptions,

    -- * Helper functions (required for generated code)
    objectSchema,
    arraySchema,
    describeField,
    emptySchema,
    enumSchema,
  )
where

import Control.Monad (forM)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withObject, withText, (.:), (.:?), (.=))
import Data.Aeson.Key qualified as K
import Data.Char (isUpper, toLower)
import Data.List (delete, foldl', head, intercalate, stripPrefix)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
-- Import types for HasJSONSchema instance generation
import ExoMonad.StructuredOutput.Class (HasJSONSchema (..), JSONSchema (..), SchemaType (..))
import Language.Haskell.TH hiding (Type)
import Language.Haskell.TH.Syntax qualified as TH
import Prelude hiding (head, (??))

-- | Options for MCP type derivation
data MCPOptions = MCPOptions
  { -- | Exact prefix to strip from field names (error if missing)
    fieldPrefix :: String,
    -- | Function to transform field names to JSON keys (default: snake_case)
    fieldModifier :: String -> String
  }

-- | Default options: no prefix, camelToSnake transformation
defaultMCPOptions :: MCPOptions
defaultMCPOptions =
  MCPOptions
    { fieldPrefix = "",
      fieldModifier = camelToSnake
    }

-- | Simple camelCase to snake_case converter
camelToSnake :: String -> String
camelToSnake = go
  where
    go [] = []
    go (c : cs)
      | isUpper c = '_' : toLower c : go cs
      | otherwise = c : go cs

-- ════════════════════════════════════════════════════════════════════════════
-- FIELD MAPPING DSL
-- ════════════════════════════════════════════════════════════════════════════
--
-- RECOMMENDED PATTERN (use for 95% of cases):
--   'fieldName ?? "Description of this field"
--
-- This auto-derives the JSON key from the field name by:
--   1. Stripping the configured prefix (e.g., "sq" from "sqQuery")
--   2. Lowercasing the first character
--   3. Converting camelCase to snake_case
--
-- ADVANCED PATTERN (only when auto-derived key won't work):
--   'fieldName ~> "explicit_key" ? "Description"
--
-- Use (~>) only when:
--   - The field name doesn't follow prefix conventions
--   - You need a JSON key that differs from snake_case transformation
--   - Backwards compatibility with existing API contracts
-- ════════════════════════════════════════════════════════════════════════════

-- | Field mapping DSL types
data FieldMapping
  = -- | Explicit mapping: field -> key + optional description
    Explicit Name String (Maybe String)
  | -- | Auto mapping: field -> derived key + optional description
    Auto Name (Maybe String)
  | -- | Omit field from JSON/Schema
    Omit Name

data FieldMappingPartial = FieldMappingPartial Name String

-- | Map a field to an explicit JSON key.
--
-- __Prefer @(??)@ unless you need non-standard key names.__
-- This operator should only be used when the auto-derived key won't work.
infixl 1 ~>

(~>) :: Name -> String -> FieldMapping
n ~> k = Explicit n k Nothing

-- | Add a description to a mapping
infixl 0 ?

(?) :: FieldMapping -> String -> FieldMapping
(Explicit n k _) ? d = Explicit n k (Just d)
(Auto n _) ? d = Auto n (Just d)
(Omit n) ? _ = Omit n -- Should not happen typically

-- | Auto-map a field with a description
infixl 0 ??

(??) :: Name -> String -> FieldMapping
n ?? d = Auto n (Just d)

-- | Explicitly omit a field
omit :: Name -> FieldMapping
omit = Omit

-- | Derive HasJSONSchema, FromJSON, and ToJSON instances for an MCP type
deriveMCPType :: Name -> [FieldMapping] -> Q [Dec]
deriveMCPType = deriveMCPTypeWith defaultMCPOptions

-- | Derive instances with custom options
deriveMCPTypeWith :: MCPOptions -> Name -> [FieldMapping] -> Q [Dec]
deriveMCPTypeWith opts typeName mappings = do
  info <- reify typeName
  case info of
    TyConI (DataD _ _ _ _ [RecC conName fields] _) ->
      deriveForRecord opts typeName conName fields mappings
    TyConI (NewtypeD _ _ _ _ (RecC conName fields) _) ->
      deriveForRecord opts typeName conName fields mappings
    _ -> fail $ "deriveMCPType: " ++ show typeName ++ " must be a single-constructor record type"

deriveForRecord :: MCPOptions -> Name -> Name -> [VarBangType] -> [FieldMapping] -> Q [Dec]
deriveForRecord opts typeName conName fields mappings = do
  -- 1. Validate fields
  let typeFields = [n | (n, _, _) <- fields]
      typeFieldNames = map nameBase typeFields
      getMappingName m = nameBase (case m of Explicit x _ _ -> x; Auto x _ -> x; Omit x -> x)
      mappedFieldNames = map getMappingName mappings

      missingFields = filter (`notElem` mappedFieldNames) typeFieldNames
      unknownFields = filter (`notElem` typeFieldNames) mappedFieldNames

  unless (null missingFields) $
    fail $
      "deriveMCPType: Fields missing from mapping: " ++ show missingFields

  unless (null unknownFields) $
    fail $
      "deriveMCPType: Unknown fields in mapping: " ++ show unknownFields

  -- 2. Process fields into a usable structure
  fieldData <- forM fields $ \(fname, _, ftype) -> do
    let mapping = head [m | m <- mappings, getMappingName m == nameBase fname]
    case mapping of
      Omit _ -> pure Nothing
      _ -> do
        let (jsonKey, desc) = resolveMapping opts fname mapping
            isOptional = isMaybeType ftype
            -- Auto-append "(optional)" to Maybe field descriptions
            desc' = case (isOptional, desc) of
              (True, Just d) -> Just (d ++ " (optional)")
              (True, Nothing) -> Just "(optional)"
              (False, d) -> d
        schemaExp <- deriveFieldSchema ftype desc'
        pure $ Just (fname, ftype, jsonKey, schemaExp, isOptional)

  let validFields = catMaybes fieldData

  -- 3. Generate HasJSONSchema
  hasJsonSchemaDec <- genHasJSONSchema typeName validFields

  -- 4. Generate FromJSON
  fromJsonDec <- genFromJSON typeName conName validFields

  -- 5. Generate ToJSON
  toJsonDec <- genToJSON typeName validFields

  pure [hasJsonSchemaDec, fromJsonDec, toJsonDec]
  where
    getFieldName (Explicit n _ _) = n
    getFieldName (Auto n _) = n
    getFieldName (Omit n) = n

resolveMapping :: MCPOptions -> Name -> FieldMapping -> (String, Maybe String)
resolveMapping _ _ (Explicit _ k d) = (k, d)
resolveMapping opts fname (Auto _ d) =
  let baseName = nameBase fname
      prefix = opts.fieldPrefix
      processed =
        if null prefix
          then baseName
          else case stripPrefix prefix baseName of
            Just rest -> rest
            Nothing -> error $ T.pack $ "deriveMCPType: Field '" ++ baseName ++ "' does not start with expected prefix '" ++ prefix ++ "'"

      processed' = case processed of
        (c : cs) -> toLower c : cs
        [] -> []
      key = opts.fieldModifier processed'
   in (key, d)
resolveMapping _ _ (Omit _) = error "Omit should be filtered out"

isMaybeType :: TH.Type -> Bool
isMaybeType (AppT (ConT name) _) = nameBase name == "Maybe"
isMaybeType _ = False

deriveFieldSchema :: TH.Type -> Maybe String -> Q Exp
deriveFieldSchema ftype desc = do
  -- Base schema expression
  baseSchema <- typeToSchemaExp ftype
  -- Add description if present
  case desc of
    Just d -> [|describeField $(litE (stringL d)) $(pure baseSchema)|]
    Nothing -> pure baseSchema

-- | Convert a Haskell type to a JSONSchema expression (simplified from original Schema.hs)
typeToSchemaExp :: TH.Type -> Q Exp
typeToSchemaExp typ = case typ of
  ConT name | nameBase name `elem` ["Text", "String", "FilePath"] -> [|emptySchema TString|]
  ConT name | nameBase name `elem` ["Int", "Integer"] -> [|emptySchema TInteger|]
  ConT name | nameBase name `elem` ["Double", "Float"] -> [|emptySchema TNumber|]
  ConT name | nameBase name == "Bool" -> [|emptySchema TBoolean|]
  AppT ListT elemType -> do
    elemSchema <- typeToSchemaExp elemType
    [|arraySchema $(pure elemSchema)|]
  AppT (ConT name) innerType | nameBase name == "Maybe" -> typeToSchemaExp innerType
  ConT name -> [|jsonSchema @($(conT name))|]
  _ -> [|emptySchema TString|] -- Fallback

-- | Generate HasJSONSchema instance
genHasJSONSchema :: Name -> [(Name, TH.Type, String, Exp, Bool)] -> Q Dec
genHasJSONSchema typeName fields = do
  let props = listE [tupE [litE (stringL k), pure s] | (_, _, k, s, _) <- fields]
      required = listE [litE (stringL k) | (_, _, k, _, isOpt) <- fields, not isOpt]

  [d|
    instance HasJSONSchema $(conT typeName) where
      jsonSchema = objectSchema $props $required
    |]
    >>= \case
      [dec] -> pure dec
      _ -> fail "genHasJSONSchema produced unexpected declarations"

-- | Generate FromJSON instance
genFromJSON :: Name -> Name -> [(Name, TH.Type, String, Exp, Bool)] -> Q Dec
genFromJSON typeName conName fields = do
  vName <- newName "v"

  -- Construct the record
  let applicativeParse =
        foldl'
          ( \acc (fname, _, key, _, isOpt) ->
              let keyLit = litE (stringL key)
                  -- We use AppE to apply the operator to the arguments
                  -- acc <*> (v .: "key")
                  -- The operator is (<*>)
                  -- The second arg is (v .: "key")
                  parseExpr =
                    if isOpt
                      then [|$(varE vName) .:? K.fromText (T.pack $keyLit)|]
                      else [|$(varE vName) .: K.fromText (T.pack $keyLit)|]
               in [|$acc <*> $parseExpr|]
          )
          [|pure $(conE conName)|]
          fields

  [d|
    instance FromJSON $(conT typeName) where
      parseJSON = withObject $(litE (stringL (nameBase typeName))) $ \ $(varP vName) -> $applicativeParse
    |]
    >>= \case
      [dec] -> pure dec
      _ -> fail "genFromJSON produced unexpected declarations"

-- | Generate ToJSON instance
genToJSON :: Name -> [(Name, TH.Type, String, Exp, Bool)] -> Q Dec
genToJSON typeName fields = do
  let pairs =
        listE
          [ [|K.fromText (T.pack $(litE (stringL key))) .= $(varE fname) args|]
          | (fname, _, key, _, _) <- fields
          ]

  [d|
    instance ToJSON $(conT typeName) where
      toJSON args = object $pairs
    |]
    >>= \case
      [dec] -> pure dec
      _ -> fail "genToJSON produced unexpected declarations"

-- Helper functions to replicate Schema.hs logic (since we can't import them easily if circular)
-- We need to export them or define them locally to be used in the splice.
-- If the splice uses them, they must be in scope where the splice is USED.
-- The user module imports ExoMonad.Schema, which imports this module.
-- So if ExoMonad.Schema defines objectSchema, and the user module imports ExoMonad.Schema,
-- then objectSchema is in scope.
-- But we need to use 'ExoMonad.Schema.objectSchema in the splice.
-- Or just assume 'objectSchema' is in scope.
-- To be safe, we can try to refer to them fully qualified, but we can't get the Name if we can't import.
--
-- HOWEVER, we can include local definitions in the generated code if we returned a let block, but that's for expressions.
-- This generates Instances.
--
-- Strategy:
-- The splice will contain references to `objectSchema`, `arraySchema`, `describeField`, `emptySchema`.
-- These must be available in the scope where $(deriveMCPType ...) is called.
-- The user is expected to import ExoMonad.Schema.
-- We can add a note about this.
-- Or better: ExoMonad.Schema.TH should NOT be imported by the user directly.
-- The user imports ExoMonad.Schema, which re-exports deriveMCPType.
-- And ExoMonad.Schema exports objectSchema etc.
-- So they will be in scope.

-- Local definitions for use in splices if needed? No, we rely on user scope.
-- But wait, if I use `varE (mkName "objectSchema")`, it captures the name "objectSchema".
-- If I use `[| objectSchema ... |]`, TH looks up `objectSchema` in the *current* module scope (ExoMonad.Schema.TH).
-- So `objectSchema` MUST be defined or imported in ExoMonad.Schema.TH.
--
-- AND I cannot import ExoMonad.Schema because of cycle.
--
-- SOLUTION:
-- I must implement `objectSchema`, `arraySchema`, `describeField`, `emptySchema` in THIS module (ExoMonad.Schema.TH)
-- and export them, OR define them locally and use them.
--
-- Since ExoMonad.Schema already defines them, I should move them to a shared module `ExoMonad.Schema.Core` or similar.
-- Or just duplicate them here (they are small wrappers around JSONSchema constructor).
--
-- I will duplicate them here (as private or exported?) and use them in the splice.
-- If I use them in the splice `[| objectSchema ... |]`, the splice will refer to `ExoMonad.Schema.TH.objectSchema`.
-- So I must export them from here.
-- And ExoMonad.Schema can re-export them (or hide its own and re-export these).
--
-- Let's duplicate them here for now to break the cycle and keep it simple.
-- I'll use the names `objectSchema`, `arraySchema` etc. in the splice, which resolve to this module's versions.

emptySchema :: SchemaType -> JSONSchema
emptySchema t = JSONSchema t Nothing Map.empty [] Nothing Nothing Nothing Nothing

objectSchema :: [(Text, JSONSchema)] -> [Text] -> JSONSchema
objectSchema props required =
  (emptySchema TObject)
    { schemaProperties = Map.fromList props,
      schemaRequired = required
    }

arraySchema :: JSONSchema -> JSONSchema
arraySchema items = (emptySchema TArray) {schemaItems = Just items}

describeField :: Text -> JSONSchema -> JSONSchema
describeField desc schema = schema {schemaDescription = Just desc}

enumSchema :: [Text] -> JSONSchema
enumSchema variants = (emptySchema TString) {schemaEnum = Just variants}

-- ════════════════════════════════════════════════════════════════════════════
-- ENUM DERIVATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Derive HasJSONSchema, FromJSON, and ToJSON for a nullary sum type (enum).
--
-- The enum values are serialized as lowercase strings matching constructor names.
-- Parsing is case-insensitive for convenience.
--
-- Example:
--
-- @
-- data Depth = Low | Medium | High
--   deriving stock (Show, Eq, Generic, Bounded, Enum)

-- $(deriveMCPEnum ''Depth)
-- @
--
-- Generates:
--
-- @
-- instance HasJSONSchema Depth where
--   jsonSchema = enumSchema ["low", "medium", "high"]
--
-- instance FromJSON Depth where
--   parseJSON = withText "Depth" $ \\t ->
--     case T.toLower t of
--       "low"    -> pure Low
--       "medium" -> pure Medium
--       "high"   -> pure High
--       _        -> fail $ "Unknown Depth: " <> T.unpack t
--
-- instance ToJSON Depth where
--   toJSON Low    = String "low"
--   toJSON Medium = String "medium"
--   toJSON High   = String "high"
-- @

deriveMCPEnum :: Name -> Q [Dec]
deriveMCPEnum typeName = do
  info <- reify typeName
  case info of
    TyConI (DataD _ _ _ _ cons _) -> do
      -- Extract nullary constructor names
      let conNames = [n | NormalC n [] <- cons]
      when (length conNames /= length cons) $
        fail $
          "deriveMCPEnum: " ++ show typeName ++ " must have only nullary constructors"
      when (null conNames) $
        fail $
          "deriveMCPEnum: " ++ show typeName ++ " must have at least one constructor"

      genEnumInstances typeName conNames
    _ -> fail $ "deriveMCPEnum: " ++ show typeName ++ " must be a data type with nullary constructors"

genEnumInstances :: Name -> [Name] -> Q [Dec]
genEnumInstances typeName conNames = do
  let -- Convert constructor names to lowercase strings
      conStrings = [(n, map toLower (nameBase n)) | n <- conNames]
      -- Auto-generate description: "One of: value1, value2, ..."
      validValues = map snd conStrings
      autoDesc = "One of: " ++ intercalate ", " validValues

  -- Generate HasJSONSchema instance with auto-description
  hasJsonSchemaDec <-
    [d|
      instance HasJSONSchema $(conT typeName) where
        jsonSchema =
          describeField (T.pack $(litE (stringL autoDesc))) $
            enumSchema $(listE [litE (stringL s) | (_, s) <- conStrings])
      |]

  -- Generate FromJSON instance
  fromJsonDec <- genFromJSONEnum typeName conStrings

  -- Generate ToJSON instance
  toJsonDec <- genToJSONEnum typeName conStrings

  pure $ concat [hasJsonSchemaDec, fromJsonDec, toJsonDec]

genFromJSONEnum :: Name -> [(Name, String)] -> Q [Dec]
genFromJSONEnum typeName conStrings = do
  tVar <- newName "t"
  let validValues = map snd conStrings
      validList = intercalate ", " validValues
      matchCases =
        [ match (litP (stringL s)) (normalB [|pure $(conE n)|]) []
        | (n, s) <- conStrings
        ]
          ++ [ match
                 wildP
                 ( normalB
                     [|fail $ "Unknown " ++ $(litE (stringL (nameBase typeName))) ++ ": " ++ T.unpack $(varE tVar) ++ ". Valid values: " ++ $(litE (stringL validList))|]
                 )
                 []
             ]

  caseExp <- caseE [|T.toLower $(varE tVar)|] matchCases
  [d|
    instance FromJSON $(conT typeName) where
      parseJSON = withText $(litE (stringL (nameBase typeName))) $ \ $(varP tVar) ->
        $(pure caseExp)
    |]

genToJSONEnum :: Name -> [(Name, String)] -> Q [Dec]
genToJSONEnum typeName conStrings = do
  let matchCases =
        [ match (conP n []) (normalB [|String (T.pack $(litE (stringL s)))|]) []
        | (n, s) <- conStrings
        ]

  [d|
    instance ToJSON $(conT typeName) where
      toJSON = $(lamCaseE matchCases)
    |]
