{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Template Haskell derivation for 'ToolRecord' instances.
--
-- This module provides automatic derivation of 'ToolRecord' instances,
-- eliminating boilerplate for tool definitions.
--
-- = Usage
--
-- @
-- data AnalysisTools es = AnalysisTools
--   { search :: SearchArgs -> Eff es SearchResult
--   , readSection :: ReadArgs -> Eff es SectionContent
--   }
--
-- deriveToolRecord ''AnalysisTools
--   [ Tool "search" "Search the document for relevant content"
--   , Tool "readSection" "Read a specific section by index"
--   ]
-- @
--
-- = Requirements
--
-- 1. The type must be a single-constructor record type with one type variable @es@
-- 2. Each field must have type @args -> Eff es result@ where:
--    - @args@ has 'FromJSON' and 'HasJSONSchema' instances
--    - @result@ has 'ToJSON' instance
-- 3. Every field must have a corresponding 'Tool' entry with description
--
-- = Generated Code
--
-- The derivation generates:
--
-- @
-- instance ToolRecord AnalysisTools where
--   toolSchemas _ =
--     [ ToolSchema "search" "Search the document for relevant content" (schemaToValue $ jsonSchema \@SearchArgs)
--     , ToolSchema "read_section" "Read a specific section by index" (schemaToValue $ jsonSchema \@ReadArgs)
--     ]
--   dispatchTool tools name input = case name of
--     "search" -> dispatchHandler (tools.search) name input
--     "read_section" -> dispatchHandler (tools.readSection) name input
--     _ -> pure $ Left $ ToolNotFound name
-- @
module ExoMonad.LLM.Tools.TH
  ( -- * Main Derivation
    deriveToolRecord

    -- * Tool Metadata
  , Tool(..)
  ) where

import Language.Haskell.TH
import Data.Char (isUpper, toLower)
import qualified Data.Text as T
import Control.Monad (unless, forM)
import GHC.Records (getField)
import Data.List (find)

-- Import for Name references in generated code
import ExoMonad.LLM.Tools
  ( ToolRecord(..)
  , ToolSchema(..)
  , dispatchHandler
  , ToolDispatchError(..)
  )
import ExoMonad.Schema (schemaToValue)
import ExoMonad.StructuredOutput.Class (HasJSONSchema(..))

-- | Tool metadata for derivation.
--
-- Associates a field name with a description for the LLM.
data Tool = Tool
  { toolFieldName :: String
    -- ^ Haskell field name (must match exactly)
  , toolDescription :: String
    -- ^ Description shown to the LLM
  }
  deriving (Show, Eq)

-- | Derive a 'ToolRecord' instance for a tool record type.
--
-- The type must be a single-constructor record where each field is
-- a function @args -> Eff es result@.
--
-- All fields must have a corresponding 'Tool' entry providing the description.
-- Missing fields cause a compile-time error.
--
-- Tool names are derived from field names by converting camelCase to snake_case.
--
-- @
-- data MyTools es = MyTools
--   { search :: SearchArgs -> Eff es SearchResult
--   , lookupById :: LookupArgs -> Eff es LookupResult
--   }
--
-- deriveToolRecord ''MyTools
--   [ Tool "search" "Search for items by query"
--   , Tool "lookupById" "Look up an item by its ID"
--   ]
-- @
--
-- This generates tool names @"search"@ and @"lookup_by_id"@.
deriveToolRecord :: Name -> [Tool] -> Q [Dec]
deriveToolRecord typeName tools = do
  info <- reify typeName
  case info of
    TyConI (DataD _ _ [tyVarBndr] _ [RecC _conName fields] _) -> do
      -- Validate type variable
      let tyVarName = getTyVarName tyVarBndr

      -- Validate all fields have descriptions
      let fieldNames = [nameBase n | (n, _, _) <- fields]
          toolNames = [t.toolFieldName | t <- tools]
          missingFromTools = filter (`notElem` toolNames) fieldNames
          unknownInTools = filter (`notElem` fieldNames) toolNames

      unless (null missingFromTools) $
        fail $ "deriveToolRecord: Missing Tool descriptions for fields: " ++ show missingFromTools
          ++ "\n  Add Tool entries for these fields."

      unless (null unknownInTools) $
        fail $ "deriveToolRecord: Unknown fields in Tool list: " ++ show unknownInTools
          ++ "\n  These don't match any field in " ++ nameBase typeName ++ "."

      -- Process each field
      fieldData <- forM fields $ \(fname, _, ftype) -> do
        argsType <- extractArgsType ftype fname
        let jsonKey = camelToSnake (nameBase fname)
            -- Find the Tool entry for this field (guaranteed to exist by validation above)
        desc <- case find (\t -> t.toolFieldName == nameBase fname) tools of
          Just t -> pure t.toolDescription
          Nothing -> fail $ "deriveToolRecord: BUG - couldn't find Tool for field " ++ nameBase fname
        pure (fname, argsType, jsonKey, desc)

      -- Generate instance
      genToolRecordInstance typeName tyVarName fieldData

    TyConI (DataD _ _ tvars _ cons _) ->
      fail $ "deriveToolRecord: " ++ nameBase typeName
        ++ " - expected single-constructor record with one type variable, but got "
        ++ show (length tvars) ++ " type variable(s) and "
        ++ show (length cons) ++ " constructor(s)"

    _ -> fail $ "deriveToolRecord: " ++ nameBase typeName ++ " must be a data type"

-- | Extract the type variable name from a TyVarBndr
getTyVarName :: TyVarBndr a -> Name
getTyVarName (PlainTV n _) = n
getTyVarName (KindedTV n _ _) = n

-- | Extract the @args@ type from @args -> Eff es result@.
extractArgsType :: Type -> Name -> Q Type
extractArgsType ftype fname = case ftype of
  -- Pattern: args -> Eff es result
  AppT (AppT ArrowT argsType) _ -> pure argsType

  -- Pattern: forall x. args -> Eff es result (unwrap forall)
  ForallT _ _ innerType -> extractArgsType innerType fname

  _ -> fail $ "deriveToolRecord: Field '" ++ nameBase fname
    ++ "' must have type (args -> Eff es result), but got: " ++ pprint ftype

-- | Convert camelCase to snake_case.
--
-- @camelToSnake "search" = "search"@
-- @camelToSnake "lookupById" = "lookup_by_id"@
camelToSnake :: String -> String
camelToSnake = go True
  where
    go _ [] = []
    go isFirst (c:cs)
      | isUpper c =
          let lower = toLower c
          in if isFirst
             then lower : go False cs
             else '_' : lower : go False cs
      | otherwise = c : go False cs

-- | Generate the ToolRecord instance.
genToolRecordInstance :: Name -> Name -> [(Name, Type, String, String)] -> Q [Dec]
genToolRecordInstance typeName _tyVarName fieldData = do
  -- Generate toolSchemas method
  toolSchemasExp <- genToolSchemas fieldData

  -- Generate dispatchTool method
  dispatchToolExp <- genDispatchTool fieldData

  -- Build the instance
  pure [InstanceD Nothing [] (AppT (ConT ''ToolRecord) (ConT typeName))
    [ FunD 'toolSchemas
        [Clause [WildP] (NormalB toolSchemasExp) []]
    , FunD 'dispatchTool
        [Clause [] (NormalB dispatchToolExp) []]
    ]]

-- | Generate the toolSchemas list expression.
--
-- @
-- [ ToolSchema "search" "Search for items" (schemaToValue $ jsonSchema \@SearchArgs)
-- , ToolSchema "lookup" "Look up by ID" (schemaToValue $ jsonSchema \@LookupArgs)
-- ]
-- @
genToolSchemas :: [(Name, Type, String, String)] -> Q Exp
genToolSchemas fieldData = do
  schemas <- forM fieldData $ \(_fname, argsType, jsonKey, desc) -> do
    -- Build: ToolSchema jsonKey desc (schemaToValue $ jsonSchema @ArgsType)
    let schemaExp = AppTypeE (VarE 'jsonSchema) argsType
    [| ToolSchema
         (T.pack $(litE (stringL jsonKey)))
         (T.pack $(litE (stringL desc)))
         (schemaToValue $(pure schemaExp))
     |]
  listE (map pure schemas)

-- | Generate the dispatchTool case expression.
--
-- @
-- \tools name input -> case name of
--   "search" -> dispatchHandler (getField \@"search" tools) name input
--   "lookup" -> dispatchHandler (getField \@"lookup" tools) name input
--   _ -> pure $ Left $ ToolNotFound name
-- @
genDispatchTool :: [(Name, Type, String, String)] -> Q Exp
genDispatchTool fieldData = do
  toolsName <- newName "tools"
  nameName <- newName "name"
  inputName <- newName "input"

  -- Build case matches
  matchCases <- forM fieldData $ \(fname, _argsType, jsonKey, _desc) -> do
    -- Build: dispatchHandler (getField @"fieldName" tools) name input
    -- Note: Using getField because NoFieldSelectors is enabled
    let fieldNameStr = nameBase fname
    match (litP (stringL jsonKey))
          (normalB [| dispatchHandler
                        (getField @($(litT (strTyLit fieldNameStr))) $(varE toolsName))
                        $(varE nameName)
                        $(varE inputName)
                    |])
          []

  -- Add wildcard case for unknown tools
  wildcardCase <- match wildP
                        (normalB [| pure $ Left $ ToolNotFound $(varE nameName) |])
                        []

  caseExp <- caseE (varE nameName) (map pure (matchCases ++ [wildcardCase]))

  -- Build lambda: \tools name input -> case name of ...
  lamE [varP toolsName, varP nameName, varP inputName] (pure caseExp)
