-- | Generic dispatch and reification for tool records.
--
-- This module provides:
--
-- * 'DispatchRecord' - Route tool calls by name through a record
-- * 'ReifyRecord' - Extract tool definitions from a schema record value
-- * 'camelToSnake' - Convert field names to tool names
--
-- Example:
--
-- @
-- data GitTools mode = GitTools
--   { gitBranch :: mode :- GitBranch
--   , gitStatus :: mode :- GitStatus
--   }
--   deriving Generic
--
-- -- Dispatch: gitTools AsHandler -> "git_branch" -> args -> IO result
-- -- Reify:    gitTools AsSchema -> [ToolDefinition]
-- @
module ExoMonad.Guest.Tool.Record
  ( -- * Record dispatch
    DispatchRecord (..),

    -- * Record reification
    ReifyRecord (..),

    -- * Naming
    camelToSnake,
  )
where

import Data.Aeson (FromJSON, Value)
import Data.Aeson qualified as Aeson
import Data.Char (isUpper, toLower)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Guest.Tool.Class (MCPCallOutput (..), MCPTool (..), ToolDefinition (..), WasmResult (..), errorResult)
import ExoMonad.Guest.Tool.Mode (AsHandler, AsSchema, Handler (..), Schema (..))
import GHC.Generics

-- ============================================================================
-- Record Dispatch
-- ============================================================================

-- | Dispatch tool calls through a record.
class DispatchRecord (tools :: Type -> Type) where
  -- | Dispatch a tool call by name.
  dispatchRecord :: tools AsHandler -> Text -> Value -> IO (WasmResult MCPCallOutput)

instance (Generic (tools AsHandler), GDispatchTool (Rep (tools AsHandler))) => DispatchRecord tools where
  dispatchRecord handlers name args = gDispatch (from handlers) name args

-- | Generic dispatch implementation.
class GDispatchTool (f :: Type -> Type) where
  gDispatch :: f p -> Text -> Value -> IO (WasmResult MCPCallOutput)

-- Datatype metadata: unwrap
instance (GDispatchTool f) => GDispatchTool (M1 D meta f) where
  gDispatch (M1 x) = gDispatch x

-- Constructor metadata: unwrap
instance (GDispatchTool f) => GDispatchTool (M1 C meta f) where
  gDispatch (M1 x) = gDispatch x

-- Product: try left, then right
instance (GDispatchTool left, GDispatchTool right) => GDispatchTool (left :*: right) where
  gDispatch (l :*: r) name args = do
    res <- gDispatch l name args
    case res of
      Done output ->
        case mcpError output of
          Just err | "Unknown tool:" `T.isPrefixOf` err -> gDispatch r name args
          _ -> pure res
      Suspend _ _ -> pure res

-- Selector (field with Handler): check if tool name matches, dispatch if so
instance
  (MCPTool tool, FromJSON (ToolArgs tool)) =>
  GDispatchTool (M1 S sel (K1 i (Handler tool)))
  where
  gDispatch (M1 (K1 (Handler handler))) name args
    | name == toolName @tool = case Aeson.fromJSON args of
        Aeson.Success a -> handler a
        Aeson.Error e -> pure $ Done $ errorResult $ "Parse error for " <> name <> ": " <> T.pack e
    | otherwise = pure $ Done $ errorResult $ "Unknown tool: " <> name

-- Nested sub-record: recurse into it
instance
  (DispatchRecord subRec) =>
  GDispatchTool (M1 S sel (K1 i (subRec AsHandler)))
  where
  gDispatch (M1 (K1 subRecord)) name args = dispatchRecord subRecord name args

-- ============================================================================
-- Record Reification
-- ============================================================================

-- | Reify tool definitions from a schema record value.
class ReifyRecord (tools :: Type -> Type) where
  -- | Get all tool definitions from an explicit schemas value.
  reifyToolDefs :: tools AsSchema -> [ToolDefinition]

instance (Generic (tools AsSchema), GReifyTools (Rep (tools AsSchema))) => ReifyRecord tools where
  reifyToolDefs schemas = gReify (from schemas)

-- | Generic reification implementation (value-based).
class GReifyTools (f :: Type -> Type) where
  gReify :: f p -> [ToolDefinition]

-- Datatype metadata: unwrap
instance (GReifyTools f) => GReifyTools (M1 D meta f) where
  gReify (M1 x) = gReify x

-- Constructor metadata: unwrap
instance (GReifyTools f) => GReifyTools (M1 C meta f) where
  gReify (M1 x) = gReify x

-- Product: combine both sides
instance (GReifyTools left, GReifyTools right) => GReifyTools (left :*: right) where
  gReify (l :*: r) = gReify l ++ gReify r

-- Selector with Schema: read the ToolDefinition from the value
instance GReifyTools (M1 S sel (K1 i (Schema tool))) where
  gReify (M1 (K1 (Schema td))) = [td]

-- Nested sub-record: recurse
instance
  (ReifyRecord subRec) =>
  GReifyTools (M1 S sel (K1 i (subRec AsSchema)))
  where
  gReify (M1 (K1 subRecord)) = reifyToolDefs subRecord

-- ============================================================================
-- Naming
-- ============================================================================

-- | Convert camelCase field name to snake_case tool name.
--
-- @
-- camelToSnake "gitBranch" == "git_branch"
-- camelToSnake "ghListIssues" == "gh_list_issues"
-- @
camelToSnake :: String -> String
camelToSnake = concatMap go
  where
    go c
      | isUpper c = ['_', toLower c]
      | otherwise = [c]
