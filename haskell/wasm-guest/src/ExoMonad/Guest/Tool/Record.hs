-- | Generic dispatch and reification for tool records.
--
-- This module provides:
--
-- * 'DispatchRecord' - Route tool calls by name through a record
-- * 'ReifyRecord' - Extract tool definitions from a record type
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
-- -- Reify:    Proxy @GitTools -> [ToolDefinition]
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
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Guest.Tool.Class (MCPCallOutput (..), MCPTool (..), ToolDefinition (..), errorResult)
import ExoMonad.Guest.Tool.Mode (AsHandler, AsSchema, Handler (..), Schema (..))
import GHC.Generics

-- ============================================================================
-- Record Dispatch
-- ============================================================================

-- | Dispatch tool calls through a record.
class DispatchRecord (tools :: Type -> Type) where
  -- | Dispatch a tool call by name.
  dispatchRecord :: tools AsHandler -> Text -> Value -> IO MCPCallOutput

instance (Generic (tools AsHandler), GDispatchTool (Rep (tools AsHandler))) => DispatchRecord tools where
  dispatchRecord handlers name args = gDispatch (from handlers) name args

-- | Generic dispatch implementation.
class GDispatchTool (f :: Type -> Type) where
  gDispatch :: f p -> Text -> Value -> IO MCPCallOutput

-- Datatype metadata: unwrap
instance (GDispatchTool f) => GDispatchTool (M1 D meta f) where
  gDispatch (M1 x) = gDispatch x

-- Constructor metadata: unwrap
instance (GDispatchTool f) => GDispatchTool (M1 C meta f) where
  gDispatch (M1 x) = gDispatch x

-- Product: try left, then right
instance (GDispatchTool left, GDispatchTool right) => GDispatchTool (left :*: right) where
  gDispatch (l :*: r) name args = do
    result <- gDispatch l name args
    case mcpError result of
      Just err | "Unknown tool:" `T.isPrefixOf` err -> gDispatch r name args
      _ -> pure result

-- Selector (field with Handler): check if tool name matches, dispatch if so
instance
  (MCPTool tool, FromJSON (ToolArgs tool)) =>
  GDispatchTool (M1 S sel (K1 i (Handler tool)))
  where
  gDispatch (M1 (K1 (Handler handler))) name args
    | name == toolName @tool = case Aeson.fromJSON args of
        Aeson.Success a -> handler a
        Aeson.Error e -> pure $ errorResult $ "Parse error for " <> name <> ": " <> T.pack e
    | otherwise = pure $ errorResult $ "Unknown tool: " <> name

-- Nested sub-record: recurse into it
instance
  (DispatchRecord subRec) =>
  GDispatchTool (M1 S sel (K1 i (subRec AsHandler)))
  where
  gDispatch (M1 (K1 subRecord)) name args = dispatchRecord subRecord name args

-- ============================================================================
-- Record Reification
-- ============================================================================

-- | Reify tool definitions from a record type.
class ReifyRecord (tools :: Type -> Type) where
  -- | Get all tool definitions.
  reifyToolDefs :: Proxy tools -> [ToolDefinition]

instance (GReifyTools (Rep (tools AsSchema))) => ReifyRecord tools where
  reifyToolDefs _ = gReify (Proxy @(Rep (tools AsSchema)))

-- | Generic reification implementation.
class GReifyTools (f :: Type -> Type) where
  gReify :: Proxy f -> [ToolDefinition]

-- Datatype metadata: unwrap
instance (GReifyTools f) => GReifyTools (M1 D meta f) where
  gReify _ = gReify (Proxy @f)

-- Constructor metadata: unwrap
instance (GReifyTools f) => GReifyTools (M1 C meta f) where
  gReify _ = gReify (Proxy @f)

-- Product: combine both sides
instance (GReifyTools left, GReifyTools right) => GReifyTools (left :*: right) where
  gReify _ = gReify (Proxy @left) ++ gReify (Proxy @right)

-- Selector with Schema: extract ToolDefinition using MCPTool
instance
  (MCPTool tool) =>
  GReifyTools (M1 S sel (K1 i (Schema tool)))
  where
  gReify _ =
    [ ToolDefinition
        { tdName = toolName @tool,
          tdDescription = toolDescription @tool,
          tdInputSchema = toolSchema @tool
        }
    ]

-- Nested sub-record: recurse
instance
  (ReifyRecord subRec) =>
  GReifyTools (M1 S sel (K1 i (subRec AsSchema)))
  where
  gReify _ = reifyToolDefs (Proxy @subRec)

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
