{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Generic dispatch for Role Tools.
--
-- This module implements runtime dispatch of tool calls to handlers defined
-- in the 'mode :- record' pattern.
module ExoMonad.Control.Role.Tool.Dispatch
  ( -- * Dispatch
    dispatchTool
  , GDispatchTool(..)
  , DispatchField(..)
  , DispatchResult(..)
  ) where

import Control.Monad.Freer (Eff)
import Data.Aeson (Value, FromJSON, ToJSON, fromJSON, toJSON)
import qualified Data.Aeson as A
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)

import ExoMonad.Graph.Generic (AsHandler, ToolHandler(..))
import ExoMonad.Control.Role.Schema (camelToSnake)

-- ════════════════════════════════════════════════════════════════════════════
-- DISPATCH RESULT
-- ════════════════════════════════════════════════════════════════════════════

-- | Result of attempting to dispatch a tool call.
data DispatchResult es
  = ToolNotFound
    -- ^ Tool name not found in record
  | ToolFound (Eff es Value)
    -- ^ Tool found; execute to get result
  | ToolParseError Text
    -- ^ Tool found but arguments invalid

-- ════════════════════════════════════════════════════════════════════════════
-- DISPATCH CLASS
-- ════════════════════════════════════════════════════════════════════════════

-- | Dispatch a tool call to a record of handlers.
--
-- @
-- result <- dispatchTool handlers "spawn_agents" argsJson
-- @
dispatchTool 
  :: (Generic (r (AsHandler es)), GDispatchTool (Rep (r (AsHandler es))) es)
  => r (AsHandler es) 
  -> Text   -- ^ Tool name (snake_case)
  -> Value  -- ^ Arguments
  -> DispatchResult es
dispatchTool record toolName args = gDispatchTool (from record) toolName args

-- | Generic dispatch class.
class GDispatchTool f es where
  gDispatchTool :: f p -> Text -> Value -> DispatchResult es

-- Unwrap metadata
instance GDispatchTool f es => GDispatchTool (M1 D c f) es where
  gDispatchTool (M1 x) = gDispatchTool x

instance GDispatchTool f es => GDispatchTool (M1 C c f) es where
  gDispatchTool (M1 x) = gDispatchTool x

-- Product: try left, then right
instance (GDispatchTool l es, GDispatchTool r es) => GDispatchTool (l :*: r) es where
  gDispatchTool (l :*: r) toolName args =
    case gDispatchTool l toolName args of
      ToolNotFound -> gDispatchTool r toolName args
      result -> result

-- Named Field: check name match and dispatch to field value
instance (KnownSymbol name, DispatchField field es) 
      => GDispatchTool (M1 S ('MetaSel ('Just name) su ss ds) (K1 i field)) es where
  gDispatchTool (M1 (K1 x)) reqToolName args
    | camelToSnake fieldName == reqToolName = dispatchField x args
    | otherwise = ToolNotFound
    where
      fieldName = T.pack $ symbolVal (Proxy @name)

-- ════════════════════════════════════════════════════════════════════════════
-- FIELD DISPATCH
-- ════════════════════════════════════════════════════════════════════════════

-- | Dispatch to a field value (Tool Handler or Sub-record).
class DispatchField field es where
  dispatchField :: field -> Value -> DispatchResult es

-- | Leaf: ToolHandler
instance (FromJSON input, ToJSON output) => DispatchField (ToolHandler es input output) es where
  dispatchField (ToolHandler handler) args =
    case fromJSON args of
      A.Error msg -> ToolParseError (T.pack msg)
      A.Success input -> ToolFound $ do
        output <- handler input
        pure (toJSON output)

-- | Sub-record: Recursively dispatch
-- Overlappable instance for nested records that support generic dispatch
instance {-# OVERLAPPABLE #-} (Generic (r (AsHandler es)), GDispatchTool (Rep (r (AsHandler es))) es) 
      => DispatchField (r (AsHandler es)) es where
  dispatchField record args = 
    -- For sub-records, we need to dispatch on the tool name *inside* the sub-record?
    -- Wait, the `gDispatchTool` takes `toolName`.
    -- `dispatchField` only takes `Value` (args).
    -- If we matched the field name (e.g. `orchestration`), that means the tool name IS "orchestration".
    -- But "orchestration" is a group, not a tool!
    --
    -- ISSUE: The current traversal matches the top-level field name against the tool name.
    -- But we want a flattened namespace where `spawn_agents` matches inside `orchestration`.
    --
    -- If `orchestration` field is matched, it means `reqToolName` == `orchestration`.
    -- That's not what we want. We want to search *inside* `orchestration`.
    --
    -- Correction: The generic traversal should NOT require matching the field name if the field is a sub-record.
    -- It should try to dispatch into the sub-record regardless of the field name?
    --
    -- No, simpler: `GDispatchTool` walks the tree.
    -- If it hits a leaf (ToolHandler), it checks the name.
    -- If it hits a node (Sub-record), it descends.
    --
    -- But `M1 S` wraps the field.
    -- If the field is a ToolHandler, we check the name.
    -- If the field is a sub-record, we assume the sub-record fields are "hoisted" to this level?
    --
    -- User said: "Compose smaller mode :- records... This preserves logical grouping"
    -- User also said: "Field name is canonical... Tool name derived from record field name".
    --
    -- Example:
    -- TLTools { github :: mode :- GitHubTools }
    -- GitHubTools { issueList :: mode :- Tool ... }
    --
    -- Tool name is `gh_issue_list` (from `GitHubTools` field `issueList`? Or `ghIssueList` field?).
    -- Wait, `GitHubTools` definition:
    -- data GitHubTools mode = GitHubTools { ghIssueList :: ... }
    --
    -- So the tool name is `gh_issue_list`.
    --
    -- The `TLTools` record has a field `github`.
    -- Does `github` appear in the tool name? e.g. `github_gh_issue_list`?
    -- Usually "flattened" means we just look for `gh_issue_list` anywhere in the tree.
    --
    -- So:
    -- When traversing `TLTools`:
    -- Field `github`: It's a sub-record. We should ignore the name "github" and recurse into the value.
    -- Field `specific`: It's a sub-record. Ignore name "specific", recurse.
    --
    -- How do we know if it's a Tool or a Sub-record at the Generic `M1 S` level?
    -- `K1 i field` holds the type.
    -- We can use instances on `field`.
    --
    -- Redesign `GDispatchTool`:
    -- `gDispatchTool` calls `dispatchField` on the field value, passing the *tool name* and args.
    --
    -- `dispatchField` for ToolHandler: checks if *my* name matches the tool name.
    -- But `ToolHandler` doesn't know its name! The name is in the `M1 S` wrapper above.
    --
    -- So `GDispatchTool` needs to handle the name check.
    --
    -- Revised Logic:
    -- `GDispatchTool` for `M1 S ... (K1 field)` calls `dispatchNode @name field toolName args`.
    --
    -- `class DispatchNode (name :: Symbol) field es`:
    --   `dispatchNode :: field -> Text -> Value -> DispatchResult es`
    --
    -- Instance for ToolHandler:
    --   Checks if `camelToSnake name` == `toolName`.
    --   If match, run.
    --
    -- Instance for Sub-record (Overlappable):
    --   Ignores `name` (the field name holding the sub-record).
    --   Recurse: `dispatchTool field toolName args`.
    --
    -- This achieves the "flattening" effect: sub-records are transparently searched.
    
    -- Error handling: If multiple sub-records contain the same tool name, the first one found wins (DFS).
    -- Since Generic product tries left then right, we have a defined order.
    
    error "Not implemented"

-- ════════════════════════════════════════════════════════════════════════════
-- NODE DISPATCH
-- ════════════════════════════════════════════════════════════════════════════

-- | Dispatch to a named node (Leaf or Sub-record).
class DispatchNode (name :: Symbol) field es where
  dispatchNode :: field -> Text -> Value -> DispatchResult es

-- | Leaf: ToolHandler
-- Check if this field's name matches the requested tool name.
instance (KnownSymbol name, FromJSON input, ToJSON output) 
      => DispatchNode name (ToolHandler es input output) es where
  dispatchNode (ToolHandler handler) reqToolName args
    | myName == reqToolName = 
        case fromJSON args of
          A.Error msg -> ToolParseError (T.pack msg)
          A.Success input -> ToolFound $ do
            output <- handler input
            pure (toJSON output)
    | otherwise = ToolNotFound
    where
      myName = camelToSnake (T.pack $ symbolVal (Proxy @name))

-- | Sub-record: Recursively dispatch
-- Ignore this field's name, search inside the sub-record.
instance {-# OVERLAPPABLE #-} (Generic (r (AsHandler es)), GDispatchTool (Rep (r (AsHandler es))) es) 
      => DispatchNode name (r (AsHandler es)) es where
  dispatchNode record reqToolName args = 
    dispatchTool record reqToolName args

