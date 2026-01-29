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

-- Named Field: delegate to DispatchNode
instance (KnownSymbol name, DispatchNode name field es) 
      => GDispatchTool (M1 S ('MetaSel ('Just name) su ss ds) (K1 i field)) es where
  gDispatchTool (M1 (K1 x)) reqToolName args = dispatchNode @name x reqToolName args

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

