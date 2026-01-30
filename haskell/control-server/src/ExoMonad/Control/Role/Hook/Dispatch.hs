{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Generic dispatch for Role Hooks.
--
-- This module implements runtime dispatch of hook events to handlers defined
-- in the 'mode :- record' pattern.
module ExoMonad.Control.Role.Hook.Dispatch
  ( -- * Dispatch
    dispatchHook,
    GDispatchHook (..),
    HookDispatchResult (..),
  )
where

import Control.Monad.Freer (Eff)
import Data.Aeson (FromJSON, ToJSON, Value, fromJSON, toJSON)
import Data.Aeson qualified as A
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Control.Role.Schema (camelToSnake)
import ExoMonad.Graph.Generic (AsHandler, HookHandler (..))
import GHC.Generics
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

-- ════════════════════════════════════════════════════════════════════════════
-- DISPATCH RESULT
-- ════════════════════════════════════════════════════════════════════════════

-- | Result of attempting to dispatch a hook event.
data HookDispatchResult es
  = -- | Hook name not found in record
    HookNotFound
  | -- | Hook found; execute to get result
    HookFound (Eff es Value)
  | -- | Hook found but arguments invalid
    HookParseError Text

-- ════════════════════════════════════════════════════════════════════════════
-- DISPATCH CLASS
-- ════════════════════════════════════════════════════════════════════════════

-- | Dispatch a hook event to a record of handlers.
--
-- @
-- result <- dispatchHook handlers "session_start" argsJson
-- @
dispatchHook ::
  (Generic (r (AsHandler es)), GDispatchHook (Rep (r (AsHandler es))) es) =>
  r (AsHandler es) ->
  -- | Hook name (snake_case)
  Text ->
  -- | Arguments
  Value ->
  HookDispatchResult es
dispatchHook record hookName args = gDispatchHook (from record) hookName args

-- | Generic dispatch class.
class GDispatchHook f es where
  gDispatchHook :: f p -> Text -> Value -> HookDispatchResult es

-- Unwrap metadata
instance (GDispatchHook f es) => GDispatchHook (M1 D c f) es where
  gDispatchHook (M1 x) = gDispatchHook x

instance (GDispatchHook f es) => GDispatchHook (M1 C c f) es where
  gDispatchHook (M1 x) = gDispatchHook x

-- Product: try left, then right
instance (GDispatchHook l es, GDispatchHook r es) => GDispatchHook (l :*: r) es where
  gDispatchHook (l :*: r) hookName args =
    case gDispatchHook l hookName args of
      HookNotFound -> gDispatchHook r hookName args
      result -> result

-- Named Field: delegate to DispatchNode
instance
  (KnownSymbol name, DispatchNode name field es) =>
  GDispatchHook (M1 S ('MetaSel ('Just name) su ss ds) (K1 i field)) es
  where
  gDispatchHook (M1 (K1 x)) reqHookName args = dispatchNode @name x reqHookName args

-- ════════════════════════════════════════════════════════════════════════════
-- NODE DISPATCH
-- ════════════════════════════════════════════════════════════════════════════

-- | Dispatch to a named node (Leaf or Sub-record).
class DispatchNode (name :: Symbol) field es where
  dispatchNode :: field -> Text -> Value -> HookDispatchResult es

-- | Leaf: HookHandler
-- Check if this field's name matches the requested hook name.
instance
  (KnownSymbol name, FromJSON input, ToJSON output) =>
  DispatchNode name (HookHandler es input output) es
  where
  dispatchNode (HookHandler handler) reqHookName args
    | myName == reqHookName =
        case fromJSON args of
          A.Error msg -> HookParseError (T.pack msg)
          A.Success input -> HookFound $ do
            output <- handler input
            pure (toJSON output)
    | otherwise = HookNotFound
    where
      myName = camelToSnake (T.pack $ symbolVal (Proxy @name))

-- | Sub-record: Recursively dispatch
-- Ignore this field's name, search inside the sub-record.
instance
  {-# OVERLAPPABLE #-}
  (Generic (r (AsHandler es)), GDispatchHook (Rep (r (AsHandler es))) es) =>
  DispatchNode name (r (AsHandler es)) es
  where
  dispatchNode record reqHookName args =
    dispatchHook record reqHookName args
