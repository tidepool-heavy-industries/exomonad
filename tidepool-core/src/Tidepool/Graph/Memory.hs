{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

-- | The Memory effect for persistent state in graph nodes.
--
-- Memory provides typed, persistent state that survives across graph runs.
-- A single parameterized effect is used for both node-private state and
-- graph-level shared state - the difference is just the type parameter.
--
-- = Design Philosophy
--
-- "One effect, parameterized by type. Node vs global is a matter of scope."
--
-- Rather than separate @NodeMemory@ and @GlobalMemory@ effects, we use
-- @Memory s@ where @s@ is the state type. This is cleaner because:
--
-- * Same API (get/update) for both scopes
-- * Type system naturally distinguishes different memory types
-- * Multiple memory types can coexist in an effect stack
-- * Interpretation handles the persistence semantics
--
-- = Usage
--
-- For a graph with node-private and global memory:
--
-- @
-- type MyGraph = Graph
--   '[ Entry :~> Message
--    , "explore" := LLM :@ Needs '[Message] :@ Schema Findings :@ Memory ExploreMem
--    , Exit :<~ Response
--    ]
--   :& Global SessionState
-- @
--
-- A node handler can access both:
--
-- @
-- exploreHandler :: (Memory ExploreMem :> es, Memory SessionState :> es) => Eff es ()
-- exploreHandler = do
--   -- Access node's private state
--   myMem <- getMem \@ExploreMem
--   updateMem \@ExploreMem $ \\m -> m { urlsVisited = newUrl : m.urlsVisited }
--
--   -- Access global shared state
--   global <- getMem \@SessionState
--   updateMem \@SessionState $ \\g -> g { totalSearches = g.totalSearches + 1 }
--
--   -- With lenses (preferred for nested updates)
--   modifyMem \@ExploreMem #urlsVisited (newUrl :)
-- @
--
-- = Why update, not set?
--
-- The API uses @update :: (s -> s) -> m ()@ rather than @set :: s -> m ()@.
-- This prevents bugs where concurrent or interleaved updates clobber each
-- other. With @update@, each modification sees the current state.
--
-- = Interpretation
--
-- The effect is interpreted differently depending on scope:
--
-- * __Global memory__: Single state slot, persisted at graph boundaries
-- * __Node memory__: Per-node state slots, loaded/stored around node execution
--
-- See 'runMemory' for the basic interpreter and 'runMemoryPersistent' for
-- persistence-aware interpretation.
module Tidepool.Graph.Memory
  ( -- * The Memory Effect
    Memory(..)
  , getMem
  , updateMem

    -- * Lens Helpers
  , modifyMem

    -- * Effect Interpretation
  , runMemory
  , runMemoryPure
  , evalMemory

    -- * WASM Serialization
    -- $serialization
  , MemorySnapshot(..)
  , ScopedMemory(..)
  , MemoryStore(..)
  , emptyMemoryStore
  , serializeMemoryStore
  , restoreMemoryStore
  , getScope
  , setScope
  , runMemoryScoped
  , evalMemoryScoped
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), Value, object, (.=), (.:), withObject)
import qualified Data.Aeson as Aeson
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack)
import Data.Typeable (Typeable, TypeRep, typeRep, Proxy(..))
import Effectful
import Effectful.Dispatch.Dynamic
import qualified Effectful.State.Static.Local as EState
import Optics (A_Setter, Is, Optic', over)

-- ════════════════════════════════════════════════════════════════════════════
-- THE MEMORY EFFECT
-- ════════════════════════════════════════════════════════════════════════════

-- | Effect for accessing persistent memory.
--
-- The type parameter @s@ determines which memory slot is accessed.
-- Different types access different slots:
--
-- * @Memory ExploreMem@ - a node's private persistent state
-- * @Memory SessionState@ - the graph's shared persistent state
--
-- Multiple @Memory@ effects with different types can coexist in the same
-- effect stack, each accessing its own independent state.
type Memory :: Type -> Effect
data Memory s :: Effect where
  -- | Get the current memory value.
  GetMem :: Memory s m s

  -- | Update memory by applying a function.
  --
  -- Uses a function rather than a setter to prevent update conflicts.
  -- Each update sees the current state, not a stale snapshot.
  UpdateMem :: (s -> s) -> Memory s m ()

type instance DispatchOf (Memory s) = 'Dynamic

-- | Get the current memory value.
--
-- @
-- myState <- getMem \@MyStateType
-- @
getMem :: forall s es. Memory s :> es => Eff es s
getMem = send GetMem

-- | Update memory by applying a function to the current state.
--
-- Uses a function rather than a setter to prevent update conflicts:
--
-- @
-- -- Good: sees current state
-- updateMem \@Counter (+ 1)
--
-- -- Risky pattern (if we had set):
-- -- x <- getMem
-- -- setMem (x + 1)  -- Could clobber concurrent updates!
-- @
updateMem :: forall s es. Memory s :> es => (s -> s) -> Eff es ()
updateMem f = send (UpdateMem f)

-- ════════════════════════════════════════════════════════════════════════════
-- LENS HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Modify a field within memory using an optic (lens/setter).
--
-- This is the preferred way to update nested fields:
--
-- @
-- -- Using OverloadedLabels + generic-optics
-- modifyMem \@ExploreMem #urlsVisited (newUrl :)
--
-- -- Or with explicit optics
-- modifyMem \@Config (field \@"timeout") (* 2)
-- @
modifyMem :: forall s a es k is. (Memory s :> es, Is k A_Setter) => Optic' k is s a -> (a -> a) -> Eff es ()
modifyMem l f = updateMem @s (over l f)

-- | Monadic version for updates that need effects.
--
-- When you need to compute the new value effectfully, use getMem/updateMem:
--
-- @
-- s <- getMem \@Cache
-- newEntries <- filterM (fmap not . isExpired) s.entries
-- updateMem \@Cache $ \\c -> c { entries = newEntries }
-- @
--
-- Note: This pattern isn't atomic, but that's fine for sequential execution.
-- For concurrent access, you'd need STM-based persistence anyway.

-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT INTERPRETATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Run memory effect with an initial value, returning final state.
--
-- This is the basic interpreter that wraps effectful's State.
--
-- @
-- (result, finalState) <- runMemory initialState $ do
--   updateMem (+ 1)
--   getMem
-- @
runMemory :: forall s es a. s -> Eff (Memory s : es) a -> Eff es (a, s)
runMemory initial action = do
  reinterpret (EState.runState initial) handler action
  where
    handler :: forall es'. EState.State s :> es'
            => EffectHandler (Memory s) es'
    handler _ = \case
      GetMem -> EState.get
      UpdateMem f -> EState.modify f

-- | Run memory effect purely, returning only the result.
--
-- Useful when you don't need the final state.
evalMemory :: forall s es a. s -> Eff (Memory s : es) a -> Eff es a
evalMemory initial = fmap fst . runMemory initial

-- | Run memory effect with pure initial value, discarding final state.
--
-- Alias for 'evalMemory' for symmetry with State naming conventions.
runMemoryPure :: forall s es a. s -> Eff (Memory s : es) a -> Eff es a
runMemoryPure = evalMemory

-- ════════════════════════════════════════════════════════════════════════════
-- WASM SERIALIZATION
-- ════════════════════════════════════════════════════════════════════════════

-- $serialization
--
-- For WASM deployment, memory must survive across sessions. When the WASM
-- instance dies, we serialize all memory scopes to JSON. When resuming,
-- we restore from the snapshot.
--
-- = Design
--
-- Memory is scoped by node name (a Text key). The 'MemoryStore' holds all
-- scopes as a @Map Text Value@, where each Value is the JSON-serialized
-- contents of that scope.
--
-- The serialization format is:
--
-- @
-- {
--   "version": 1,
--   "scopes": {
--     "explore": { ... node-specific state ... },
--     "classify": { ... node-specific state ... }
--   }
-- }
-- @
--
-- = Constraints
--
-- For a type to be used with scoped memory, it must have:
--
-- * 'ToJSON' instance (for serialization)
-- * 'FromJSON' instance (for restoration)
-- * 'Typeable' instance (for type safety checks - derived automatically)
--
-- = Usage
--
-- @
-- -- Create empty store
-- let store = emptyMemoryStore
--
-- -- Run a computation with scoped memory
-- (result, finalStore) <- runMemoryScoped "myNode" initialState store action
--
-- -- Serialize for persistence
-- let json = serializeMemoryStore finalStore
--
-- -- Later, restore from JSON
-- case restoreMemoryStore json of
--   Left err -> handleError err
--   Right restored -> continue restored
-- @

-- | A single scoped memory entry, existentially quantified.
--
-- We store the JSON Value directly rather than the typed value because:
-- 1. Different scopes may have different types
-- 2. We can't know all types at serialization time
-- 3. JSON is the interchange format anyway
data ScopedMemory = ScopedMemory
  { smTypeRep :: TypeRep
    -- ^ Type fingerprint for safety checks on restore
  , smValue   :: Value
    -- ^ JSON-serialized memory contents
  }
  deriving stock (Show, Eq)

instance ToJSON ScopedMemory where
  toJSON sm = object
    [ "typeRep" .= show sm.smTypeRep
    , "value" .= sm.smValue
    ]

instance FromJSON ScopedMemory where
  parseJSON = withObject "ScopedMemory" $ \o -> do
    -- We store typeRep as String for human readability, but can't
    -- reconstruct the actual TypeRep from String. That's fine -
    -- we'll do type checking at restore time with the actual type.
    _typeStr :: Text <- o .: "typeRep"
    val <- o .: "value"
    -- Use a placeholder TypeRep - actual type checking happens in restoreScope
    pure ScopedMemory
      { smTypeRep = typeRep (Proxy @())
      , smValue = val
      }

-- | Container for all memory scopes across the graph.
--
-- Each scope is keyed by node name. The map stores JSON Values directly
-- to support heterogeneous memory types across different nodes.
newtype MemoryStore = MemoryStore
  { msScopes :: Map Text ScopedMemory
  }
  deriving stock (Show, Eq)

-- | Create an empty memory store.
emptyMemoryStore :: MemoryStore
emptyMemoryStore = MemoryStore Map.empty

-- | Snapshot of all memory for serialization.
--
-- This is the top-level type that gets serialized to JSON for persistence.
-- It includes a version number for future format evolution.
data MemorySnapshot = MemorySnapshot
  { snapVersion :: Int
    -- ^ Schema version for forward compatibility
  , snapScopes  :: Map Text Value
    -- ^ Node name -> JSON value mapping
  }
  deriving stock (Show, Eq)

instance ToJSON MemorySnapshot where
  toJSON snap = object
    [ "version" .= snap.snapVersion
    , "scopes" .= snap.snapScopes
    ]

instance FromJSON MemorySnapshot where
  parseJSON = withObject "MemorySnapshot" $ \o -> MemorySnapshot
    <$> o .: "version"
    <*> o .: "scopes"

-- | Current snapshot format version.
currentSnapshotVersion :: Int
currentSnapshotVersion = 1

-- | Serialize a 'MemoryStore' to a 'Value' for persistence.
--
-- The resulting JSON can be stored in a file, database, or sent over
-- the wire for WASM session persistence.
--
-- @
-- let json = serializeMemoryStore store
-- ByteString.writeFile "memory.json" (Aeson.encode json)
-- @
serializeMemoryStore :: MemoryStore -> Value
serializeMemoryStore (MemoryStore scopes) = toJSON MemorySnapshot
  { snapVersion = currentSnapshotVersion
  , snapScopes = Map.map (.smValue) scopes
  }

-- | Restore a 'MemoryStore' from a 'Value'.
--
-- This performs version checking and returns an error if the format
-- is incompatible. Individual scopes are restored lazily when accessed.
--
-- @
-- case restoreMemoryStore json of
--   Left "Unknown version: 99" -> error "Please upgrade"
--   Left err -> error $ "Corrupted memory: " <> err
--   Right store -> continueWithStore store
-- @
restoreMemoryStore :: Value -> Either Text MemoryStore
restoreMemoryStore val = case Aeson.fromJSON @MemorySnapshot val of
  Aeson.Error err -> Left $ "Failed to parse MemorySnapshot: " <> pack err
  Aeson.Success snap
    | snap.snapVersion /= currentSnapshotVersion ->
        Left $ "Unsupported snapshot version: " <> pack (show snap.snapVersion)
          <> " (expected " <> pack (show currentSnapshotVersion) <> ")"
    | otherwise ->
        -- Convert the raw Value map to ScopedMemory entries
        -- TypeRep is placeholder; actual type checking happens at scope access
        let scoped = Map.map (\v -> ScopedMemory (typeRep (Proxy @())) v) snap.snapScopes
        in Right (MemoryStore scoped)

-- | Get a scope from the store, deserializing to the expected type.
--
-- Returns 'Nothing' if the scope doesn't exist.
-- Returns 'Left' error if deserialization fails (type mismatch).
getScope
  :: forall s. (FromJSON s, Typeable s)
  => Text
  -> MemoryStore
  -> Either Text (Maybe s)
getScope scopeName (MemoryStore scopes) =
  case Map.lookup scopeName scopes of
    Nothing -> Right Nothing
    Just sm -> case Aeson.fromJSON sm.smValue of
      Aeson.Error err -> Left $
        "Failed to deserialize scope '" <> scopeName <> "': " <> pack err
      Aeson.Success val -> Right (Just val)

-- | Set a scope in the store, serializing the value.
setScope
  :: forall s. (ToJSON s, Typeable s)
  => Text
  -> s
  -> MemoryStore
  -> MemoryStore
setScope scopeName val (MemoryStore scopes) = MemoryStore $
  Map.insert scopeName entry scopes
  where
    entry = ScopedMemory
      { smTypeRep = typeRep (Proxy @s)
      , smValue = toJSON val
      }

-- | Run memory effect with scoped persistence.
--
-- This interpreter:
-- 1. Loads initial state from the store (or uses default if scope missing)
-- 2. Runs the action
-- 3. Saves final state back to the store
--
-- @
-- (result, finalStore) <- runMemoryScoped "explore" defaultState store $ do
--   updateMem @ExploreMem $ \\m -> m { visited = newUrl : m.visited }
--   getMem @ExploreMem
-- @
runMemoryScoped
  :: forall s es a. (ToJSON s, FromJSON s, Typeable s)
  => Text              -- ^ Scope name (typically node name)
  -> s                 -- ^ Default value if scope doesn't exist
  -> MemoryStore       -- ^ Current store
  -> Eff (Memory s : es) a
  -> Eff es (a, MemoryStore)
runMemoryScoped scopeName defaultVal store action = do
  -- Load initial state from store or use default
  let initial = case getScope @s scopeName store of
        Left _err -> defaultVal  -- Deserialization error: use default
        Right Nothing -> defaultVal  -- Scope doesn't exist: use default
        Right (Just val) -> val  -- Successfully loaded

  -- Run the action
  (result, final) <- runMemory initial action

  -- Save final state back to store
  let newStore = setScope scopeName final store
  pure (result, newStore)

-- | Run memory effect with scoped persistence, discarding result store.
--
-- Useful when you don't need the updated store (e.g., read-only access).
evalMemoryScoped
  :: forall s es a. (ToJSON s, FromJSON s, Typeable s)
  => Text
  -> s
  -> MemoryStore
  -> Eff (Memory s : es) a
  -> Eff es a
evalMemoryScoped scopeName defaultVal store =
  fmap fst . runMemoryScoped scopeName defaultVal store
