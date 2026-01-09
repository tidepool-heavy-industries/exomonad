{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- Pattern exhaustiveness checker doesn't understand GADT constraints for OneOf

-- | Graph-to-Actor Execution Layer
--
-- This module bridges the typed graph DSL to the actor runtime. Each graph node
-- becomes one actor, and transitions become messages between actors.
--
-- = Design
--
-- The graph DSL already has the right shape for actors:
--
-- * Nodes are independent units (receive input, produce output)
-- * Transitions are explicit (@Goto "target" payload@)
-- * No shared state between nodes (Memory is per-node)
--
-- = Type Erasure Strategy
--
-- Handlers operate in @Eff es@ with typed payloads. Actors communicate via
-- JSON @Value@. We bridge at actor boundaries:
--
-- @
-- Actor receives Value → fromJSON to typed → run handler in Eff → GotoChoice
--                                                                      ↓
--                                           ExtractChoice → (target, toJSON payload)
--                                                                      ↓
--                                                              router target payload
-- @
--
-- Type safety is enforced at compile time via graph constraints. JSON is the
-- wire format between actors.
module Tidepool.Actor.Graph
  ( -- * Graph Execution
    runGraphAsActors
  , HandlerBuilder

    -- * Choice Extraction
  , ExtractChoice(..)

    -- * Handler Builders
  , Router
  , NodeHandler(..)
  , pureHandler
  , ioHandler
  , effHandler
  , effHandlerWithMem

    -- * Low-level Wrappers (for custom interpreters)
  , wrapPureHandler
  , wrapIOHandler
  , wrapEffHandler

    -- * Memory Effect
  , Memory(..)
  , getMem
  , updateMem
  , runMemoryWithIORef

    -- * Types
  , ActorId

    -- * Re-exports
  , GotoChoice
  , To
  , Exit
  , gotoChoice
  , gotoExit
  , Eff
  , Member
  , LastMember
  , runM
  ) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad.Freer (Eff, Member, interpret, sendM, LastMember, runM)
import qualified Control.Monad.Freer as Freer
import Data.Aeson (Value, ToJSON(..), FromJSON(..))
import Data.Aeson.Types (parseEither)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import Data.Kind (Type, Constraint)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import qualified Ki

import Tidepool.Graph.Goto (GotoChoice, To, gotoChoice, gotoExit)
import Tidepool.Graph.Goto.Internal (GotoChoice(..), OneOf(..))
import Tidepool.Graph.Types (Exit, Self, Arrive)

import Tidepool.Actor.Types (ActorId, Actor(..))
import Tidepool.Actor.Mailbox (send)
import Tidepool.Actor.Spawn (spawnActor)


-- ════════════════════════════════════════════════════════════════════════════
-- HANDLER BRIDGE
-- ════════════════════════════════════════════════════════════════════════════

-- | Router function for sending messages to other actors.
--
-- Takes target actor name and JSON payload.
type Router = Text -> Value -> IO ()


-- | A node handler wrapped for actor execution.
--
-- This is the unified type for actor handlers - it takes a JSON payload
-- and a router, and performs its action (potentially sending to other actors).
newtype NodeHandler = NodeHandler
  { runNodeHandler :: Router -> Value -> IO ()
  }


-- | Wrap a pure handler as a 'NodeHandler'.
--
-- The handler receives typed input and returns a 'GotoChoice'. The wrapper:
--
-- 1. Parses JSON input to the expected type
-- 2. Calls the pure handler
-- 3. Extracts target and payload from 'GotoChoice'
-- 4. Routes to the next actor
--
-- @
-- myHandler :: Intent -> GotoChoice '[To \"process\" Data, To Exit Response]
-- myHandler intent = case intent of
--   NeedsProcess d -> gotoChoice \@\"process\" d
--   Done r -> gotoExit r
--
-- nodeHandler = wrapPureHandler myHandler
-- @
wrapPureHandler
  :: forall payload targets.
     ( FromJSON payload
     , ExtractChoice targets
     )
  => (payload -> GotoChoice targets)
  -> NodeHandler
wrapPureHandler handler = NodeHandler $ \router jsonPayload ->
  case parseEither parseJSON jsonPayload of
    Left err -> error $ "Failed to parse payload: " <> err
    Right payload -> do
      let choice = handler payload
          (target, nextPayload) = extractChoice choice
      router target nextPayload


-- | Wrap an IO handler as a 'NodeHandler'.
--
-- Like 'wrapPureHandler' but for handlers that need IO (e.g., for logging,
-- external API calls, etc.).
--
-- @
-- myIOHandler :: Intent -> IO (GotoChoice '[To \"process\" Data, To Exit Response])
-- myIOHandler intent = do
--   putStrLn \"Processing intent...\"
--   pure $ case intent of
--     NeedsProcess d -> gotoChoice \@\"process\" d
--     Done r -> gotoExit r
--
-- nodeHandler = wrapIOHandler myIOHandler
-- @
wrapIOHandler
  :: forall payload targets.
     ( FromJSON payload
     , ExtractChoice targets
     )
  => (payload -> IO (GotoChoice targets))
  -> NodeHandler
wrapIOHandler handler = NodeHandler $ \router jsonPayload ->
  case parseEither parseJSON jsonPayload of
    Left err -> error $ "Failed to parse payload: " <> err
    Right payload -> do
      choice <- handler payload
      let (target, nextPayload) = extractChoice choice
      router target nextPayload


-- | Wrap an effectful handler as a 'NodeHandler'.
--
-- This is the primary way to wrap handlers that use freer-simple effects.
-- The caller provides an interpreter that runs their effect stack to IO.
--
-- @
-- -- Define handler with effects
-- myHandler :: Member Log effs => Message -> Eff effs (GotoChoice '[To Exit Response])
-- myHandler msg = do
--   logInfo "Processing message"
--   pure $ gotoExit (Response $ "processed: " <> msg.content)
--
-- -- Provide interpreter when wrapping
-- nodeHandler = wrapEffHandler
--   (runM . runLog Info)  -- interpreter: Eff effs a -> IO a
--   myHandler
-- @
wrapEffHandler
  :: forall payload targets es.
     ( FromJSON payload
     , ExtractChoice targets
     )
  => (forall a. Eff es a -> IO a)             -- ^ Interpreter: run effects to IO
  -> (payload -> Eff es (GotoChoice targets)) -- ^ Effectful handler
  -> NodeHandler
wrapEffHandler runEffects handler = NodeHandler $ \router jsonPayload ->
  case parseEither parseJSON jsonPayload of
    Left err -> error $ "Failed to parse payload: " <> err
    Right payload -> do
      choice <- runEffects (handler payload)
      let (target, nextPayload) = extractChoice choice
      router target nextPayload


-- ════════════════════════════════════════════════════════════════════════════
-- MEMORY EFFECT
-- ════════════════════════════════════════════════════════════════════════════

-- | Per-actor memory effect.
--
-- Each actor can have its own persistent state via an IORef. This allows
-- actors to maintain state across message processing without shared mutable
-- state between actors.
data Memory s r where
  GetMem    :: Memory s s
  UpdateMem :: (s -> s) -> Memory s ()

-- | Read the current memory state.
getMem :: forall s es. Member (Memory s) es => Eff es s
getMem = Freer.send GetMem

-- | Update the memory state.
updateMem :: forall s es. Member (Memory s) es => (s -> s) -> Eff es ()
updateMem = Freer.send . UpdateMem

-- | Run the Memory effect using an IORef.
--
-- Each actor should have its own IORef, providing isolated persistent state.
--
-- @
-- memRef <- newIORef initialState
-- let interpreter = runM . runMemoryWithIORef memRef . runOtherEffects
-- @
runMemoryWithIORef
  :: forall s effs a.
     LastMember IO effs
  => IORef s
  -> Eff (Memory s ': effs) a
  -> Eff effs a
runMemoryWithIORef ref = interpret $ \case
  GetMem      -> sendM $ readIORef ref
  UpdateMem f -> sendM $ modifyIORef' ref f


-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH EXECUTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Builder for creating a handler.
--
-- Each builder is an IO action that creates a NodeHandler. This allows
-- handlers to initialize their own state (e.g., IORef for memory) when
-- the actor is spawned. Each actor runs its builder once at startup.
type HandlerBuilder = IO NodeHandler


-- | Run a graph as an actor system.
--
-- Each entry in the handler map becomes one actor. Each handler builder
-- is run once when its actor spawns, allowing per-actor state initialization.
-- Messages flow between actors via the router. The system terminates when
-- a handler routes to "exit", returning the exit payload.
--
-- = Node Names
--
-- * @"entry"@ - Entry point (receives initial input)
-- * @"exit"@ - Exit handler (returns result)
-- * Other names map directly to graph node names
--
-- = Example
--
-- @
-- handlers :: Map Text HandlerBuilder
-- handlers = Map.fromList
--   [ (\"entry\", pureHandler entryHandler)
--   , (\"processor\", effHandlerWithMem 0 processorHandler)  -- has its own Int memory
--   , (\"logger\", effHandlerWithMem [] logHandler)          -- has its own [Text] memory
--   ]
--
-- result <- runGraphAsActors handlers (toJSON (Message \"hello\"))
-- @
runGraphAsActors
  :: forall result.
     FromJSON result
  => Map Text HandlerBuilder  -- ^ Map of node names to handler builders
  -> Value                    -- ^ Initial input (sent to "entry")
  -> IO result
runGraphAsActors handlerBuilders entryPayload = do
  -- MVar for exit result
  exitChan <- newEmptyMVar

  -- IORef for router (chicken-and-egg: router needs actors, actors need router)
  routerRef <- newIORef (\_ _ -> error "Router not yet initialized")

  -- IORef for actors map
  actorsRef <- newIORef Map.empty

  Ki.scoped $ \scope -> do
    -- Build the real router
    let router :: Router
        router target payload
          | target == "exit" = do
              -- Exit: put result in MVar
              case parseEither parseJSON payload of
                Left err -> error $ "Failed to parse exit payload: " <> err
                Right result -> putMVar exitChan result
          | otherwise = do
              -- Normal dispatch: look up actor and send
              actors <- readIORef actorsRef
              case Map.lookup target actors of
                Nothing -> error $ "Unknown actor target: " <> T.unpack target
                Just actor -> send (actorMailbox actor) payload

    -- Spawn one actor per handler builder
    actors <- flip Map.traverseWithKey handlerBuilders $ \name mkHandler -> do
      (NodeHandler handler) <- mkHandler  -- Run builder to create handler
      spawnActor scope name $ \payload -> do
        r <- readIORef routerRef
        handler r payload

    -- Store actors and initialize router
    writeIORef actorsRef actors
    writeIORef routerRef router

    -- Send initial message to entry
    case Map.lookup "entry" actors of
      Nothing -> error "No 'entry' handler in graph"
      Just entryActor -> send (actorMailbox entryActor) entryPayload

    -- Wait for exit
    takeMVar exitChan


-- ════════════════════════════════════════════════════════════════════════════
-- HANDLER BUILDERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Build a handler from a pure function (no effects, no memory).
pureHandler
  :: (FromJSON payload, ExtractChoice targets)
  => (payload -> GotoChoice targets)
  -> HandlerBuilder
pureHandler h = pure $ wrapPureHandler h


-- | Build a handler from an IO function (IO effects, no memory).
ioHandler
  :: (FromJSON payload, ExtractChoice targets)
  => (payload -> IO (GotoChoice targets))
  -> HandlerBuilder
ioHandler h = pure $ wrapIOHandler h


-- | Build a handler with a custom effect interpreter (no memory).
effHandler
  :: (FromJSON payload, ExtractChoice targets)
  => (forall a. Eff es a -> IO a)
  -> (payload -> Eff es (GotoChoice targets))
  -> HandlerBuilder
effHandler interpreter h = pure $ wrapEffHandler interpreter h


-- | Build a handler with per-actor memory.
--
-- Creates an IORef for the actor's memory, initialized with the given value.
-- Each actor gets its own isolated memory that persists across messages.
--
-- @
-- counterHandler :: Message -> Eff '[Memory Int, IO] (GotoChoice '[To Exit Int])
-- counterHandler _ = do
--   updateMem (+ 1)
--   getMem >>= pure . gotoExit
--
-- handlers = Map.fromList
--   [ (\"counter\", effHandlerWithMem 0 counterHandler)
--   ]
-- @
effHandlerWithMem
  :: (FromJSON payload, ExtractChoice targets)
  => mem  -- ^ Initial memory state
  -> (payload -> Eff '[Memory mem, IO] (GotoChoice targets))
  -> HandlerBuilder
effHandlerWithMem initial handler = do
  memRef <- newIORef initial
  pure $ wrapEffHandler (runM . runMemoryWithIORef memRef) handler


-- ════════════════════════════════════════════════════════════════════════════
-- EXTRACT CHOICE TYPECLASS
-- ════════════════════════════════════════════════════════════════════════════

-- | Extract target name and JSON payload from a 'GotoChoice'.
--
-- This typeclass enables runtime dispatch by converting the typed sum
-- to an untyped (target name, payload value) pair suitable for actor routing.
--
-- = How It Works
--
-- @
-- GotoChoice '[To \"nodeA\" Int, To \"nodeB\" String, To Exit Bool]
--   ↓ extractChoice
-- (\"nodeA\", Number 42)      -- if Here payload
-- (\"nodeB\", String \"hi\")  -- if There (Here payload)
-- (\"Exit\", Bool True)       -- if There (There (Here payload))
-- @
--
-- The typeclass recursively matches on @OneOf@ positions to determine
-- which target was selected and serializes the payload to JSON.
--
-- = Special Targets
--
-- * @\"Exit\"@ - Graph termination
-- * @\"Self\"@ - Self-loop (re-invoke same handler)
-- * @\"Arrive\"@ - Worker completion at barrier
type ExtractChoice :: [Type] -> Constraint
class ExtractChoice targets where
  extractChoice :: GotoChoice targets -> (Text, Value)


-- | Base case: single named target.
instance {-# OVERLAPPING #-}
  ( KnownSymbol name
  , ToJSON payload
  ) => ExtractChoice '[To (name :: Symbol) payload] where
  extractChoice (GotoChoice (Here payload)) =
    (T.pack (symbolVal (Proxy @name)), toJSON payload)


-- | Named target with more targets following.
instance {-# OVERLAPPABLE #-}
  ( KnownSymbol name
  , ToJSON payload
  , ExtractChoice rest
  ) => ExtractChoice (To (name :: Symbol) payload ': rest) where
  extractChoice (GotoChoice (Here payload)) =
    (T.pack (symbolVal (Proxy @name)), toJSON payload)
  extractChoice (GotoChoice (There rest)) =
    extractChoice @rest (GotoChoice rest)


-- | Exit target only.
instance {-# OVERLAPPING #-}
  ToJSON payload => ExtractChoice '[To Exit payload] where
  extractChoice (GotoChoice (Here payload)) =
    ("exit", toJSON payload)


-- | Exit target with more targets following.
instance {-# OVERLAPPABLE #-}
  ( ToJSON payload
  , ExtractChoice rest
  ) => ExtractChoice (To Exit payload ': rest) where
  extractChoice (GotoChoice (Here payload)) =
    ("exit", toJSON payload)
  extractChoice (GotoChoice (There rest)) =
    extractChoice @rest (GotoChoice rest)


-- | Self target only.
instance {-# OVERLAPPING #-}
  ToJSON payload => ExtractChoice '[To Self payload] where
  extractChoice (GotoChoice (Here payload)) =
    ("self", toJSON payload)


-- | Self target with more targets following.
instance {-# OVERLAPPABLE #-}
  ( ToJSON payload
  , ExtractChoice rest
  ) => ExtractChoice (To Self payload ': rest) where
  extractChoice (GotoChoice (Here payload)) =
    ("self", toJSON payload)
  extractChoice (GotoChoice (There rest)) =
    extractChoice @rest (GotoChoice rest)


-- | Arrive target only (for ForkNode workers).
instance {-# OVERLAPPING #-}
  ToJSON payload => ExtractChoice '[To Arrive payload] where
  extractChoice (GotoChoice (Here payload)) =
    ("arrive", toJSON payload)


-- | Arrive target with more targets following.
instance {-# OVERLAPPABLE #-}
  ( ToJSON payload
  , ExtractChoice rest
  ) => ExtractChoice (To Arrive payload ': rest) where
  extractChoice (GotoChoice (Here payload)) =
    ("arrive", toJSON payload)
  extractChoice (GotoChoice (There rest)) =
    extractChoice @rest (GotoChoice rest)
