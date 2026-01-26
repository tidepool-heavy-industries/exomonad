{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | ForkNode execution support for the actor runtime.
--
-- This module provides the handler wrapper for ForkNode execution. A ForkNode
-- receives input, produces an HList of payloads for each spawned worker, and
-- routes them to the corresponding actors.
--
-- = Design
--
-- ForkNode doesn't spawn new actors - the workers already exist. It just routes
-- payloads to them:
--
-- @
-- ForkNode handler: Input -> HList '[TestsCtx, ImplCtx]
--
-- 1. Receive input
-- 2. Build HList of worker payloads
-- 3. Extract (targetName, jsonPayload) pairs via SpawnTargets
-- 4. Route each payload to its target actor
-- @
--
-- The BarrierNode collects results from workers via 'Arrive' messages.
module ExoMonad.Actor.Fork
  ( -- * SpawnTargets Typeclass
    SpawnTargets(..)

    -- * Handler Wrapper
  , forkHandler
  ) where

import Control.Monad (forM_)
import Control.Monad.Freer (Eff, runM)
import Data.Aeson (Value, ToJSON(..), FromJSON(..))
import Data.Aeson.Types (parseEither)
import Data.Kind (Type, Constraint)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import System.IO (hFlush, stdout)

import ExoMonad.Graph.Goto (To)
import ExoMonad.Graph.Types (HList(..))

import ExoMonad.Actor.Graph (Router, NodeHandler(..))


-- ════════════════════════════════════════════════════════════════════════════
-- SPAWN TARGETS TYPECLASS
-- ════════════════════════════════════════════════════════════════════════════

-- | Extract (targetName, payload) pairs from an HList based on Spawn targets.
--
-- This typeclass enables type-safe extraction of worker payloads from a
-- ForkNode handler's return value. The type-level target list specifies
-- which actors receive which payloads.
--
-- @
-- -- Given targets: '[To "hTests" TestsCtx, To "hImpl" ImplCtx]
-- -- And HList: TestsCtx ::: ImplCtx ::: HNil
-- -- Produces: [("hTests", toJSON testsCtx), ("hImpl", toJSON implCtx)]
-- @
type SpawnTargets :: [Type] -> [Type] -> Constraint
class SpawnTargets targets payloads | targets -> payloads where
  -- | Extract (targetName, jsonPayload) pairs from an HList.
  extractSpawnTargets :: HList payloads -> [(Text, Value)]


-- | Base case: empty target list.
instance SpawnTargets '[] '[] where
  extractSpawnTargets HNil = []


-- | Recursive case: extract target name and payload.
instance
  ( KnownSymbol name
  , ToJSON payload
  , SpawnTargets rest restPayloads
  ) => SpawnTargets (To name payload ': rest) (payload ': restPayloads) where
  extractSpawnTargets (payload ::: restPayloads) =
    let targetName = T.pack (symbolVal (Proxy @name))
        jsonPayload = toJSON payload
        restTargets = extractSpawnTargets @rest @restPayloads restPayloads
    in (targetName, jsonPayload) : restTargets


-- ════════════════════════════════════════════════════════════════════════════
-- FORK HANDLER WRAPPER
-- ════════════════════════════════════════════════════════════════════════════

-- | Wrap a ForkNode handler as a 'NodeHandler'.
--
-- The handler receives input and returns an HList of payloads for each worker.
-- The wrapper extracts target/payload pairs and routes them to workers.
--
-- @
-- -- Define fork handler
-- forkH :: GatedState -> Eff es (HList '[TestsTemplateCtx, ImplTemplateCtx])
-- forkH gatedState = do
--   let testsCtx = buildTestsContext gatedState
--       implCtx = buildImplContext gatedState
--   pure $ testsCtx ::: implCtx ::: HNil
--
-- -- Build node handler
-- handler = forkHandler
--   \@'[To "hTests" TestsTemplateCtx, To "hImpl" ImplTemplateCtx]
--   (runM . runEffects)  -- interpreter
--   forkH
-- @
--
-- Note: ForkNode doesn't spawn actors - it routes to existing actors.
-- The workers must already exist in the actor system.
forkHandler
  :: forall targets payloads input es.
     ( SpawnTargets targets payloads
     , FromJSON input
     )
  => (forall a. Eff es a -> IO a)         -- ^ Effect interpreter
  -> (input -> Eff es (HList payloads))   -- ^ Fork handler
  -> NodeHandler
forkHandler interpret handler = NodeHandler $ \router jsonPayload ->
  case parseEither parseJSON jsonPayload of
    Left err -> error $ "ForkNode: Failed to parse input: " <> err
    Right input -> do
      -- Run handler to get HList of payloads
      hlist <- interpret (handler input)
      -- Extract targets and route to each worker
      let targets = extractSpawnTargets @targets @payloads hlist
      putStrLn $ "[FORK] Dispatching to: " <> show (map fst targets)
      hFlush stdout
      forM_ targets $ \(target, payload) ->
        router target payload
