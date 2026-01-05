{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- Pattern exhaustiveness checker doesn't understand GADT constraints for OneOf

-- | Instrumented graph executor with OpenTelemetry tracing.
--
-- Provides traced versions of graph execution functions that emit spans
-- for each node transition, enabling distributed tracing through Grafana Tempo.
--
-- = Usage
--
-- @
-- import Tidepool.Graph.Execute.Instrumented
--
-- -- Run graph with automatic span emission
-- result <- runGraphWithSpans handlers entryValue
--
-- -- Manually flush traces after the graph completes
-- flushTraces otlpConfig "my-service" traceContext
-- @
--
-- = Span Structure
--
-- Each graph execution creates a span hierarchy:
--
-- @
-- graph:MyGraph (root span)
-- └── node:entry (first handler)
-- └── node:process
-- └── node:exit
-- @
--
-- Node spans include attributes:
--
-- * @node.name@ - The handler field name
--
-- = Limitations
--
-- Exception safety: If a handler throws an exception, the current span
-- will not be closed. For production use, wrap graph execution in
-- exception-safe bracketing at the runner level.
module Tidepool.Graph.Execute.Instrumented
  ( -- * Traced Graph Execution
    runGraphWithSpans
  , runGraphFromWithSpans
  , withGraphSpan

    -- * Traced Dispatch
  , DispatchGotoTraced(..)

    -- * Re-exports from Execute
  , DispatchGoto(..)
  , CallHandler(..)
  , executeLLMHandler
  ) where

import Data.Kind (Constraint, Type)
import Control.Monad.Freer (Eff, Member)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic(..))
import GHC.Records (HasField(..))
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.Proxy (Proxy(..))

import Tidepool.Effects.Observability
  ( Observability, SpanKind(..), SpanAttribute(..)
  , startSpan, endSpan
  )
import Tidepool.Graph.Execute
  ( DispatchGoto(..), CallHandler(..), executeLLMHandler
  , FindEntryHandler
  )
import Tidepool.Graph.Generic (AsHandler, FieldsWithNamesOf)
import Tidepool.Graph.Generic.Core (AsGraph)
import Tidepool.Graph.Goto (GotoChoice, To)
import Tidepool.Graph.Goto.Internal (GotoChoice(..), OneOf(..))
import Tidepool.Graph.Types (Exit)


-- | Effect type alias.
type Effect = Type -> Type


-- ════════════════════════════════════════════════════════════════════════════
-- TRACED GRAPH EXECUTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Run a graph with automatic span emission for each node transition.
--
-- Creates a root span for the graph and child spans for each handler invocation.
--
-- @
-- result <- runGraphWithSpans @MyGraph handlers inputValue
-- @
runGraphWithSpans
  :: forall graph entryType targets exitType es entryHandlerName handler.
     ( Generic (graph AsGraph)
     , FindEntryHandler entryType (FieldsWithNamesOf graph) ~ 'Just entryHandlerName
     , KnownSymbol entryHandlerName
     , HasField entryHandlerName (graph (AsHandler es)) handler
     , CallHandler handler entryType es targets
     , DispatchGotoTraced graph targets es exitType
     , Member Observability es
     )
  => graph (AsHandler es)
  -> entryType
  -> Eff es exitType
runGraphWithSpans = runGraphFromWithSpans @entryHandlerName


-- | Run a graph from a named handler with span emission.
--
-- @
-- result <- runGraphFromWithSpans @"compute" handlers inputValue
-- @
runGraphFromWithSpans
  :: forall (name :: Symbol) graph entryType targets exitType es handler.
     ( KnownSymbol name
     , HasField name (graph (AsHandler es)) handler
     , CallHandler handler entryType es targets
     , DispatchGotoTraced graph targets es exitType
     , Member Observability es
     )
  => graph (AsHandler es)
  -> entryType
  -> Eff es exitType
runGraphFromWithSpans graph input = do
  let handlerName = T.pack $ symbolVal (Proxy @name)

  -- Start span for this node
  _ <- startSpan ("node:" <> handlerName) SpanInternal
    [AttrText "node.name" handlerName]

  -- Call the handler
  let handler = getField @name graph
  choice <- callHandler handler input

  -- End the node span
  endSpan False []

  -- Dispatch to next node (traced)
  dispatchGotoTraced graph choice


-- ════════════════════════════════════════════════════════════════════════════
-- TRACED DISPATCH TYPECLASS
-- ════════════════════════════════════════════════════════════════════════════

-- | Dispatch with span emission for each node transition.
--
-- Like 'DispatchGoto', but emits OpenTelemetry spans for observability.
type DispatchGotoTraced :: (Type -> Type) -> [Type] -> [Effect] -> Type -> Constraint
class DispatchGotoTraced graph targets es exitType where
  dispatchGotoTraced :: graph (AsHandler es) -> GotoChoice targets -> Eff es exitType


-- ════════════════════════════════════════════════════════════════════════════
-- EXIT INSTANCES
-- ════════════════════════════════════════════════════════════════════════════

-- | Base case: Exit is the only target.
instance DispatchGotoTraced graph '[To Exit exitType] es exitType where
  dispatchGotoTraced _ (GotoChoice (Here result)) = pure result

-- | Exit is first, but there are more targets.
instance
  ( DispatchGotoTraced graph rest es exitType
  , Member Observability es
  ) => DispatchGotoTraced graph (To Exit exitType ': rest) es exitType where
  dispatchGotoTraced _ (GotoChoice (Here result)) = pure result
  dispatchGotoTraced graph (GotoChoice (There rest)) =
    dispatchGotoTraced @graph @rest graph (GotoChoice rest)


-- ════════════════════════════════════════════════════════════════════════════
-- NAMED NODE INSTANCES
-- ════════════════════════════════════════════════════════════════════════════

-- | Single named node target with tracing.
instance
  ( KnownSymbol name
  , HasField name (graph (AsHandler es)) handler
  , CallHandler handler payload es handlerTargets
  , DispatchGotoTraced graph handlerTargets es exitType
  , Member Observability es
  ) => DispatchGotoTraced graph '[To (name :: Symbol) payload] es exitType where

  dispatchGotoTraced graph (GotoChoice (Here payload)) = do
    let handlerName = T.pack $ symbolVal (Proxy @name)

    -- Start span for this node
    _ <- startSpan ("node:" <> handlerName) SpanInternal
      [AttrText "node.name" handlerName]

    -- Call handler
    let handler = getField @name graph
    nextChoice <- callHandler handler payload

    -- End the node span
    endSpan False []

    -- Continue dispatch
    dispatchGotoTraced graph nextChoice


-- | Named node target with more targets, with tracing.
instance {-# OVERLAPPABLE #-}
  ( KnownSymbol name
  , HasField name (graph (AsHandler es)) handler
  , CallHandler handler payload es handlerTargets
  , DispatchGotoTraced graph handlerTargets es exitType
  , DispatchGotoTraced graph rest es exitType
  , Member Observability es
  ) => DispatchGotoTraced graph (To (name :: Symbol) payload ': rest) es exitType where

  dispatchGotoTraced graph (GotoChoice (Here payload)) = do
    let handlerName = T.pack $ symbolVal (Proxy @name)

    -- Start span for this node
    _ <- startSpan ("node:" <> handlerName) SpanInternal
      [AttrText "node.name" handlerName]

    -- Call handler
    let handler = getField @name graph
    nextChoice <- callHandler handler payload

    -- End the node span
    endSpan False []

    -- Continue dispatch
    dispatchGotoTraced graph nextChoice

  dispatchGotoTraced graph (GotoChoice (There rest)) =
    dispatchGotoTraced @graph @rest graph (GotoChoice rest)


-- ════════════════════════════════════════════════════════════════════════════
-- ROOT SPAN WRAPPER
-- ════════════════════════════════════════════════════════════════════════════

-- | Wrap a graph execution with a root span.
--
-- Creates a server span that contains all node spans as children.
-- Use this when you want explicit control over the root span name.
--
-- @
-- result <- withGraphSpan "my-agent:handle-request" $ do
--   runGraphWithSpans handlers input
-- @
withGraphSpan
  :: Member Observability es
  => Text           -- ^ Span name (e.g., "graph:MyGraph" or "agent:handle-request")
  -> Eff es a       -- ^ Action to run
  -> Eff es a
withGraphSpan name action = do
  _ <- startSpan name SpanServer []
  result <- action
  endSpan False []
  pure result
