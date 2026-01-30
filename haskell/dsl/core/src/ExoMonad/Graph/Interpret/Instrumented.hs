{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-redundant-constraints #-}

-- Pattern exhaustiveness checker doesn't understand GADT constraints for OneOf
-- Redundant constraints are documentation for type-level dispatch

-- | Instrumented graph interpreter with OpenTelemetry tracing.
--
-- Provides traced versions of graph interpretation functions that emit spans
-- for each node transition, enabling distributed tracing through Grafana Tempo.
--
-- = Usage
--
-- @
-- import ExoMonad.Graph.Interpret.Instrumented
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
module ExoMonad.Graph.Interpret.Instrumented
  ( -- * Traced Graph Interpretation
    runGraphWithSpans,
    runGraphFromWithSpans,
    withGraphSpan,
    withNodeSpan,

    -- * Traced Dispatch
    DispatchGotoTraced (..),

    -- * Re-exports from Execute
    DispatchGoto (..),
    CallHandler (..),
    executeLLMHandler,
  )
where

import Control.Monad.Freer (Eff, Member)
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Effects.Observability
  ( Observability,
    SpanAttribute (..),
    SpanKind (..),
    endSpan,
    startSpan,
  )
import ExoMonad.Graph.Generic (AsHandler, FieldsWithNamesOf)
import ExoMonad.Graph.Generic.Core (AsGraph)
import ExoMonad.Graph.Goto (GotoChoice, To)
import ExoMonad.Graph.Goto.Internal (GotoChoice (..), OneOf (..))
import ExoMonad.Graph.Interpret
import ExoMonad.Graph.Types (Exit)
import GHC.Generics (Generic (..))
import GHC.Records (HasField (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

-- | Effect type alias.
type Effect = Type -> Type

-- ════════════════════════════════════════════════════════════════════════════
-- NODE SPAN HELPER
-- ════════════════════════════════════════════════════════════════════════════

-- | Wrap a handler call with span emission for a named node.
--
-- This extracts the common pattern of:
-- 1. Start span for node
-- 2. Execute action
-- 3. End span
--
-- Used by 'DispatchGotoTraced' instances to reduce duplication.
-- The span name follows the pattern @node:<handler-name>@.
withNodeSpan ::
  (Member Observability es) =>
  -- | Handler name
  Text ->
  -- | Action to run (handler call + continuation)
  Eff es a ->
  Eff es a
withNodeSpan handlerName action = do
  _ <-
    startSpan
      ("node:" <> handlerName)
      SpanInternal
      [AttrText "node.name" handlerName]
  result <- action
  endSpan False []
  pure result

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
runGraphWithSpans ::
  forall graph entryType targets exitType es entryHandlerName handler.
  ( Generic (graph AsGraph),
    FindEntryHandler entryType (FieldsWithNamesOf graph) ~ 'Just entryHandlerName,
    KnownSymbol entryHandlerName,
    HasField entryHandlerName (graph (AsHandler es)) handler,
    CallHandler handler entryType es targets,
    DispatchGotoTraced graph targets es exitType,
    Member Observability es
  ) =>
  graph (AsHandler es) ->
  entryType ->
  Eff es exitType
runGraphWithSpans = runGraphFromWithSpans @entryHandlerName

-- | Run a graph from a named handler with span emission.
--
-- @
-- result <- runGraphFromWithSpans @"compute" handlers inputValue
-- @
runGraphFromWithSpans ::
  forall (name :: Symbol) graph entryType targets exitType es handler.
  ( KnownSymbol name,
    HasField name (graph (AsHandler es)) handler,
    CallHandler handler entryType es targets,
    DispatchGotoTraced graph targets es exitType,
    Member Observability es
  ) =>
  graph (AsHandler es) ->
  entryType ->
  Eff es exitType
runGraphFromWithSpans graph input = do
  let handlerName = T.pack $ symbolVal (Proxy @name)
  withNodeSpan handlerName $ do
    let handler = getField @name graph
    choice <- callHandler handler input
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
  ( DispatchGotoTraced graph rest es exitType,
    Member Observability es
  ) =>
  DispatchGotoTraced graph (To Exit exitType ': rest) es exitType
  where
  dispatchGotoTraced _ (GotoChoice (Here result)) = pure result
  dispatchGotoTraced graph (GotoChoice (There rest)) =
    dispatchGotoTraced @graph @rest graph (GotoChoice rest)

-- ════════════════════════════════════════════════════════════════════════════
-- NAMED NODE INSTANCES
-- ════════════════════════════════════════════════════════════════════════════

-- | Single named node target with tracing.
instance
  ( KnownSymbol name,
    HasField name (graph (AsHandler es)) handler,
    CallHandler handler payload es handlerTargets,
    DispatchGotoTraced graph handlerTargets es exitType,
    Member Observability es
  ) =>
  DispatchGotoTraced graph '[To (name :: Symbol) payload] es exitType
  where
  dispatchGotoTraced graph (GotoChoice (Here payload)) =
    withNodeSpan (T.pack $ symbolVal (Proxy @name)) $ do
      let handler = getField @name graph
      nextChoice <- callHandler handler payload
      dispatchGotoTraced graph nextChoice

-- | Named node target with more targets, with tracing.
instance
  {-# OVERLAPPABLE #-}
  ( KnownSymbol name,
    HasField name (graph (AsHandler es)) handler,
    CallHandler handler payload es handlerTargets,
    DispatchGotoTraced graph handlerTargets es exitType,
    DispatchGotoTraced graph rest es exitType,
    Member Observability es
  ) =>
  DispatchGotoTraced graph (To (name :: Symbol) payload ': rest) es exitType
  where
  dispatchGotoTraced graph (GotoChoice (Here payload)) =
    withNodeSpan (T.pack $ symbolVal (Proxy @name)) $ do
      let handler = getField @name graph
      nextChoice <- callHandler handler payload
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
withGraphSpan ::
  (Member Observability es) =>
  -- | Span name (e.g., "graph:MyGraph" or "agent:handle-request")
  Text ->
  -- | Action to run
  Eff es a ->
  Eff es a
withGraphSpan name action = do
  _ <- startSpan name SpanServer []
  result <- action
  endSpan False []
  pure result
