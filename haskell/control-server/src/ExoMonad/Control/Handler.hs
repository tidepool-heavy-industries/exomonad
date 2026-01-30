{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Message routing for control server.
module ExoMonad.Control.Handler
  ( handleMessage,
  )
where

import Control.Concurrent.STM (TVar)
import Control.Exception (SomeException, displayException, throwIO, try)
import Control.Monad (when)
import Control.Monad.Freer (runM, sendM)
import Data.Aeson (encode)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text qualified as T
import ExoMonad.Control.Export (exportMCPTools)
import ExoMonad.Control.Handler.Hook (handleHook)
import ExoMonad.Control.Hook.CircuitBreaker (CircuitBreakerMap)
import ExoMonad.Control.Logging (Logger, logError, logInfo)
import ExoMonad.Control.Protocol
-- Tracing Imports

-- New Role DSL Imports

import ExoMonad.Control.Role.Definition.Dev (DevRole (..))
import ExoMonad.Control.Role.Definition.PM (PMRole (..))
import ExoMonad.Control.Role.Definition.TL (TLRole (..))
import ExoMonad.Control.Role.Handlers (devHandlers, pmHandlers, tlHandlers)
import ExoMonad.Control.Role.Registry (allToolsForRole)
import ExoMonad.Control.Role.Tool.Dispatch (DispatchResult (..), dispatchTool)
import ExoMonad.Control.RoleConfig (roleFromText)
import ExoMonad.Control.Runtime (AppEffects, runApp)
import ExoMonad.Control.Types (ServerConfig (..))
import ExoMonad.Effects.Observability (SpanAttribute (..), SpanKind (..), addSpanAttribute, withSpan)
import ExoMonad.Observability.Interpreter (runObservabilityWithContext)
import ExoMonad.Observability.Types (ObservabilityConfig (..), TraceContext, defaultLokiConfig)
import ExoMonad.Role (Role (..))
import OpenTelemetry.Trace (Tracer)
import PyF (fmt)

-- | Route a control message to the appropriate handler.
handleMessage :: Logger -> ServerConfig -> Tracer -> TraceContext -> CircuitBreakerMap -> TVar [AgentStatus] -> ControlMessage -> IO ControlResponse
handleMessage logger config tracer traceCtx cbMap agentStore = \case
  HookEvent input r rl cid -> handleHook logger tracer config input r rl cid cbMap agentStore
  MCPToolCall reqId name args ->
    handleMcpToolTyped logger config tracer traceCtx cbMap agentStore reqId name args
  ToolsListRequest -> handleToolsList logger config
  Ping -> pure Pong

-- | Handle tool discovery request.
handleToolsList :: Logger -> ServerConfig -> IO ControlResponse
handleToolsList logger config = do
  let effectiveRole = fromMaybe (config.defaultRole) (config.role >>= roleFromText)
      roleStr = show effectiveRole
  logInfo logger [fmt|[MCP] Handling ToolsListRequest for role: {roleStr}|]
  tools <- exportMCPTools logger effectiveRole
  logInfo logger [fmt|[MCP] Returning {length tools} tools|]
  pure $ ToolsListResponse tools

-- | Handle MCP tool call using Typed Role DSL.
handleMcpToolTyped :: Logger -> ServerConfig -> Tracer -> TraceContext -> CircuitBreakerMap -> TVar [AgentStatus] -> T.Text -> T.Text -> Aeson.Value -> IO ControlResponse
handleMcpToolTyped logger config tracer traceCtx cbMap agentStore reqId toolName args =
  withMcpTracing logger config traceCtx reqId toolName args $ do
    let effectiveRole = fromMaybe config.defaultRole (config.role >>= roleFromText)
        roleStr = show effectiveRole

    logInfo logger [fmt|[MCP:{reqId}] Dispatching: {toolName} (Role: {roleStr})|]

    -- Dispatch based on role
    -- Uses Generic 'dispatchTool' on the 'mode :- record' structure
    let dispatchResult = case effectiveRole of
          TL ->
            let TLRole {tlToolsRecord = tools} = tlHandlers @AppEffects
             in dispatchTool tools toolName args
          Dev ->
            let DevRole {devToolsRecord = tools} = devHandlers @AppEffects
             in dispatchTool tools toolName args
          PM ->
            let PMRole {pmToolsRecord = tools} = pmHandlers @AppEffects
             in dispatchTool tools toolName args

    case dispatchResult of
      ToolParseError msg -> do
        logError logger [fmt|[MCP:{reqId}] Tool parse error: {msg}|]
        pure $ mcpToolError reqId InvalidRequest $ "Invalid arguments for tool '" <> toolName <> "': " <> msg
      ToolNotFound -> do
        let available = allToolsForRole effectiveRole
        logError logger [fmt|[MCP:{reqId}] Tool not found/allowed: {toolName}|]
        pure $
          mcpToolError reqId NotFound $
            "Tool '"
              <> toolName
              <> "' not found for role "
              <> T.pack (show effectiveRole)
              <> ". Available tools: "
              <> T.intercalate ", " (Set.toList available)
      ToolFound action -> do
        -- Execute the action within the AppEffects runtime
        resultOrErr <- try $ runApp config tracer cbMap logger agentStore action

        case resultOrErr of
          Left (e :: SomeException) -> do
            logError logger [fmt|[MCP:{reqId}] Execution failed: {displayException e}|]
            pure $
              mcpToolError reqId ExternalFailure $
                "Tool execution failed: " <> T.pack (displayException e)
          Right val -> do
            logInfo logger [fmt|[MCP:{reqId}] Success|]
            pure $ mcpToolSuccess reqId val

-- | Wrap MCP tool call with tracing if enabled.
withMcpTracing ::
  Logger ->
  ServerConfig ->
  TraceContext ->
  T.Text ->
  T.Text ->
  Aeson.Value ->
  IO ControlResponse ->
  IO ControlResponse
withMcpTracing logger config traceCtx reqId toolName args action = do
  case config.observabilityConfig of
    Nothing -> action
    Just obsConfig -> do
      -- We want to ensure action is ONLY called once.
      resRef <- newIORef Nothing

      let runActionOnce = do
            mRes <- readIORef resRef
            case mRes of
              Just res -> pure res
              Nothing -> do
                res <- action
                writeIORef resRef (Just res)
                pure res

      resultOrErr <- try
        $ runM
        $ runObservabilityWithContext traceCtx (fromMaybe defaultLokiConfig $ obsConfig.loki)
        $ withSpan
          ("mcp:tool:" <> toolName)
          SpanServer
          [ AttrText "mcp.request_id" reqId,
            AttrText "mcp.tool_name" toolName,
            AttrInt "mcp.input_size" (fromIntegral $ LBS.length $ encode args)
          ]
        $ do
          -- Run the actual action (or get cached result)
          res <- sendM runActionOnce

          -- Add response metadata
          let resSize = case res of
                MCPToolResponse _ (Just val) _ -> LBS.length $ encode val
                _ -> 0
              isError = case res of
                MCPToolResponse _ _ (Just _) -> True
                _ -> False

          addSpanAttribute (AttrInt "mcp.output_size" (fromIntegral resSize))
          when isError $ addSpanAttribute (AttrText "error" "true")

          pure res

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger [fmt|[MCP:{reqId}] Tracing error: {show e}|]
          throwIO e
        Right res -> pure res
