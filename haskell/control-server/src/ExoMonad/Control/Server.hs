{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

-- | Unix socket control server for Claude Code++ integration.
--
-- Listens on Unix socket (default .exomonad/sockets/control.sock) and handles
-- HTTP requests via Servant.
module ExoMonad.Control.Server
  ( runServer,
  )
where

import Control.Concurrent.Async (race_)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Exception (bracket, catch)
import Control.Exception qualified as E
import Control.Lens ((.~))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Managed
import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Text qualified as AesonText
import Data.Aeson.Types (parseMaybe)
import Data.Function ((&))
import Data.Generics.Labels ()
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import ExoMonad.Control.API
import ExoMonad.Control.Export (exportMCPTools)
import ExoMonad.Control.Handler (handleMessage)
import ExoMonad.Control.Hook.CircuitBreaker (CircuitBreakerMap, initCircuitBreaker)
import ExoMonad.Control.Logging (Logger, logDebug, logError, logInfo)
import ExoMonad.Control.Protocol
import ExoMonad.Control.RoleConfig
-- Imports for effects

import ExoMonad.Control.Runtime (runApp)
import ExoMonad.Control.Types (ServerConfig (..))
import ExoMonad.Effects.DockerSpawner (ContainerId (..), stopContainer)
import ExoMonad.GitHub.Interpreter (GitHubConfig (..))
import ExoMonad.LLM.Interpreter.Types (LLMConfig (..))
import ExoMonad.Observability.Interpreter (flushTraces)
import ExoMonad.Observability.Types (LokiConfig (..), OTLPConfig (..), ObservabilityConfig (..), newTraceContext)
import Network.Socket
import Network.Wai.Handler.Warp
import OpenTelemetry.Trace (Tracer)
import PyF (fmt)
import Servant
import System.Directory (createDirectoryIfMissing, removeFile)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory)
import System.IO.Error (isDoesNotExistError)

-- | Load LLM configuration from environment variables.
-- REQUIRES EXOMONAD_SERVICE_SOCKET.
loadLLMConfig :: IO LLMConfig
loadLLMConfig = do
  socketPath <- lookupEnv "EXOMONAD_SERVICE_SOCKET"
  case socketPath of
    Just path -> pure $ LLMSocketConfig path
    Nothing -> error "EXOMONAD_SERVICE_SOCKET environment variable required for LLM configuration. This is the mandatory 'blessed path' for service integration."

-- | Load GitHub configuration from environment variables.
-- REQUIRES EXOMONAD_SERVICE_SOCKET.
loadGitHubConfig :: IO GitHubConfig
loadGitHubConfig = do
  socketPath <- lookupEnv "EXOMONAD_SERVICE_SOCKET"
  case socketPath of
    Just path -> pure $ GitHubSocketConfig path
    Nothing -> error "EXOMONAD_SERVICE_SOCKET environment variable required for GitHub configuration. This is the mandatory 'blessed path' for service integration."

-- | Load observability configuration from environment variables.
loadObservabilityConfig :: IO (Maybe ObservabilityConfig)
loadObservabilityConfig = do
  lokiUrl <- lookupEnv "LOKI_URL"
  otlpEndpoint <- lookupEnv "OTLP_ENDPOINT"

  case (lokiUrl, otlpEndpoint) of
    (Nothing, Nothing) -> pure Nothing
    _ -> do
      lokiUser <- lookupEnv "LOKI_USER"
      lokiToken <- lookupEnv "LOKI_TOKEN"
      otlpUser <- lookupEnv "OTLP_USER"
      otlpToken <- lookupEnv "OTLP_TOKEN"

      let loki = fmap (\url -> LokiConfig (T.pack url) (fmap T.pack lokiUser) (fmap T.pack lokiToken) "exomonad-control-server") lokiUrl
          otlp = fmap (\end -> OTLPConfig (T.pack end) (fmap T.pack otlpUser) (fmap T.pack otlpToken)) otlpEndpoint

      pure $ Just $ ObservabilityOtelConfig loki otlp "exomonad-control-server"

-- | Run the control server. Blocks forever.
runServer :: Logger -> ServerConfig -> Tracer -> IO ()
runServer logger config tracer = do
  -- Get socket path from environment
  controlSocketEnv <- lookupEnv "EXOMONAD_CONTROL_SOCKET"
  let controlSocket = case controlSocketEnv of
        Just s -> s
        Nothing -> error "EXOMONAD_CONTROL_SOCKET environment variable not set (should be set via Docker or .env)"

  -- Load observability config if not already provided
  obsConfig <- case config.observabilityConfig of
    Just c -> pure (Just c)
    Nothing -> loadObservabilityConfig

  -- Load LLM and GitHub config if not already provided
  llmCfg <- case config.llmConfig of
    Just c -> pure (Just c)
    Nothing -> Just <$> loadLLMConfig

  ghCfg <- case config.githubConfig of
    Just c -> pure (Just c)
    Nothing -> Just <$> loadGitHubConfig

  let configFull =
        config
          { observabilityConfig = obsConfig,
            llmConfig = llmCfg,
            githubConfig = ghCfg
          }

  when (isJust obsConfig) $
    logInfo logger "Observability enabled (Loki/OTLP)"

  case llmCfg of
    Just (LLMSocketConfig path) -> logInfo logger [fmt|LLM via Service Socket: {path}|]
    _ -> pure ()

  case ghCfg of
    Just (GitHubSocketConfig path) -> logInfo logger [fmt|GitHub via Service Socket: {path}|]
    _ -> pure ()

  cbMap <- initCircuitBreaker

  -- Initialize agent store (ephemeral, in-memory)
  agentStore <- newTVarIO []

  let settings =
        defaultSettings
          & setTimeout (5 * 60) -- 5 minutes timeout

  -- Run both listeners concurrently
  logInfo logger "Control server starting dual listeners..."
  runManaged $ do
    sock <- managed (bracket (setupUnixSocket controlSocket) (cleanupUnixSocket controlSocket))
    liftIO $
      race_
        ( do
            logInfo logger [fmt|Listening on (Unix): {controlSocket}|]
            runSettingsSocket settings sock (app logger configFull tracer cbMap agentStore)
        )
        ( do
            logInfo logger "Listening on (TCP): 0.0.0.0:7432"
            runSettings (setPort 7432 settings) (app logger configFull tracer cbMap agentStore)
        )

-- | Setup Unix socket at given path.
setupUnixSocket :: FilePath -> IO Socket
setupUnixSocket path = do
  -- Ensure directory exists
  createDirectoryIfMissing True (takeDirectory path)

  -- Remove existing socket file if it exists
  cleanupSocketFile path

  sock <- socket AF_UNIX Stream 0
  bind sock (SockAddrUnix path)
  listen sock 10
  pure sock

-- | Cleanup Unix socket at given path.
cleanupUnixSocket :: FilePath -> Socket -> IO ()
cleanupUnixSocket path sock = do
  close sock
  cleanupSocketFile path

-- | Cleanup just the socket file
cleanupSocketFile :: FilePath -> IO ()
cleanupSocketFile path =
  catch (removeFile path) $ \e ->
    if isDoesNotExistError e
      then pure ()
      else E.throwIO e

-- | Servant application
app :: Logger -> ServerConfig -> Tracer -> CircuitBreakerMap -> TVar [AgentStatus] -> Application
app logger config tracer cbMap agentStore = serve (Proxy @ExoMonadControlAPI) (server logger config tracer cbMap agentStore)

-- | Servant server implementation
server :: Logger -> ServerConfig -> Tracer -> CircuitBreakerMap -> TVar [AgentStatus] -> Server ExoMonadControlAPI
server logger config tracer cbMap agentStore =
  handleHook
    :<|> handleMcpCall
    :<|> handleMcpTools
    :<|> handlePing
    :<|> handleRoleMcpTools
    :<|> handleRoleMcpCall
    :<|> handleRoleMcpJsonRpc
    :<|> handleAgents
    :<|> handleAgentLogs
    :<|> handleAgentStop
  where
    handleHook (input, runtime, agentRole, containerId) = do
      liftIO $ do
        logDebug logger $ "[HOOK] " <> input.hookEventName <> " runtime=" <> T.pack (show runtime) <> " role=" <> T.pack (show agentRole) <> " container=" <> T.pack (show containerId)
        traceCtx <- newTraceContext
        handleMessage logger config tracer traceCtx cbMap agentStore (HookEvent input runtime agentRole containerId)

    -- Note: We do NOT flushTraces here because we use hs-opentelemetry (via Tracer)
    -- which handles export automatically via BatchSpanProcessor.
    -- Legacy ObservabilityConfig is skipped for Hooks.

    handleMcpCall req = liftIO $ do
      logInfo logger $ "[MCP:" <> req.mcpId <> "] tool=" <> req.toolName
      traceCtx <- newTraceContext
      res <-
        handleMessage
          logger
          config
          tracer
          traceCtx
          cbMap
          agentStore
          (MCPToolCall req.mcpId req.toolName req.arguments Nothing)

      -- Flush traces (legacy support for MCP tools)
      case config.observabilityConfig of
        Just (ObservabilityOtelConfig _ (Just otlp) svc) ->
          flushTraces otlp svc traceCtx
        _ -> pure ()

      pure res

    handleMcpTools = liftIO $ do
      logDebug logger "[MCP] tools/list request"
      -- Default to config role or defaultRole
      let effectiveRole = fromMaybe (config.defaultRole) (config.role >>= roleFromText)
      exportMCPTools logger effectiveRole

    handlePing :: Handler T.Text
    handlePing = pure "pong"

    handleRoleMcpTools slug _mSessionId _mContainer = do
      case roleFromText slug of
        Nothing -> throwError err404 {errBody = "Unknown role: " <> (Aeson.encode slug)}
        Just role -> liftIO $ do
          logDebug logger $ "[MCP:" <> slug <> "] tools/list request"
          exportMCPTools logger role

    handleRoleMcpCall slug mSessionId mContainer req = do
      case roleFromText slug of
        Nothing -> throwError err404 {errBody = "Unknown role: " <> (Aeson.encode slug)}
        Just role -> do
          if isToolAllowed role req.toolName
            then liftIO $ do
              -- Use session ID from header if available, otherwise use request ID
              let sessId = fromMaybe req.mcpId mSessionId
              logInfo logger $ "[MCP:" <> slug <> ":" <> sessId <> "] tool=" <> req.toolName <> " container=" <> T.pack (show mContainer)
              traceCtx <- newTraceContext

              -- Update config with the role from the slug for handlers that need it
              let configWithRole = config & #role .~ Just slug

              res <-
                handleMessage
                  logger
                  configWithRole
                  tracer
                  traceCtx
                  cbMap
                  agentStore
                  (MCPToolCall sessId req.toolName req.arguments mContainer)

              -- Flush traces (legacy)
              case config.observabilityConfig of
                Just (ObservabilityOtelConfig _ (Just otlp) svc) ->
                  flushTraces otlp svc traceCtx
                _ -> pure ()

              pure res
            else do
              let sessId = fromMaybe req.mcpId mSessionId
              liftIO $ logError logger $ "[MCP:" <> slug <> ":" <> sessId <> "] Forbidden tool: " <> req.toolName
              pure $ mcpToolError sessId PermissionDenied ("Tool not allowed for role " <> slug <> ": " <> req.toolName)

    -- \| Unified MCP JSON-RPC endpoint for Claude Code HTTP transport.
    -- Dispatches based on method field: initialize, notifications/initialized, tools/list, tools/call, ping
    handleRoleMcpJsonRpc :: T.Text -> Maybe T.Text -> Maybe T.Text -> McpJsonRpcRequest -> Handler McpJsonRpcResponse
    handleRoleMcpJsonRpc slug mSessionId mContainer req = do
      case roleFromText slug of
        Nothing -> throwError err404 {errBody = "Unknown role: " <> Aeson.encode slug}
        Just role -> do
          let reqId = req.jrpcId
          liftIO $ logDebug logger $ "[MCP-RPC:" <> slug <> "] method=" <> req.jrpcMethod

          case req.jrpcMethod of
            -- Initialize handshake
            "initialize" -> do
              liftIO $ logInfo logger $ "[MCP-RPC:" <> slug <> "] initialize"
              pure $
                McpJsonRpcResponse
                  { jrpcRespId = reqId,
                    jrpcResult =
                      Just $
                        Aeson.toJSON
                          McpInitializeResult
                            { mirProtocolVersion = "2024-11-05",
                              mirCapabilities = McpServerCapabilities {mscTools = Just (object ["listChanged" .= False])},
                              mirServerInfo = McpServerInfo {msiName = "exomonad-control-server", msiVersion = "1.0.0"}
                            },
                    jrpcError = Nothing
                  }

            -- Notification: initialized (no response body needed, but we return empty for HTTP)
            "notifications/initialized" -> do
              liftIO $ logDebug logger $ "[MCP-RPC:" <> slug <> "] notifications/initialized"
              -- For notifications, return minimal response (HTTP requires some response)
              pure $ McpJsonRpcResponse {jrpcRespId = Nothing, jrpcResult = Nothing, jrpcError = Nothing}

            -- Tools list
            "tools/list" -> do
              tools <- liftIO $ exportMCPTools logger role
              pure $
                McpJsonRpcResponse
                  { jrpcRespId = reqId,
                    jrpcResult = Just $ Aeson.toJSON McpToolsListResult {mtlrTools = tools},
                    jrpcError = Nothing
                  }

            -- Tool call
            "tools/call" -> do
              case req.jrpcParams >>= parseMaybe Aeson.parseJSON of
                Nothing ->
                  pure $
                    McpJsonRpcResponse
                      { jrpcRespId = reqId,
                        jrpcResult = Nothing,
                        jrpcError =
                          Just
                            McpJsonRpcError
                              { jrpcErrCode = -32602,
                                jrpcErrMessage = "Invalid params for tools/call",
                                jrpcErrData = Nothing
                              }
                      }
                Just (params :: McpToolCallParams) -> do
                  let sessId = fromMaybe "jsonrpc" mSessionId
                  if isToolAllowed role params.mtcpName
                    then do
                      liftIO $ logInfo logger $ "[MCP-RPC:" <> slug <> ":" <> sessId <> "] tool=" <> params.mtcpName <> " container=" <> T.pack (show mContainer)
                      traceCtx <- liftIO newTraceContext
                      let configWithRole = config & #role .~ Just slug
                      res <-
                        liftIO $
                          handleMessage
                            logger
                            configWithRole
                            tracer
                            traceCtx
                            cbMap
                            agentStore
                            (MCPToolCall sessId params.mtcpName params.mtcpArguments mContainer)

                      -- Convert ControlResponse to MCP tool call result
                      case res of
                        MCPToolResponse _ mResult mErr -> do
                          let content = case mResult of
                                Just val -> [McpContentItem "text" (Just $ TL.toStrict $ AesonText.encodeToLazyText val)]
                                Nothing -> []
                              isErr = case mErr of
                                Just _ -> True
                                Nothing -> False
                          pure $
                            McpJsonRpcResponse
                              { jrpcRespId = reqId,
                                jrpcResult = Just $ Aeson.toJSON McpToolCallResult {mtcrContent = content, mtcrIsError = isErr},
                                jrpcError = Nothing
                              }
                        _ ->
                          pure $
                            McpJsonRpcResponse
                              { jrpcRespId = reqId,
                                jrpcResult = Nothing,
                                jrpcError =
                                  Just
                                    McpJsonRpcError
                                      { jrpcErrCode = -32603,
                                        jrpcErrMessage = "Internal error: unexpected response type",
                                        jrpcErrData = Nothing
                                      }
                              }
                    else do
                      liftIO $ logError logger $ "[MCP-RPC:" <> slug <> "] Forbidden tool: " <> params.mtcpName
                      pure $
                        McpJsonRpcResponse
                          { jrpcRespId = reqId,
                            jrpcResult = Nothing,
                            jrpcError =
                              Just
                                McpJsonRpcError
                                  { jrpcErrCode = -32001,
                                    jrpcErrMessage = "Tool not allowed for role " <> slug <> ": " <> params.mtcpName,
                                    jrpcErrData = Nothing
                                  }
                          }

            -- Ping
            "ping" -> do
              liftIO $ logDebug logger $ "[MCP-RPC:" <> slug <> "] ping"
              pure $
                McpJsonRpcResponse
                  { jrpcRespId = reqId,
                    jrpcResult = Just $ object [],
                    jrpcError = Nothing
                  }

            -- Unknown method
            _ -> do
              liftIO $ logError logger $ "[MCP-RPC:" <> slug <> "] Unknown method: " <> req.jrpcMethod
              pure $
                McpJsonRpcResponse
                  { jrpcRespId = reqId,
                    jrpcResult = Nothing,
                    jrpcError =
                      Just
                        McpJsonRpcError
                          { jrpcErrCode = -32601,
                            jrpcErrMessage = "Method not found: " <> req.jrpcMethod,
                            jrpcErrData = Nothing
                          }
                  }

    -- \| Return agent status for dashboard.
    handleAgents :: Handler AgentsResponse
    handleAgents = liftIO $ do
      agents <- readTVarIO agentStore
      pure $ AgentsResponse agents

    -- \| Return logs for an agent (placeholder).
    handleAgentLogs :: T.Text -> Handler T.Text
    handleAgentLogs _id = pure "Logs not implemented"

    -- \| Stop an agent.
    handleAgentStop :: T.Text -> Handler ()
    handleAgentStop id_ = do
      agents <- liftIO $ readTVarIO agentStore
      let mAgent = listToMaybe $ filter (\a -> a.asId == id_) agents
      case mAgent of
        Nothing -> do
          liftIO $ logError logger $ "Agent not found for stop: " <> id_
          throwError $ err404 {errBody = "Agent not found for stop: " <> (Aeson.encode id_)}
        Just agent -> do
          liftIO $ logInfo logger $ "Stopping agent: " <> id_ <> " (" <> agent.asContainerId <> ")"
          let cid = ContainerId agent.asContainerId
          -- Execute stop effect, passing the agent's container ID
          res <- liftIO $ runApp config tracer cbMap logger agentStore (Just agent.asContainerId) (stopContainer cid)          case res of
            Left err -> liftIO $ logError logger $ "Failed to stop agent: " <> T.pack (show err)
            Right () -> pure ()
