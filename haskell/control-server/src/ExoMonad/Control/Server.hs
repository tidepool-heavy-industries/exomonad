{-# LANGUAGE TypeApplications #-}

-- | Unix socket control server for Claude Code++ integration.
--
-- Listens on Unix socket (default .exomonad/sockets/control.sock) and handles
-- HTTP requests via Servant.
module ExoMonad.Control.Server
  ( runServer
  ) where

import ExoMonad.Control.API
import ExoMonad.Control.Handler (handleMessage)
import ExoMonad.Control.Hook.CircuitBreaker (initCircuitBreaker, CircuitBreakerMap)
import ExoMonad.Control.Logging (Logger, logInfo, logDebug, logError)
import ExoMonad.Control.Protocol hiding (role)
import ExoMonad.Control.RoleConfig
import ExoMonad.Control.Types (ServerConfig(..))
import ExoMonad.LLM.Types (LLMConfig(..))
import ExoMonad.GitHub.Interpreter (GitHubConfig(..))
import ExoMonad.Observability.Types (newTraceContext, ObservabilityConfig(..), LokiConfig(..), OTLPConfig(..))
import ExoMonad.Observability.Interpreter (flushTraces)
import ExoMonad.Control.Export (exportMCPTools)
import OpenTelemetry.Trace (Tracer)

import Control.Concurrent.Async (race_)
import Control.Exception (catch, bracket)
import qualified Control.Exception as E
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.Text as AesonText
import qualified Data.Text.Lazy as TL
import Data.Function ((&))
import Data.Maybe (isJust, fromMaybe)
import qualified Data.Text as T
import Network.Socket
import Network.Wai.Handler.Warp
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
        Nothing -> error "EXOMONAD_CONTROL_SOCKET environment variable not set (should be set via start-augmented.sh or .env)"

  -- Load observability config if not already provided
  obsConfig <- case config.observabilityConfig of
    Just c  -> pure (Just c)
    Nothing -> loadObservabilityConfig

  -- Load LLM and GitHub config if not already provided
  llmCfg <- case config.llmConfig of
    Just c -> pure (Just c)
    Nothing -> Just <$> loadLLMConfig
  
  ghCfg <- case config.githubConfig of
    Just c -> pure (Just c)
    Nothing -> Just <$> loadGitHubConfig

  let configFull = config 
        { observabilityConfig = obsConfig
        , llmConfig = llmCfg
        , githubConfig = ghCfg
        }

  when (isJust obsConfig) $
    logInfo logger "Observability enabled (Loki/OTLP)"
  
  case llmCfg of
    Just (LLMSocketConfig path) -> logInfo logger $ "LLM via Service Socket: " <> T.pack path
    _ -> pure ()

  case ghCfg of
    Just (GitHubSocketConfig path) -> logInfo logger $ "GitHub via Service Socket: " <> T.pack path
    _ -> pure ()

  cbMap <- initCircuitBreaker

  let settings = defaultSettings
        & setTimeout (5 * 60) -- 5 minutes timeout

  -- Run both listeners concurrently
  logInfo logger "Control server starting dual listeners..."
  race_
    (bracket
      (setupUnixSocket controlSocket)
      (cleanupUnixSocket controlSocket)
      $ \sock -> do
        logInfo logger $ "Listening on (Unix): " <> T.pack controlSocket
        runSettingsSocket settings sock (app logger configFull tracer cbMap))
    (do
        logInfo logger "Listening on (TCP): 0.0.0.0:7432"
        runSettings (setPort 7432 settings) (app logger configFull tracer cbMap))

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
app :: Logger -> ServerConfig -> Tracer -> CircuitBreakerMap -> Application
app logger config tracer cbMap = serve (Proxy @ExoMonadControlAPI) (server logger config tracer cbMap)

-- | Servant server implementation
server :: Logger -> ServerConfig -> Tracer -> CircuitBreakerMap -> Server ExoMonadControlAPI
server logger config tracer cbMap =
       handleHook
  :<|> handleMcpCall
  :<|> handleMcpTools
  :<|> handlePing
  :<|> handleRoleMcpTools
  :<|> handleRoleMcpCall
  :<|> handleRoleMcpJsonRpc
  where
    handleHook (input, runtime, agentRole) = do
      res <- liftIO $ do
        logDebug logger $ "[HOOK] " <> input.hookEventName <> " runtime=" <> T.pack (show runtime) <> " role=" <> T.pack (show agentRole)
        traceCtx <- newTraceContext
        handleMessage logger config tracer traceCtx cbMap (HookEvent input runtime agentRole)

        -- Note: We do NOT flushTraces here because we use hs-opentelemetry (via Tracer)
        -- which handles export automatically via BatchSpanProcessor.
        -- Legacy ObservabilityConfig is skipped for Hooks.

      case res of
        HookResponse out ec -> pure (out, ec)
        _ -> throwError $ err500 { errBody = "Unexpected response from handleHook" }

    handleMcpCall req = liftIO $ do
      logInfo logger $ "[MCP:" <> req.mcpId <> "] tool=" <> req.toolName
      traceCtx <- newTraceContext
      res <- handleMessage logger config tracer traceCtx cbMap
        (McpToolCall req.mcpId req.toolName req.arguments)

      -- Flush traces (legacy support for MCP tools)
      case config.observabilityConfig of
        Just (ObservabilityOtelConfig _ (Just otlp) svc) ->
           flushTraces otlp svc traceCtx
        _ -> pure ()

      pure res

    handleMcpTools = liftIO $ do
      logDebug logger "[MCP] tools/list request"
      -- Default to config role or defaultRole
      let effectiveRole = fromMaybe (defaultRole config) (role config >>= roleFromText)
      exportMCPTools logger effectiveRole

    handlePing :: Handler T.Text
    handlePing = pure "pong"

    handleRoleMcpTools slug _mSessionId = do
      case roleFromText slug of
        Nothing -> throwError err404 { errBody = "Unknown role: " <> (Aeson.encode slug) }
        Just role -> liftIO $ do
          logDebug logger $ "[MCP:" <> slug <> "] tools/list request"
          exportMCPTools logger role

    handleRoleMcpCall slug mSessionId req = do
      case roleFromText slug of
        Nothing -> throwError err404 { errBody = "Unknown role: " <> (Aeson.encode slug) }
        Just role -> do
          if isToolAllowed role req.toolName
            then liftIO $ do
              -- Use session ID from header if available, otherwise use request ID
              let sessId = fromMaybe req.mcpId mSessionId
              logInfo logger $ "[MCP:" <> slug <> ":" <> sessId <> "] tool=" <> req.toolName
              traceCtx <- newTraceContext

              -- Update config with the role from the slug for handlers that need it
              let configWithRole = config { role = Just slug }

              res <- handleMessage logger configWithRole tracer traceCtx cbMap
                (McpToolCall sessId req.toolName req.arguments)

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

    -- | Unified MCP JSON-RPC endpoint for Claude Code HTTP transport.
    -- Dispatches based on method field: initialize, notifications/initialized, tools/list, tools/call, ping
    handleRoleMcpJsonRpc :: T.Text -> Maybe T.Text -> McpJsonRpcRequest -> Handler McpJsonRpcResponse
    handleRoleMcpJsonRpc slug mSessionId req = do
      case roleFromText slug of
        Nothing -> throwError err404 { errBody = "Unknown role: " <> Aeson.encode slug }
        Just role -> do
          let reqId = req.jrpcId
          liftIO $ logDebug logger $ "[MCP-RPC:" <> slug <> "] method=" <> req.jrpcMethod

          case req.jrpcMethod of
            -- Initialize handshake
            "initialize" -> do
              liftIO $ logInfo logger $ "[MCP-RPC:" <> slug <> "] initialize"
              pure $ McpJsonRpcResponse
                { jrpcRespId = reqId
                , jrpcResult = Just $ Aeson.toJSON McpInitializeResult
                    { mirProtocolVersion = "2024-11-05"
                    , mirCapabilities = McpServerCapabilities { mscTools = Just (object ["listChanged" .= False]) }
                    , mirServerInfo = McpServerInfo { msiName = "exomonad-control-server", msiVersion = "1.0.0" }
                    }
                , jrpcError = Nothing
                }

            -- Notification: initialized (no response body needed, but we return empty for HTTP)
            "notifications/initialized" -> do
              liftIO $ logDebug logger $ "[MCP-RPC:" <> slug <> "] notifications/initialized"
              -- For notifications, return minimal response (HTTP requires some response)
              pure $ McpJsonRpcResponse { jrpcRespId = Nothing, jrpcResult = Nothing, jrpcError = Nothing }

            -- Tools list
            "tools/list" -> do
              tools <- liftIO $ exportMCPTools logger role
              pure $ McpJsonRpcResponse
                { jrpcRespId = reqId
                , jrpcResult = Just $ Aeson.toJSON McpToolsListResult { mtlrTools = tools }
                , jrpcError = Nothing
                }

            -- Tool call
            "tools/call" -> do
              case req.jrpcParams >>= parseMaybe Aeson.parseJSON of
                Nothing -> pure $ McpJsonRpcResponse
                  { jrpcRespId = reqId
                  , jrpcResult = Nothing
                  , jrpcError = Just McpJsonRpcError
                      { jrpcErrCode = -32602
                      , jrpcErrMessage = "Invalid params for tools/call"
                      , jrpcErrData = Nothing
                      }
                  }
                Just (params :: McpToolCallParams) -> do
                  if isToolAllowed role params.mtcpName
                    then do
                      let sessId = fromMaybe "jsonrpc" mSessionId
                      liftIO $ logInfo logger $ "[MCP-RPC:" <> slug <> ":" <> sessId <> "] tool=" <> params.mtcpName
                      traceCtx <- liftIO newTraceContext
                      let configWithRole = config { role = Just slug }
                      res <- liftIO $ handleMessage logger configWithRole tracer traceCtx cbMap
                        (McpToolCall sessId params.mtcpName params.mtcpArguments)

                      -- Convert ControlResponse to MCP tool call result
                      case res of
                        McpToolResponse _ mResult mErr -> do
                          let content = case mResult of
                                Just val -> [McpContentItem "text" (Just $ TL.toStrict $ AesonText.encodeToLazyText val)]
                                Nothing -> []
                              isErr = case mErr of
                                Just _ -> True
                                Nothing -> False
                          pure $ McpJsonRpcResponse
                            { jrpcRespId = reqId
                            , jrpcResult = Just $ Aeson.toJSON McpToolCallResult { mtcrContent = content, mtcrIsError = isErr }
                            , jrpcError = Nothing
                            }
                        _ -> pure $ McpJsonRpcResponse
                          { jrpcRespId = reqId
                          , jrpcResult = Nothing
                          , jrpcError = Just McpJsonRpcError
                              { jrpcErrCode = -32603
                              , jrpcErrMessage = "Internal error: unexpected response type"
                              , jrpcErrData = Nothing
                              }
                          }
                    else do
                      liftIO $ logError logger $ "[MCP-RPC:" <> slug <> "] Forbidden tool: " <> params.mtcpName
                      pure $ McpJsonRpcResponse
                        { jrpcRespId = reqId
                        , jrpcResult = Nothing
                        , jrpcError = Just McpJsonRpcError
                            { jrpcErrCode = -32001
                            , jrpcErrMessage = "Tool not allowed for role " <> slug <> ": " <> params.mtcpName
                            , jrpcErrData = Nothing
                            }
                        }

            -- Ping
            "ping" -> do
              liftIO $ logDebug logger $ "[MCP-RPC:" <> slug <> "] ping"
              pure $ McpJsonRpcResponse
                { jrpcRespId = reqId
                , jrpcResult = Just $ object []
                , jrpcError = Nothing
                }

            -- Unknown method
            _ -> do
              liftIO $ logError logger $ "[MCP-RPC:" <> slug <> "] Unknown method: " <> req.jrpcMethod
              pure $ McpJsonRpcResponse
                { jrpcRespId = reqId
                , jrpcResult = Nothing
                , jrpcError = Just McpJsonRpcError
                    { jrpcErrCode = -32601
                    , jrpcErrMessage = "Method not found: " <> req.jrpcMethod
                    , jrpcErrData = Nothing
                    }
                }