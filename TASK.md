# Task: Haskell Socket-Based Interpreters

## Scope

Add socket-based service communication to Haskell interpreters. **No Rust changes in this branch.** Keep existing HTTP implementations working (dual-mode) until integration.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                 Interpreter (e.g., llm-interpreter)          │
│                                                              │
│  runLLM :: Config -> Eff (LLM : es) a -> Eff es a           │
│                                                              │
│  Config = HttpConfig { endpoint, apiKey }                    │
│         | SocketConfig { socketPath }                        │
│                                                              │
│  interpret $ \case                                           │
│    CallLLM req -> case config of                            │
│      HttpConfig{} -> httpCall req    -- existing            │
│      SocketConfig{} -> socketCall req -- new                │
└─────────────────────────────────────────────────────────────┘
```

## Packages to Modify

| Package | Effect | Current | Add |
|---------|--------|---------|-----|
| `llm-interpreter` | `LLM` | HTTP to Anthropic | Socket to Rust |
| `github-interpreter` | `GitHub` | HTTP to GitHub API | Socket to Rust |
| `gemini-interpreter` | `Gemini` | HTTP to Ollama | Socket to Rust |
| `observability-interpreter` | `Observability` | hs-opentelemetry | Socket to Rust |

## Shared Socket Client

Create `haskell/effects/socket-client/` - shared infrastructure:

```haskell
-- SocketClient.hs
module ExoMonad.Effects.SocketClient
  ( SocketConfig(..)
  , ServiceRequest(..)
  , ServiceResponse(..)
  , sendRequest
  , withSocketConnection
  ) where

import Network.Socket
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson

data SocketConfig = SocketConfig
  { scSocketPath :: FilePath
  , scTimeout :: Int  -- milliseconds
  }

-- Mirror of Rust protocol types
data ServiceRequest
  = AnthropicChat { model :: Text, messages :: [Message], maxTokens :: Int, ... }
  | GitHubGetIssue { owner :: Text, repo :: Text, number :: Int }
  | GitHubListIssues { owner :: Text, repo :: Text, state :: IssueState, labels :: [Text] }
  | OllamaGenerate { model :: Text, prompt :: Text, system :: Maybe Text }
  | OtelSpan { traceId :: Text, spanId :: Text, name :: Text, ... }
  deriving (Generic, ToJSON)

data ServiceResponse
  = AnthropicChatResponse { content :: [ContentBlock], stopReason :: StopReason, usage :: Usage }
  | GitHubIssueResponse { number :: Int, title :: Text, body :: Text, ... }
  | GitHubIssuesResponse { issues :: [GitHubIssueRef] }
  | OllamaGenerateResponse { response :: Text, done :: Bool }
  | OtelAckResponse
  | ErrorResponse { code :: Int, message :: Text }
  deriving (Generic, FromJSON)

-- NDJSON over Unix socket
sendRequest :: SocketConfig -> ServiceRequest -> IO (Either ServiceError ServiceResponse)
sendRequest config req = withSocketConnection config $ \sock -> do
  let encoded = encode req <> "\n"
  sendAll sock (LBS.toStrict encoded)
  response <- recvLine sock
  pure $ eitherDecode (LBS.fromStrict response)

withSocketConnection :: SocketConfig -> (Socket -> IO a) -> IO a
withSocketConnection config action = bracket open close action
  where
    open = do
      sock <- socket AF_UNIX Stream defaultProtocol
      connect sock (SockAddrUnix (scSocketPath config))
      pure sock
```

## Interpreter Modifications

### llm-interpreter

```haskell
-- Before: Only HTTP
data LLMConfig = LLMConfig
  { lcEndpoint :: Text
  , lcApiKey :: Text
  }

runLLM :: LLMConfig -> Eff (LLM : es) a -> Eff es a

-- After: HTTP or Socket
data LLMConfig
  = LLMHttpConfig { lcEndpoint :: Text, lcApiKey :: Text }
  | LLMSocketConfig { lcSocketPath :: FilePath }

runLLM :: LLMConfig -> Eff (LLM : es) a -> Eff es a
runLLM config = interpret $ \case
  CallLLM req -> case config of
    LLMHttpConfig{} -> httpCallLLM config req
    LLMSocketConfig path -> do
      let socketCfg = SocketConfig path 30000
      let serviceReq = toServiceRequest req  -- Convert to protocol type
      result <- liftIO $ sendRequest socketCfg serviceReq
      case result of
        Right (AnthropicChatResponse content stop usage) ->
          pure $ fromServiceResponse content stop usage
        Right (ErrorResponse code msg) ->
          throwError $ LLMError code msg
        Left err ->
          throwError $ SocketError err
```

### github-interpreter

```haskell
data GitHubConfig
  = GitHubHttpConfig { ghToken :: Text }
  | GitHubSocketConfig { ghSocketPath :: FilePath }

runGitHub :: GitHubConfig -> Eff (GitHub : es) a -> Eff es a
runGitHub config = interpret $ \case
  GetIssue owner repo num -> case config of
    GitHubHttpConfig token -> httpGetIssue token owner repo num
    GitHubSocketConfig path -> do
      let req = GitHubGetIssue owner repo num
      result <- liftIO $ sendRequest (SocketConfig path 10000) req
      handleGitHubResponse result
```

### gemini-interpreter

```haskell
data GeminiConfig
  = GeminiHttpConfig { gcEndpoint :: Text }
  | GeminiSocketConfig { gcSocketPath :: FilePath }

runGemini :: GeminiConfig -> Eff (Gemini : es) a -> Eff es a
```

### observability-interpreter

```haskell
data ObservabilityConfig
  = ObservabilityOtelConfig { ... }  -- Current hs-opentelemetry
  | ObservabilitySocketConfig { ocSocketPath :: FilePath }

runObservability :: ObservabilityConfig -> Eff (Observability : es) a -> Eff es a
```

## Protocol Type Alignment

The Haskell `ServiceRequest`/`ServiceResponse` must exactly match the Rust definitions in `exomonad-shared/protocol.rs`. Use the same JSON serialization:

```haskell
-- Must match Rust's #[serde(tag = "type")]
instance ToJSON ServiceRequest where
  toJSON (AnthropicChat model msgs maxTok tools sys) = object
    [ "type" .= ("AnthropicChat" :: Text)
    , "model" .= model
    , "messages" .= msgs
    , "max_tokens" .= maxTok
    , "tools" .= tools
    , "system" .= sys
    ]
```

## Cabal Changes

### New package: socket-client

```cabal
-- haskell/effects/socket-client/socket-client.cabal
name: exomonad-socket-client
version: 0.1.0

library
  exposed-modules:
    ExoMonad.Effects.SocketClient
  build-depends:
    base >=4.14 && <5,
    aeson,
    bytestring,
    network,
    text
```

### Modified packages

Each interpreter adds `exomonad-socket-client` to build-depends:

```cabal
-- llm-interpreter.cabal
build-depends:
    ...existing...,
    exomonad-socket-client
```

### cabal.project

```cabal
packages:
    ...
    haskell/effects/socket-client
```

## Testing Strategy

1. **Unit tests**: Test JSON encoding matches expected format
2. **Mock socket server**: Simple echo server for testing socket communication
3. **Property tests**: Round-trip ServiceRequest → JSON → ServiceRequest

```haskell
-- Test that our encoding matches Rust expectations
spec :: Spec
spec = do
  describe "ServiceRequest JSON" $ do
    it "encodes AnthropicChat with type tag" $ do
      let req = AnthropicChat "claude-3" [UserMessage "hello"] 1000 Nothing Nothing
      let json = encode req
      json `shouldContain` "\"type\":\"AnthropicChat\""
      json `shouldContain` "\"model\":\"claude-3\""
```

## Acceptance Criteria

- [ ] `cabal build all` succeeds
- [ ] All existing tests pass (HTTP mode still works)
- [ ] New `socket-client` package created
- [ ] `llm-interpreter` supports `LLMSocketConfig`
- [ ] `github-interpreter` supports `GitHubSocketConfig`
- [ ] `gemini-interpreter` supports `GeminiSocketConfig`
- [ ] `observability-interpreter` supports `ObservabilitySocketConfig`
- [ ] JSON encoding tests verify protocol alignment
- [ ] No new HTTP/TLS dependencies added

## Non-Goals (for this branch)

- Actually connecting to Rust service (no Rust changes)
- Removing HTTP code paths
- Removing HTTP dependencies from .cabal files
- Modifying control-server to use socket configs

## Migration Path

After this branch merges (and rust-services merges):

1. **Integration PR**: Wire control-server to pass `SocketConfig` to interpreters
2. **Verification**: E2E tests with real Rust services
3. **Cleanup PR**: Remove HTTP code paths, remove HTTP deps

## File Checklist

New files:
- [ ] `haskell/effects/socket-client/socket-client.cabal`
- [ ] `haskell/effects/socket-client/src/ExoMonad/Effects/SocketClient.hs`
- [ ] `haskell/effects/socket-client/test/Spec.hs`

Modified files:
- [ ] `cabal.project` (add socket-client)
- [ ] `haskell/effects/llm-interpreter/src/...` (add socket mode)
- [ ] `haskell/effects/llm-interpreter/exomonad-llm-interpreter.cabal` (add dep)
- [ ] `haskell/effects/github-interpreter/src/...` (add socket mode)
- [ ] `haskell/effects/github-interpreter/exomonad-github-interpreter.cabal` (add dep)
- [ ] `haskell/effects/gemini-interpreter/src/...` (add socket mode)
- [ ] `haskell/effects/gemini-interpreter/exomonad-gemini-interpreter.cabal` (add dep)
- [ ] `haskell/effects/observability-interpreter/src/...` (add socket mode)
- [ ] `haskell/effects/observability-interpreter/exomonad-observability-interpreter.cabal` (add dep)
