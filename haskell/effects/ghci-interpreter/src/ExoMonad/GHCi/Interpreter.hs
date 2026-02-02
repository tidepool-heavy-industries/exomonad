-- | GHCi effect interpreter - thin client for ghci-oracle server.
--
-- = Usage
--
-- @
-- import ExoMonad.GHCi.Interpreter (runGHCiIO, withGHCiConnection)
-- import ExoMonad.Effect.GHCi (queryType)
--
-- main = withGHCiConnection "127.0.0.1" 9999 $ \conn -> do
--   result <- runM $ runGHCiIO conn $ do
--     typeInfo <- queryType "fmap"
--     pure typeInfo
--   print result
-- @
--
-- = Requirements
--
-- Requires a running ghci-oracle server. Start one with:
--
-- @
-- ghci-oracle --port 9999 --project /path/to/project
-- @
module ExoMonad.GHCi.Interpreter
  ( -- * Interpreter
    runGHCiIO,

    -- * Connection Management
    GHCiConnection,
    withGHCiConnection,
    connectToOracle,
    disconnectFromOracle,

    -- * Configuration
    GHCiClientConfig (..),
    defaultClientConfig,
  )
where

import Control.Exception (bracket)
import Polysemy (Sem, Member, interpret, embed)
import Polysemy.Embed (Embed)
import Data.Text (Text)
import ExoMonad.Effect.GHCi
  ( GHCi (..),
    GHCiError (..),
    GHCiRequest (..),
    GHCiResponse (..),
  )
import ExoMonad.GHCi.Protocol
  ( GHCiConnection,
    connect,
    disconnect,
    sendRequest,
  )

-- ════════════════════════════════════════════════════════════════════════════
-- CONFIGURATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Client configuration for connecting to ghci-oracle.
data GHCiClientConfig = GHCiClientConfig
  { -- | Server host (default: "127.0.0.1")
    gccHost :: String,
    -- | Server port (default: 9999)
    gccPort :: Int
  }
  deriving stock (Show, Eq)

-- | Default client configuration.
defaultClientConfig :: GHCiClientConfig
defaultClientConfig =
  GHCiClientConfig
    { gccHost = "127.0.0.1",
      gccPort = 9999
    }

-- ════════════════════════════════════════════════════════════════════════════
-- CONNECTION MANAGEMENT
-- ════════════════════════════════════════════════════════════════════════════

-- | Connect to a ghci-oracle server.
connectToOracle :: GHCiClientConfig -> IO (Either GHCiError GHCiConnection)
connectToOracle config = connect (gccHost config) (gccPort config)

-- | Disconnect from a ghci-oracle server.
disconnectFromOracle :: GHCiConnection -> IO ()
disconnectFromOracle = disconnect

-- | Bracket for connection lifecycle.
--
-- Returns @Left GHCiNotConnected@ if the initial connection fails.
-- If the action throws an exception, the connection is still closed
-- but the exception propagates (not wrapped in @Left@).
--
-- @
-- withGHCiConnection "127.0.0.1" 9999 $ \conn -> do
--   result <- runM $ runGHCiIO conn $ queryType "fmap"
--   print result
-- @
withGHCiConnection :: String -> Int -> (GHCiConnection -> IO a) -> IO (Either GHCiError a)
withGHCiConnection host port action = do
  connResult <- connect host port
  case connResult of
    Left err -> pure $ Left err
    Right conn -> do
      result <- bracket (pure conn) disconnect action
      pure $ Right result

-- ════════════════════════════════════════════════════════════════════════════
-- INTERPRETER
-- ════════════════════════════════════════════════════════════════════════════

-- | Run GHCi effects by communicating with ghci-oracle server.
--
-- Requires an active connection to a ghci-oracle server.
runGHCiIO :: (Member (Embed IO) r) => GHCiConnection -> Sem (GHCi ': r) a -> Sem r a
runGHCiIO conn = interpret $ \case
  QueryType expr -> embed $ do
    resp <- sendRequest conn (ReqQueryType expr)
    pure $ handleTextResponse resp
  QueryInfo name -> embed $ do
    resp <- sendRequest conn (ReqQueryInfo name)
    pure $ handleTextResponse resp
  QueryKind typ -> embed $ do
    resp <- sendRequest conn (ReqQueryKind typ)
    pure $ handleTextResponse resp
  Evaluate expr -> embed $ do
    resp <- sendRequest conn (ReqEvaluate expr)
    pure $ handleTextResponse resp
  CheckCompiles expr -> embed $ do
    resp <- sendRequest conn (ReqCheckCompiles expr)
    pure $ handleBoolResponse resp
  LoadModule modName -> embed $ do
    resp <- sendRequest conn (ReqLoadModule modName)
    pure $ handleUnitResponse resp
  ReloadModules -> embed $ do
    resp <- sendRequest conn ReqReloadModules
    pure $ handleUnitResponse resp

-- ════════════════════════════════════════════════════════════════════════════
-- RESPONSE HANDLERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Handle response expecting Text.
handleTextResponse :: Either GHCiError GHCiResponse -> Either GHCiError Text
handleTextResponse = \case
  Left err -> Left err
  Right (RespSuccess txt) -> Right txt
  Right (RespError err) -> Left err
  Right other -> Left $ GHCiServerError $ "Unexpected response: " <> showResponse other

-- | Handle response expecting Bool.
handleBoolResponse :: Either GHCiError GHCiResponse -> Either GHCiError Bool
handleBoolResponse = \case
  Left err -> Left err
  Right (RespBool b) -> Right b
  Right (RespError err) -> Left err
  Right other -> Left $ GHCiServerError $ "Unexpected response: " <> showResponse other

-- | Handle response expecting ().
handleUnitResponse :: Either GHCiError GHCiResponse -> Either GHCiError ()
handleUnitResponse = \case
  Left err -> Left err
  Right RespUnit -> Right ()
  Right (RespError err) -> Left err
  Right other -> Left $ GHCiServerError $ "Unexpected response: " <> showResponse other

-- | Show response for error messages.
showResponse :: GHCiResponse -> Text
showResponse = \case
  RespSuccess _ -> "RespSuccess"
  RespBool _ -> "RespBool"
  RespUnit -> "RespUnit"
  RespError _ -> "RespError"
  RespPong -> "RespPong"
