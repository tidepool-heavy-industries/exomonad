{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | IO-blind GHCi Oracle effect (Native-only)
--
-- = Overview
--
-- This module provides an IO-blind GHCi effect for native builds.
-- GHCi Oracle requires a separate server process that manages a persistent
-- GHCi session, which is not available in WASM/browser environments.
--
-- All operations have a 'NativeOnly' constraint that produces
-- a helpful compile-time error if used in WASM builds.
--
-- = Architecture
--
-- @
--  +---------------------+
--  | ExoMonad.Effect.GHCi | <- This module (types, effect, wire protocol)
--  +---------------------+
--            |
--    +-------v--------+
--    | ghci-interpreter  | <- Native interpreter (socket client)
--    +----------------+
--            |
--       socket/IPC
--            |
--    +-------v--------+
--    | ghci-oracle    | <- Standalone server (manages ghci subprocess)
--    +----------------+
-- @
--
-- Includes:
--   - Effect definition (GHCi GADT)
--   - Smart constructors for GHCi queries
--   - Error types
--   - Wire protocol types (GHCiRequest, GHCiResponse)
--   - Stub runner for testing (runGHCiStub)
--
-- = Usage
--
-- Effectful handler:
--
-- @
-- import ExoMonad.Effect.GHCi
--
-- myHandler :: (Member GHCi r, NativeOnly) => Sem r Text
-- myHandler = do
--   result <- queryType "fmap"
--   case result of
--     Right typeInfo -> pure typeInfo
--     Left err -> handleError err
-- @
--
-- Stub runner for testing:
--
-- @
-- import ExoMonad.Effect (runLog, LogLevel(..))
--
-- test :: IO ()
-- test = void $ runM $ runLog InfoLevel $ runGHCiStub $ do
--   result <- queryType "fmap"
--   pure ()
-- @
module ExoMonad.Effect.GHCi
  ( -- * Platform Constraint
    NativeOnly,

    -- * Effect
    GHCi (..),

    -- * Smart Constructors
    queryType,
    queryInfo,
    queryKind,
    evaluate,
    checkCompiles,
    loadModule,
    reloadModules,

    -- * Error Types
    GHCiError (..),

    -- * Wire Protocol
    GHCiRequest (..),
    GHCiResponse (..),

    -- * Stub Runner
    runGHCiStub,
  )
where

import Polysemy (Sem, Member, interpret, makeSem)
import Data.Kind (Type)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Effect (Log, logInfo)
import ExoMonad.Platform (NativeOnly)
import GHC.Generics (Generic)

-- ════════════════════════════════════════════════════════════════════════════
-- ERROR TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Errors that can occur during GHCi operations.
data GHCiError
  = GHCiSessionCrashed
      { -- | Last output before crash
        gseCrashOutput :: Text,
        -- | Exit code if available
        gseExitCode :: Maybe Int
      }
  | -- \^ GHCi process crashed unexpectedly
    GHCiTimeout
      { -- | Query that timed out
        gteQuery :: Text,
        -- | Timeout in milliseconds
        gteTimeoutMs :: Int
      }
  | -- \^ Query exceeded timeout
    GHCiParseError
      { -- | Query that failed
        gpeQuery :: Text,
        -- | GHCi error output
        gpeGHCOutput :: Text
      }
  | -- \^ GHCi reported a parse/type error
    GHCiLoadError
      { -- | Module that failed to load
        gleModule :: Text,
        -- | Compilation errors
        gleErrors :: Text
      }
  | -- \^ Module failed to load

    -- | Cannot reach the oracle server
    GHCiNotConnected
  | GHCiServerError
      { -- | Error message from server
        gseMessage :: Text
      }
  -- \^ Oracle server reported an error
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ════════════════════════════════════════════════════════════════════════════
-- WIRE PROTOCOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Request sent from client to oracle server.
data GHCiRequest
  = -- | @:type expression@
    ReqQueryType Text
  | -- | @:info name@
    ReqQueryInfo Text
  | -- | @:kind type@
    ReqQueryKind Text
  | -- | Evaluate expression
    ReqEvaluate Text
  | -- | Check if expression compiles (no execution)
    ReqCheckCompiles Text
  | -- | @:load ModuleName@
    ReqLoadModule Text
  | -- | @:reload@
    ReqReloadModules
  | -- | Health check
    ReqPing
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Response sent from oracle server to client.
data GHCiResponse
  = -- | Successful query with text result
    RespSuccess Text
  | -- | Boolean result (for CheckCompiles)
    RespBool Bool
  | -- | Successful operation with no result (for Load, Reload)
    RespUnit
  | -- | Error occurred
    RespError GHCiError
  | -- | Response to Ping
    RespPong
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT DEFINITION
-- ════════════════════════════════════════════════════════════════════════════

-- | The GHCi effect provides type-level oracle queries.
--
-- All operations use a persistent GHCi session managed by a separate
-- oracle server process. This enables fast repeated queries without
-- spawning new processes.
--
-- This effect is IO-blind - interpreters handle the actual communication:
--
-- * Native: Use 'ExoMonad.GHCi.Interpreter.runGHCiIO' to connect to oracle
-- * WASM: Not available - 'NativeOnly' constraint prevents use in WASM builds
data GHCi m a where
  -- | Query the type of an expression.
  -- @:type expression@
  QueryType :: Text -> GHCi m (Either GHCiError Text)
  -- | Query information about a name (type, definition, instances).
  -- @:info name@
  QueryInfo :: Text -> GHCi m (Either GHCiError Text)
  -- | Query the kind of a type.
  -- @:kind type@
  QueryKind :: Text -> GHCi m (Either GHCiError Text)
  -- | Evaluate an expression (for pure expressions).
  -- Returns the result or error.
  Evaluate :: Text -> GHCi m (Either GHCiError Text)
  -- | Check if an expression compiles without executing.
  -- Useful for type-checking code generation results.
  CheckCompiles :: Text -> GHCi m (Either GHCiError Bool)
  -- | Load a module into the session.
  -- @:load Module.Name@
  LoadModule :: Text -> GHCi m (Either GHCiError ())
  -- | Reload all currently loaded modules.
  -- @:reload@
  ReloadModules :: GHCi m (Either GHCiError ())

makeSem ''GHCi

-- | Stub runner that logs operations and returns mock results.
--
-- This is a placeholder for testing and development. For real GHCi
-- functionality, use 'ExoMonad.GHCi.Interpreter.runGHCiIO' from ghci-interpreter.
runGHCiStub :: (Member Log effs) => Sem (GHCi ': effs) a -> Sem effs a
runGHCiStub = interpret $ \case
  QueryType expr -> do
    logInfo $ "[GHCi:stub] :type " <> expr
    pure $ Right $ expr <> " :: a -> b -> c"
  QueryInfo name -> do
    logInfo $ "[GHCi:stub] :info " <> name
    pure $ Right $ "-- info for " <> name
  QueryKind typ -> do
    logInfo $ "[GHCi:stub] :kind " <> typ
    pure $ Right $ typ <> " :: * -> *"
  Evaluate expr -> do
    logInfo $ "[GHCi:stub] evaluate: " <> expr
    pure $ Right "(stub result)"
  CheckCompiles expr -> do
    logInfo $ "[GHCi:stub] check compiles: " <> T.take 50 expr
    pure $ Right True
  LoadModule modName -> do
    logInfo $ "[GHCi:stub] :load " <> modName
    pure $ Right ()
  ReloadModules -> do
    logInfo "[GHCi:stub] :reload"
    pure $ Right ()

