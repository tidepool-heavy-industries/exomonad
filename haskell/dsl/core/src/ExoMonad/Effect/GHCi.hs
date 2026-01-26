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
-- myHandler :: (Member GHCi effs, NativeOnly) => Eff effs Text
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
--
module ExoMonad.Effect.GHCi
  ( -- * Platform Constraint
    NativeOnly

    -- * Effect
  , GHCi(..)

    -- * Smart Constructors
  , queryType
  , queryInfo
  , queryKind
  , evaluate
  , checkCompiles
  , loadModule
  , reloadModules

    -- * Error Types
  , GHCiError(..)

    -- * Wire Protocol
  , GHCiRequest(..)
  , GHCiResponse(..)

    -- * Stub Runner
  , runGHCiStub
  ) where

import Control.Monad.Freer (Eff, Member, interpret, send)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import ExoMonad.Effect (Log, logInfo)
import ExoMonad.Platform (NativeOnly)


-- ════════════════════════════════════════════════════════════════════════════
-- ERROR TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Errors that can occur during GHCi operations.
data GHCiError
  = GHCiSessionCrashed
      { gseCrashOutput :: Text
        -- ^ Last output before crash
      , gseExitCode :: Maybe Int
        -- ^ Exit code if available
      }
    -- ^ GHCi process crashed unexpectedly
  | GHCiTimeout
      { gteQuery :: Text
        -- ^ Query that timed out
      , gteTimeoutMs :: Int
        -- ^ Timeout in milliseconds
      }
    -- ^ Query exceeded timeout
  | GHCiParseError
      { gpeQuery :: Text
        -- ^ Query that failed
      , gpeGHCOutput :: Text
        -- ^ GHCi error output
      }
    -- ^ GHCi reported a parse/type error
  | GHCiLoadError
      { gleModule :: Text
        -- ^ Module that failed to load
      , gleErrors :: Text
        -- ^ Compilation errors
      }
    -- ^ Module failed to load
  | GHCiNotConnected
    -- ^ Cannot reach the oracle server
  | GHCiServerError
      { gseMessage :: Text
        -- ^ Error message from server
      }
    -- ^ Oracle server reported an error
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)


-- ════════════════════════════════════════════════════════════════════════════
-- WIRE PROTOCOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Request sent from client to oracle server.
data GHCiRequest
  = ReqQueryType Text
    -- ^ @:type expression@
  | ReqQueryInfo Text
    -- ^ @:info name@
  | ReqQueryKind Text
    -- ^ @:kind type@
  | ReqEvaluate Text
    -- ^ Evaluate expression
  | ReqCheckCompiles Text
    -- ^ Check if expression compiles (no execution)
  | ReqLoadModule Text
    -- ^ @:load ModuleName@
  | ReqReloadModules
    -- ^ @:reload@
  | ReqPing
    -- ^ Health check
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)


-- | Response sent from oracle server to client.
data GHCiResponse
  = RespSuccess Text
    -- ^ Successful query with text result
  | RespBool Bool
    -- ^ Boolean result (for CheckCompiles)
  | RespUnit
    -- ^ Successful operation with no result (for Load, Reload)
  | RespError GHCiError
    -- ^ Error occurred
  | RespPong
    -- ^ Response to Ping
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
data GHCi r where
  -- | Query the type of an expression.
  -- @:type expression@
  QueryType :: Text -> GHCi (Either GHCiError Text)

  -- | Query information about a name (type, definition, instances).
  -- @:info name@
  QueryInfo :: Text -> GHCi (Either GHCiError Text)

  -- | Query the kind of a type.
  -- @:kind type@
  QueryKind :: Text -> GHCi (Either GHCiError Text)

  -- | Evaluate an expression (for pure expressions).
  -- Returns the result or error.
  Evaluate :: Text -> GHCi (Either GHCiError Text)

  -- | Check if an expression compiles without executing.
  -- Useful for type-checking code generation results.
  CheckCompiles :: Text -> GHCi (Either GHCiError Bool)

  -- | Load a module into the session.
  -- @:load Module.Name@
  LoadModule :: Text -> GHCi (Either GHCiError ())

  -- | Reload all currently loaded modules.
  -- @:reload@
  ReloadModules :: GHCi (Either GHCiError ())


-- ════════════════════════════════════════════════════════════════════════════
-- SMART CONSTRUCTORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Query the type of an expression.
--
-- @
-- queryType "fmap"
-- -- Right "fmap :: Functor f => (a -> b) -> f a -> f b"
-- @
queryType :: (Member GHCi effs, NativeOnly) => Text -> Eff effs (Either GHCiError Text)
queryType = send . QueryType

-- | Query information about a name.
--
-- @
-- queryInfo "Functor"
-- -- Right "class Functor (f :: * -> *) where..."
-- @
queryInfo :: (Member GHCi effs, NativeOnly) => Text -> Eff effs (Either GHCiError Text)
queryInfo = send . QueryInfo

-- | Query the kind of a type.
--
-- @
-- queryKind "Maybe"
-- -- Right "Maybe :: * -> *"
-- @
queryKind :: (Member GHCi effs, NativeOnly) => Text -> Eff effs (Either GHCiError Text)
queryKind = send . QueryKind

-- | Evaluate an expression and return the result.
--
-- @
-- evaluate "1 + 1"
-- -- Right "2"
-- @
evaluate :: (Member GHCi effs, NativeOnly) => Text -> Eff effs (Either GHCiError Text)
evaluate = send . Evaluate

-- | Check if code compiles without executing it.
--
-- @
-- checkCompiles "let x :: Int; x = \"hello\" in x"
-- -- Right False
-- @
checkCompiles :: (Member GHCi effs, NativeOnly) => Text -> Eff effs (Either GHCiError Bool)
checkCompiles = send . CheckCompiles

-- | Load a module into the session.
--
-- @
-- loadModule "Data.Map"
-- -- Right ()
-- @
loadModule :: (Member GHCi effs, NativeOnly) => Text -> Eff effs (Either GHCiError ())
loadModule = send . LoadModule

-- | Reload all loaded modules.
--
-- Call this after file changes to refresh the session.
reloadModules :: (Member GHCi effs, NativeOnly) => Eff effs (Either GHCiError ())
reloadModules = send ReloadModules


-- ════════════════════════════════════════════════════════════════════════════
-- STUB RUNNER
-- ════════════════════════════════════════════════════════════════════════════

-- | Stub runner that logs operations and returns mock results.
--
-- This is a placeholder for testing and development. For real GHCi
-- functionality, use 'ExoMonad.GHCi.Interpreter.runGHCiIO' from ghci-interpreter.
runGHCiStub :: Member Log effs => Eff (GHCi ': effs) a -> Eff effs a
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
