-- | Wire protocol types for GHCi Oracle.
--
-- These types MUST match the wire protocol types in
-- @ExoMonad.Effect.GHCi@ exactly for interoperability.
module GHCi.Oracle.Types
  ( -- * Error Types
    GHCiError (..),

    -- * Wire Protocol
    GHCiRequest (..),
    GHCiResponse (..),

    -- * Configuration
    OracleConfig (..),
    defaultConfig,

    -- * Helpers
    isLoadError,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

-- ════════════════════════════════════════════════════════════════════════════
-- ERROR TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Errors that can occur during GHCi operations.
--
-- MUST match ExoMonad.Effect.GHCi.GHCiError exactly.
data GHCiError
  = GHCiSessionCrashed
      { gseCrashOutput :: Text,
        gseExitCode :: Maybe Int
      }
  | GHCiTimeout
      { gteQuery :: Text,
        gteTimeoutMs :: Int
      }
  | GHCiParseError
      { gpeQuery :: Text,
        gpeGHCOutput :: Text
      }
  | GHCiLoadError
      { gleModule :: Text,
        gleErrors :: Text
      }
  | GHCiNotConnected
  | GHCiServerError
      { gseMessage :: Text
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ════════════════════════════════════════════════════════════════════════════
-- WIRE PROTOCOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Request sent from client to oracle server.
--
-- MUST match ExoMonad.Effect.GHCi.GHCiRequest exactly.
data GHCiRequest
  = ReqQueryType Text
  | ReqQueryInfo Text
  | ReqQueryKind Text
  | ReqEvaluate Text
  | ReqCheckCompiles Text
  | ReqLoadModule Text
  | ReqReloadModules
  | ReqPing
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Response sent from oracle server to client.
--
-- MUST match ExoMonad.Effect.GHCi.GHCiResponse exactly.
data GHCiResponse
  = RespSuccess Text
  | RespBool Bool
  | RespUnit
  | RespError GHCiError
  | RespPong
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ════════════════════════════════════════════════════════════════════════════
-- CONFIGURATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Oracle server configuration.
data OracleConfig = OracleConfig
  { -- | Port to listen on
    ocPort :: Int,
    -- | Project root (where to run ghci from)
    ocProjectRoot :: FilePath,
    -- | Modules to load at startup
    ocInitialModules :: [Text],
    -- | Command to start ghci (default: "cabal repl")
    ocGhciCommand :: String,
    -- | Timeout for individual queries in milliseconds
    ocQueryTimeoutMs :: Int,
    -- | Timeout for session startup in milliseconds
    ocStartupTimeoutMs :: Int,
    -- | Auto-restart on crash
    ocRestartOnCrash :: Bool,
    -- | Maximum restart attempts
    ocMaxRestarts :: Int,
    -- | Verbose logging
    ocVerbose :: Bool
  }
  deriving stock (Show, Eq)

-- | Default configuration.
defaultConfig :: OracleConfig
defaultConfig =
  OracleConfig
    { ocPort = 9999,
      ocProjectRoot = ".",
      ocInitialModules = [],
      ocGhciCommand = "cabal repl",
      ocQueryTimeoutMs = 10000,
      ocStartupTimeoutMs = 60000,
      ocRestartOnCrash = True,
      ocMaxRestarts = 3,
      ocVerbose = False
    }

-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Check if GHCi output indicates a load error.
isLoadError :: Text -> Bool
isLoadError t =
  "Failed" `T.isInfixOf` t || "Could not find module" `T.isInfixOf` t
