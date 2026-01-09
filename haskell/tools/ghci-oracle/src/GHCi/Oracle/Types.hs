-- | Wire protocol types for GHCi Oracle.
--
-- These types MUST match the wire protocol types in
-- @Tidepool.Effect.GHCi@ exactly for interoperability.
module GHCi.Oracle.Types
  ( -- * Error Types
    GHCiError(..)

    -- * Wire Protocol
  , GHCiRequest(..)
  , GHCiResponse(..)

    -- * Configuration
  , OracleConfig(..)
  , defaultConfig

    -- * Helpers
  , isLoadError
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)


-- ════════════════════════════════════════════════════════════════════════════
-- ERROR TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Errors that can occur during GHCi operations.
--
-- MUST match Tidepool.Effect.GHCi.GHCiError exactly.
data GHCiError
  = GHCiSessionCrashed
      { gseCrashOutput :: Text
      , gseExitCode :: Maybe Int
      }
  | GHCiTimeout
      { gteQuery :: Text
      , gteTimeoutMs :: Int
      }
  | GHCiParseError
      { gpeQuery :: Text
      , gpeGHCOutput :: Text
      }
  | GHCiLoadError
      { gleModule :: Text
      , gleErrors :: Text
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
-- MUST match Tidepool.Effect.GHCi.GHCiRequest exactly.
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
-- MUST match Tidepool.Effect.GHCi.GHCiResponse exactly.
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
  { ocPort :: Int
    -- ^ Port to listen on
  , ocProjectRoot :: FilePath
    -- ^ Project root (where to run ghci from)
  , ocInitialModules :: [Text]
    -- ^ Modules to load at startup
  , ocGhciCommand :: String
    -- ^ Command to start ghci (default: "cabal repl")
  , ocQueryTimeoutMs :: Int
    -- ^ Timeout for individual queries in milliseconds
  , ocStartupTimeoutMs :: Int
    -- ^ Timeout for session startup in milliseconds
  , ocRestartOnCrash :: Bool
    -- ^ Auto-restart on crash
  , ocMaxRestarts :: Int
    -- ^ Maximum restart attempts
  , ocVerbose :: Bool
    -- ^ Verbose logging
  }
  deriving stock (Show, Eq)


-- | Default configuration.
defaultConfig :: OracleConfig
defaultConfig = OracleConfig
  { ocPort = 9999
  , ocProjectRoot = "."
  , ocInitialModules = []
  , ocGhciCommand = "cabal repl"
  , ocQueryTimeoutMs = 10000
  , ocStartupTimeoutMs = 60000
  , ocRestartOnCrash = True
  , ocMaxRestarts = 3
  , ocVerbose = False
  }


-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Check if GHCi output indicates a load error.
isLoadError :: Text -> Bool
isLoadError t =
  "Failed" `T.isInfixOf` t || "Could not find module" `T.isInfixOf` t
