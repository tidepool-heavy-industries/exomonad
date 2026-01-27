{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module ExoMonad.Control.CircuitBreakerAdmin
  ( CbStatusGraph(..)
  , CbResetGraph(..)
  , circuitBreakerAdminHandlers
  , circuitBreakerResetHandlers
  , cbStatusLogic
  , cbResetLogic
  , CbStatusArgs(..)
  , CbStatusResult(..)
  , CbSessionInfo(..)
  , CbResetArgs(..)
  , CbResetResult(..)
  ) where

import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), (.:?), withObject)
import Control.Monad.Freer (Eff, Member)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Time.Clock (UTCTime)

import ExoMonad.Graph.Generic (AsHandler, type (:-))
import ExoMonad.Graph.Generic.Core (EntryNode, ExitNode, LogicNode)
import ExoMonad.Graph.Types (type (:@), Input, UsesEffects, Exit, MCPExport, MCPToolDef, MCPRoleHint)
import ExoMonad.Schema (HasJSONSchema(..), objectSchema, emptySchema, SchemaType(..), describeField)
import ExoMonad.Graph.Goto (Goto, GotoChoice, To, gotoExit)
import ExoMonad.Role (Role(..))
import ExoMonad.Control.Hook.CircuitBreaker (CircuitBreakerState(..), SessionId)
import ExoMonad.Effect.CircuitBreaker (CircuitBreaker, getAllCBStates, resetCBSession, resetAllCB)

-- ════════════════════════════════════════════════════════════════════════════
-- CB_STATUS
-- ════════════════════════════════════════════════════════════════════════════

data CbStatusArgs = CbStatusArgs
  { filterSession :: Maybe Text
  } deriving (Show, Eq, Generic)

instance HasJSONSchema CbStatusArgs where
  jsonSchema = objectSchema
    [ ("filter_session", describeField "filter_session" "Optional session ID to filter results" (emptySchema TString))
    ] []

instance FromJSON CbStatusArgs where
  parseJSON = withObject "CbStatusArgs" $ \v ->
    CbStatusArgs <$> v .:? "filter_session"

instance ToJSON CbStatusArgs where
  toJSON args = object
    [ "filter_session" .= filterSession args
    ]

data CbSessionInfo = CbSessionInfo
  { sessionId :: Text
  , globalStops :: Int
  , stageRetries :: Map Text Int
  , lastStopTime :: UTCTime
  , active :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON CbSessionInfo where
  toJSON s = object
    [ "session_id" .= s.sessionId
    , "global_stops" .= s.globalStops
    , "stage_retries" .= s.stageRetries
    , "last_stop_time" .= s.lastStopTime
    , "active" .= s.active
    ]

data CbStatusResult = CbStatusResult
  { activeSessionCount :: Int
  , totalSessions :: Int
  , sessions :: [CbSessionInfo]
  } deriving (Show, Eq, Generic)

instance ToJSON CbStatusResult where
  toJSON r = object
    [ "active_session_count" .= r.activeSessionCount
    , "total_sessions" .= r.totalSessions
    , "sessions" .= r.sessions
    ]

data CbStatusGraph mode = CbStatusGraph
  { cbsEntry :: mode :- EntryNode CbStatusArgs
      :@ MCPExport
      :@ MCPToolDef '("cb_status", "Check status of the ExoMonad circuit breaker system.")
      :@ MCPRoleHint 'TL
  , cbsRun :: mode :- LogicNode
      :@ Input CbStatusArgs
      :@ UsesEffects '[CircuitBreaker, Goto Exit CbStatusResult]
  , cbsExit :: mode :- ExitNode CbStatusResult
  } deriving Generic

circuitBreakerAdminHandlers :: (Member CircuitBreaker es) => CbStatusGraph (AsHandler es)
circuitBreakerAdminHandlers = CbStatusGraph
  { cbsEntry = ()
  , cbsRun = cbStatusLogic
  , cbsExit = ()
  }

cbStatusLogic :: (Member CircuitBreaker es) => CbStatusArgs -> Eff es (GotoChoice '[To Exit CbStatusResult])
cbStatusLogic args = do
  allStates <- getAllCBStates
  
  let statesList = Map.elems allStates
      filtered = case args.filterSession of
        Nothing -> statesList
        Just sid -> filter (\s -> s.cbSessionId == sid) statesList

  let sessionInfos = map toInfo filtered
      activeCount = length $ filter (.cbStopHookActive) filtered
      totalCount = length filtered

  pure $ gotoExit CbStatusResult
    { activeSessionCount = activeCount
    , totalSessions = totalCount
    , sessions = sessionInfos
    }
  where
    toInfo :: CircuitBreakerState -> CbSessionInfo
    toInfo s = CbSessionInfo
      { sessionId = s.cbSessionId
      , globalStops = s.cbGlobalStops
      , stageRetries = s.cbStageRetries
      , lastStopTime = s.cbLastStopTime
      , active = s.cbStopHookActive
      }

-- ════════════════════════════════════════════════════════════════════════════
-- CB_RESET
-- ════════════════════════════════════════════════════════════════════════════

data CbResetArgs = CbResetArgs
  { sessionId :: Maybe Text
  } deriving (Show, Eq, Generic)

instance HasJSONSchema CbResetArgs where
  jsonSchema = objectSchema
    [ ("session_id", describeField "session_id" "Session ID to reset. If omitted, resets ALL sessions." (emptySchema TString))
    ] []

instance FromJSON CbResetArgs where
  parseJSON = withObject "CbResetArgs" $ \v ->
    CbResetArgs <$> v .:? "session_id"

instance ToJSON CbResetArgs where
  toJSON args = object
    [ "session_id" .= args.sessionId
    ]

data CbResetResult = CbResetResult
  { status :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON CbResetResult where
  toJSON r = object [ "status" .= r.status ]

data CbResetGraph mode = CbResetGraph
  { cbrEntry :: mode :- EntryNode CbResetArgs
      :@ MCPExport
      :@ MCPToolDef '("cb_reset", "Reset circuit breaker state for a session or globally.")
      :@ MCPRoleHint 'TL
  , cbrRun :: mode :- LogicNode
      :@ Input CbResetArgs
      :@ UsesEffects '[CircuitBreaker, Goto Exit CbResetResult]
  , cbrExit :: mode :- ExitNode CbResetResult
  } deriving Generic

circuitBreakerResetHandlers :: (Member CircuitBreaker es) => CbResetGraph (AsHandler es)
circuitBreakerResetHandlers = CbResetGraph
  { cbrEntry = ()
  , cbrRun = cbResetLogic
  , cbrExit = ()
  }

cbResetLogic :: (Member CircuitBreaker es) => CbResetArgs -> Eff es (GotoChoice '[To Exit CbResetResult])
cbResetLogic args = do
  case args.sessionId of
    Just sid -> do
      resetCBSession sid
      pure $ gotoExit CbResetResult { status = "Reset session: " <> sid }
    Nothing -> do
      resetAllCB
      pure $ gotoExit CbResetResult { status = "Reset ALL sessions" }
