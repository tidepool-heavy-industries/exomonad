{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module ExoMonad.Control.CircuitBreakerAdmin
  ( CbStatusGraph (..),
    CbResetGraph (..),
    circuitBreakerAdminHandlers,
    circuitBreakerResetHandlers,
    cbStatusLogic,
    cbResetLogic,
    CbStatusArgs (..),
    CbStatusResult (..),
    CbSessionInfo (..),
    CbResetArgs (..),
    CbResetResult (..),
  )
where

import Control.Monad.Freer (Eff, Member)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:?), (.=))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import ExoMonad.Control.Hook.CircuitBreaker (CircuitBreakerState (..))
import ExoMonad.Effect.CircuitBreaker (CircuitBreaker, getAllCBStates, resetAllCB, resetCBSession)
import ExoMonad.Graph.Generic (AsHandler, type (:-))
import ExoMonad.Graph.Generic.Core (EntryNode, ExitNode, LogicNode)
import ExoMonad.Graph.Goto (Goto, GotoChoice, To, gotoExit)
import ExoMonad.Graph.Types (Exit, Input, MCPExport, MCPRoleHint, MCPToolDef, UsesEffects, type (:@))
import ExoMonad.Role (Role (..))
import ExoMonad.Schema (HasJSONSchema (..), SchemaType (..), describeField, emptySchema, objectSchema)
import GHC.Generics (Generic)

-- ════════════════════════════════════════════════════════════════════════════
-- CB_STATUS
-- ════════════════════════════════════════════════════════════════════════════

data CbStatusArgs = CbStatusArgs
  { filterSession :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance HasJSONSchema CbStatusArgs where
  jsonSchema =
    objectSchema
      [ ("filter_session", describeField "Optional session ID to filter results" (emptySchema TString))
      ]
      []

instance FromJSON CbStatusArgs where
  parseJSON = withObject "CbStatusArgs" $ \v ->
    CbStatusArgs <$> v .:? "filter_session"

instance ToJSON CbStatusArgs where
  toJSON a = object ["filter_session" .= a.filterSession]

data CbSessionInfo = CbSessionInfo
  { sessionId :: Text,
    globalStops :: Int,
    stageRetries :: Map Text Int,
    lastStopTime :: UTCTime,
    active :: Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON CbSessionInfo where
  toJSON s =
    object
      [ "session_id" .= s.sessionId,
        "global_stops" .= s.globalStops,
        "stage_retries" .= s.stageRetries,
        "last_stop_time" .= s.lastStopTime,
        "active" .= s.active
      ]

data CbStatusResult = CbStatusResult
  { activeSessionCount :: Int,
    totalSessions :: Int,
    sessions :: [CbSessionInfo]
  }
  deriving (Show, Eq, Generic)

instance ToJSON CbStatusResult where
  toJSON r =
    object
      [ "active_session_count" .= r.activeSessionCount,
        "total_sessions" .= r.totalSessions,
        "sessions" .= r.sessions
      ]

data CbStatusGraph mode = CbStatusGraph
  { cbsEntry ::
      mode
        :- EntryNode CbStatusArgs
        :@ MCPExport
        :@ MCPToolDef '("cb_status", "Check status of the ExoMonad circuit breaker system.")
        :@ MCPRoleHint 'TL,
    cbsRun ::
      mode
        :- LogicNode
        :@ Input CbStatusArgs
        :@ UsesEffects '[CircuitBreaker, Goto Exit CbStatusResult],
    cbsExit :: mode :- ExitNode CbStatusResult
  }
  deriving (Generic)

circuitBreakerAdminHandlers :: (Member CircuitBreaker es) => CbStatusGraph (AsHandler es)
circuitBreakerAdminHandlers =
  CbStatusGraph
    { cbsEntry = (),
      cbsRun = cbStatusLogic,
      cbsExit = ()
    }

cbStatusLogic :: (Member CircuitBreaker es) => CbStatusArgs -> Eff es (GotoChoice '[To Exit CbStatusResult])
cbStatusLogic args = do
  allStates <- getAllCBStates

  let statesList = Map.elems allStates
      filtered = case args.filterSession of
        Nothing -> statesList
        Just sid -> filter (\s -> s.sessionId == sid) statesList

  let sessionInfos = map toInfo filtered
      activeCount = length $ filter (.stopHookActive) filtered
      totalCount = length filtered

  pure $
    gotoExit
      CbStatusResult
        { activeSessionCount = activeCount,
          totalSessions = totalCount,
          sessions = sessionInfos
        }
  where
    toInfo :: CircuitBreakerState -> CbSessionInfo
    toInfo s =
      CbSessionInfo
        { sessionId = s.sessionId,
          globalStops = s.globalStops,
          stageRetries = s.stageRetries,
          lastStopTime = s.lastStopTime,
          active = s.stopHookActive
        }

-- ════════════════════════════════════════════════════════════════════════════
-- CB_RESET
-- ════════════════════════════════════════════════════════════════════════════

data CbResetArgs = CbResetArgs
  { sessionId :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance HasJSONSchema CbResetArgs where
  jsonSchema =
    objectSchema
      [ ("session_id", describeField "Session ID to reset. If omitted, resets ALL sessions." (emptySchema TString))
      ]
      []

instance FromJSON CbResetArgs where
  parseJSON = withObject "CbResetArgs" $ \v ->
    CbResetArgs <$> v .:? "session_id"

instance ToJSON CbResetArgs where
  toJSON a = object ["session_id" .= a.sessionId]

data CbResetResult = CbResetResult
  { status :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON CbResetResult where
  toJSON r = object ["status" .= r.status]

data CbResetGraph mode = CbResetGraph
  { cbrEntry ::
      mode
        :- EntryNode CbResetArgs
        :@ MCPExport
        :@ MCPToolDef '("cb_reset", "Reset circuit breaker state for a session or globally.")
        :@ MCPRoleHint 'TL,
    cbrRun ::
      mode
        :- LogicNode
        :@ Input CbResetArgs
        :@ UsesEffects '[CircuitBreaker, Goto Exit CbResetResult],
    cbrExit :: mode :- ExitNode CbResetResult
  }
  deriving (Generic)

circuitBreakerResetHandlers :: (Member CircuitBreaker es) => CbResetGraph (AsHandler es)
circuitBreakerResetHandlers =
  CbResetGraph
    { cbrEntry = (),
      cbrRun = cbResetLogic,
      cbrExit = ()
    }

cbResetLogic :: (Member CircuitBreaker es) => CbResetArgs -> Eff es (GotoChoice '[To Exit CbResetResult])
cbResetLogic args = do
  case args.sessionId of
    Just sid -> do
      resetCBSession sid
      pure $ gotoExit CbResetResult {status = "Reset session: " <> sid}
    Nothing -> do
      resetAllCB
      pure $ gotoExit CbResetResult {status = "Reset ALL sessions"}
