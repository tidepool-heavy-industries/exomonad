{-# LANGUAGE StrictData #-}
-- | Core types for the Tidepool GUI bridge
--
-- This module provides the communication infrastructure between
-- a game loop (running effects) and a threepenny-gui interface.
-- The bridge uses TVar for state sharing and MVar for request/response
-- synchronization.
module Tidepool.GUI.Core
  ( -- * Bridge type
    GUIBridge(..)
  , newGUIBridge
    -- * Request/Response types
  , PendingRequest(..)
  , RequestResponse(..)
    -- * Debug logging
  , DebugEntry(..)
  , LogLevel(..)
  , addDebugEntry
    -- * Narrative
  , addNarrative
  , clearNarrative
  ) where

import Control.Concurrent.MVar (MVar, newEmptyMVar)
import Control.Concurrent.STM (TVar, newTVarIO, atomically, modifyTVar')
import Data.Aeson (Value)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)

-- | Bridge between game loop and GUI
--
-- The game loop writes to 'gbPendingRequest' when it needs input,
-- then blocks on 'gbRequestResponse'. The GUI polls 'gbPendingRequest',
-- renders the appropriate input widget, and puts the response into
-- 'gbRequestResponse' when the user interacts.
data GUIBridge state = GUIBridge
  { gbState          :: TVar state
    -- ^ The game state (e.g., WorldState). GUI reads this for display.
  , gbPendingRequest :: TVar (Maybe PendingRequest)
    -- ^ Current pending input request, if any
  , gbRequestResponse :: MVar RequestResponse
    -- ^ Response channel - game loop blocks here, GUI puts response
  , gbNarrativeLog   :: TVar (Seq Text)
    -- ^ Narrative text log for display
  , gbDebugLog       :: TVar (Seq DebugEntry)
    -- ^ Debug log entries
  , gbLLMActive      :: TVar Bool
    -- ^ Whether an LLM call is in progress (for loading spinner)
  }

-- | Create a new GUI bridge with initial state
newGUIBridge :: state -> IO (GUIBridge state)
newGUIBridge initialState = do
  stateVar <- newTVarIO initialState
  pendingVar <- newTVarIO Nothing
  responseVar <- newEmptyMVar
  narrativeVar <- newTVarIO Seq.empty
  debugVar <- newTVarIO Seq.empty
  llmActiveVar <- newTVarIO False
  pure GUIBridge
    { gbState = stateVar
    , gbPendingRequest = pendingVar
    , gbRequestResponse = responseVar
    , gbNarrativeLog = narrativeVar
    , gbDebugLog = debugVar
    , gbLLMActive = llmActiveVar
    }

-- | A pending input request from the game loop
data PendingRequest
  = PendingChoice Text [(Text, Int)]
    -- ^ Choice selection: prompt and (label, index) pairs
  | PendingText Text
    -- ^ Free-form text input with prompt
  deriving (Show, Eq)

-- | Response from the GUI to the game loop
data RequestResponse
  = ChoiceResponse Int
    -- ^ Selected choice index
  | TextResponse Text
    -- ^ Entered text
  deriving (Show, Eq)

-- | Log levels for debug entries
data LogLevel
  = Debug
  | Info
  | Warn
  | Error
  deriving (Show, Eq, Ord)

-- | A debug log entry
data DebugEntry = DebugEntry
  { deTimestamp :: UTCTime
  , deLevel     :: LogLevel
  , deMessage   :: Text
  , deContext   :: Maybe Value
    -- ^ Optional JSON context (e.g., LLM request/response)
  }
  deriving (Show)

-- | Add a debug entry to the log
addDebugEntry :: GUIBridge state -> LogLevel -> Text -> Maybe Value -> IO ()
addDebugEntry bridge level msg ctx = do
  now <- getCurrentTime
  let entry = DebugEntry now level msg ctx
  atomically $ modifyTVar' (gbDebugLog bridge) (Seq.|> entry)

-- | Add narrative text to the log
addNarrative :: GUIBridge state -> Text -> IO ()
addNarrative bridge txt =
  atomically $ modifyTVar' (gbNarrativeLog bridge) (Seq.|> txt)

-- | Clear the narrative log
clearNarrative :: GUIBridge state -> IO ()
clearNarrative bridge =
  atomically $ modifyTVar' (gbNarrativeLog bridge) (const Seq.empty)
