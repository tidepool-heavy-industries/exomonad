{-# LANGUAGE StrictData #-}
-- | Core types for the Tidepool GUI bridge
--
-- This module provides the communication infrastructure between
-- a game loop (running effects) and a threepenny-gui interface.
-- The bridge uses TVar for state sharing and MVar for request/response
-- synchronization.
--
-- = Communication Pattern
--
-- @
--   Game Loop                    GUI
--   ─────────                    ───
--       │                          │
--       │  writeTVar pending ────► │ (polls for requests)
--       │                          │
--       │  takeMVar response ◄──── │ putMVar response
--       │  (blocks)                │ (user interaction)
--       │                          │
--       ▼                          ▼
-- @
--
-- = Thread Safety
--
-- All operations are thread-safe:
-- * TVars use STM for atomic reads/writes
-- * The response MVar ensures proper synchronization
-- * State updates use 'modifyTVar'' (strict) to avoid space leaks
--
module Tidepool.GUI.Core
  ( -- * Bridge type
    GUIBridge(..)
    -- * Safe response submission
  , safeSubmitResponse
  , newGUIBridge
    -- * Request/Response types
  , PendingRequest(..)
  , RequestResponse(..)
    -- * State management
  , updateState
  , updateStateM
  , readState
  , getStateVersion
    -- * Change detection
  , hasStateChanged
  , markStateObserved
    -- * LLM activity
  , setLLMActive
  , withLLMActive
    -- * Debug logging
  , DebugEntry(..)
  , LogLevel(..)
  , addDebugEntry
  , logDebug
  , logInfo
  , logWarn
  , logError
  , logDebugWithContext
  , logInfoWithContext
    -- * Narrative
  , addNarrative
  , clearNarrative
    -- * Suggested actions
  , setSuggestedActions
  , clearSuggestedActions
    -- * Log management
  , trimDebugLog
  , trimNarrativeLog
  , defaultMaxLogEntries
  ) where

import Control.Concurrent.MVar (MVar, newEmptyMVar, tryPutMVar)
import Control.Concurrent.STM (TVar, newTVarIO, atomically, modifyTVar', readTVar, writeTVar)
import Control.Exception (bracket_)
import Data.Aeson (Value)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import Data.Time (UTCTime, getCurrentTime, formatTime, defaultTimeLocale)
import System.IO (stderr)

-- | Bridge between game loop and GUI
--
-- The game loop writes to 'gbPendingRequest' when it needs input,
-- then blocks on 'gbRequestResponse'. The GUI polls 'gbPendingRequest',
-- renders the appropriate input widget, and puts the response into
-- 'gbRequestResponse' when the user interacts.
--
-- = State Versioning
--
-- The bridge tracks a version number that increments on every state change.
-- The GUI can use this to detect changes efficiently:
--
-- @
-- lastSeen <- getStateVersion bridge
-- -- ... later ...
-- changed <- hasStateChanged bridge lastSeen
-- when changed $ refreshUI
-- @
--
data GUIBridge state = GUIBridge
  { gbState          :: TVar state
    -- ^ The game state (e.g., WorldState). GUI reads this for display.
  , gbStateVersion   :: TVar Int
    -- ^ Version counter, incremented on every state change.
    -- Used for efficient change detection.
  , gbPendingRequest :: TVar (Maybe PendingRequest)
    -- ^ Current pending input request, if any
  , gbRequestResponse :: MVar RequestResponse
    -- ^ Response channel - game loop blocks here, GUI puts response
  , gbNarrativeLog   :: TVar (Seq Text)
    -- ^ Narrative text log for display (generic text entries)
  , gbNarrativeVersion :: TVar Int
    -- ^ Version counter for narrative changes
  , gbDebugLog       :: TVar (Seq DebugEntry)
    -- ^ Debug log entries
  , gbDebugVersion   :: TVar Int
    -- ^ Version counter for debug log changes
  , gbLLMActive      :: TVar Bool
    -- ^ Whether an LLM call is in progress (for loading spinner)
  , gbSuggestedActions :: TVar [Text]
    -- ^ Suggested actions from last LLM response (for quick-pick buttons)
  , gbSuggestionsVersion :: TVar Int
    -- ^ Version counter for suggestions changes
  , gbCharacterCreationResult :: MVar Value
    -- ^ Character creation result (JSON-encoded CharacterChoices)
    -- Used for one-time startup flow, bypasses normal request/response
  }

-- | Create a new GUI bridge with initial state
newGUIBridge :: state -> IO (GUIBridge state)
newGUIBridge initialState = do
  stateVar <- newTVarIO initialState
  stateVerVar <- newTVarIO 0
  pendingVar <- newTVarIO Nothing
  responseVar <- newEmptyMVar
  narrativeVar <- newTVarIO Seq.empty
  narrativeVerVar <- newTVarIO 0
  debugVar <- newTVarIO Seq.empty
  debugVerVar <- newTVarIO 0
  llmActiveVar <- newTVarIO False
  suggestionsVar <- newTVarIO []
  suggestionsVerVar <- newTVarIO 0
  charCreationVar <- newEmptyMVar
  pure GUIBridge
    { gbState = stateVar
    , gbStateVersion = stateVerVar
    , gbPendingRequest = pendingVar
    , gbRequestResponse = responseVar
    , gbNarrativeLog = narrativeVar
    , gbNarrativeVersion = narrativeVerVar
    , gbDebugLog = debugVar
    , gbDebugVersion = debugVerVar
    , gbLLMActive = llmActiveVar
    , gbSuggestedActions = suggestionsVar
    , gbSuggestionsVersion = suggestionsVerVar
    , gbCharacterCreationResult = charCreationVar
    }

-- | Safely submit a response, ignoring if one is already pending
--
-- This prevents blocking on double-clicks or rapid submissions.
-- Uses 'tryPutMVar' which returns immediately if the MVar is full.
safeSubmitResponse :: GUIBridge state -> RequestResponse -> IO ()
safeSubmitResponse bridge response = do
  TIO.hPutStrLn stderr $ "[safeSubmitResponse] Attempting to put: " <> pack (show response)
  success <- tryPutMVar bridge.gbRequestResponse response
  TIO.hPutStrLn stderr $ "[safeSubmitResponse] tryPutMVar result: " <> pack (show success)

-- | A pending input request from the game loop
--
-- Note: BetweenScenes is handled differently - the display context
-- is stored in WorldState.betweenScenesDisplay, and the choice
-- uses a regular PendingChoice with labeled options.
data PendingRequest
  = PendingChoice Text [(Text, Int)]
    -- ^ Choice selection: prompt and (label, index) pairs
  | PendingText Text
    -- ^ Free-form text input with prompt
  | PendingDice Text [(Int, Int, Text)]
    -- ^ Dice selection: prompt and (die value, index, hint) triples
    -- Hints are LLM-generated previews of what each outcome means
  | PendingPhoto Text
    -- ^ Photo upload request with prompt
    -- Used by tidying agent to request photos of spaces/items
  | PendingCharacterCreation
    -- ^ Character creation flow (full-screen takeover)
  | PendingCustom Text Value
    -- ^ Custom request: (type tag, JSON payload)
    -- Used for agent-specific UI flows like Tidying's Question DSL.
    -- The type tag helps the GUI dispatch to the right renderer.
  deriving (Show, Eq)

-- | Response from the GUI to the game loop
data RequestResponse
  = ChoiceResponse Int
    -- ^ Selected choice index
  | TextResponse Text
    -- ^ Entered text
  | PhotoResponse Text Text
    -- ^ Photo upload: (base64 data, mime type)
  | TextWithPhotoResponse Text Text Text
    -- ^ Text with optional photo: (text, base64 data, mime type)
    -- Used when user attaches a photo to their text message
  | CustomResponse Value
    -- ^ Custom response (JSON payload)
    -- Used for agent-specific responses like Tidying's Answer type.
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
--
-- Also increments the debug version for change detection.
-- Prints to stderr for terminal visibility.
addDebugEntry :: GUIBridge state -> LogLevel -> Text -> Maybe Value -> IO ()
addDebugEntry bridge level msg ctx = do
  now <- getCurrentTime
  let entry = DebugEntry now level msg ctx
      timeStr = formatTime defaultTimeLocale "%H:%M:%S" now
      levelStr = case level of
        Debug -> "DEBUG"
        Info  -> "INFO "
        Warn  -> "WARN "
        Error -> "ERROR"
  -- Print to stderr for terminal visibility
  TIO.hPutStrLn stderr $ "[" <> levelStr <> " " <> pack timeStr <> "] " <> msg
  -- Add to GUI debug log
  atomically $ do
    modifyTVar' bridge.gbDebugLog (Seq.|> entry)
    modifyTVar' bridge.gbDebugVersion (+1)

-- | Add narrative text to the log
--
-- Also increments the narrative version for change detection.
addNarrative :: GUIBridge state -> Text -> IO ()
addNarrative bridge txt = atomically $ do
  modifyTVar' bridge.gbNarrativeLog (Seq.|> txt)
  modifyTVar' bridge.gbNarrativeVersion (+1)

-- | Clear the narrative log
clearNarrative :: GUIBridge state -> IO ()
clearNarrative bridge = atomically $ do
  modifyTVar' bridge.gbNarrativeLog (const Seq.empty)
  modifyTVar' bridge.gbNarrativeVersion (+1)

-- | Set the suggested actions for display
--
-- These are shown as quick-pick buttons in the input area.
setSuggestedActions :: GUIBridge state -> [Text] -> IO ()
setSuggestedActions bridge actions = atomically $ do
  writeTVar bridge.gbSuggestedActions actions
  modifyTVar' bridge.gbSuggestionsVersion (+1)

-- | Clear the suggested actions
clearSuggestedActions :: GUIBridge state -> IO ()
clearSuggestedActions bridge = atomically $ do
  writeTVar bridge.gbSuggestedActions []
  modifyTVar' bridge.gbSuggestionsVersion (+1)

-- | Update the game state
--
-- Uses a modify function to safely update the state.
-- Increments the state version for change detection.
updateState :: GUIBridge state -> (state -> state) -> IO ()
updateState bridge f = atomically $ do
  modifyTVar' bridge.gbState f
  modifyTVar' bridge.gbStateVersion (+1)

-- | Update the game state with an IO action
--
-- For when your update function needs IO (e.g., getting current time).
-- Increments the state version.
updateStateM :: GUIBridge state -> (state -> IO state) -> IO ()
updateStateM bridge f = do
  oldState <- atomically $ readTVar bridge.gbState
  newState <- f oldState
  atomically $ do
    writeTVar bridge.gbState newState
    modifyTVar' bridge.gbStateVersion (+1)

-- | Read the current game state
readState :: GUIBridge state -> IO state
readState bridge =
  atomically $ readTVar bridge.gbState

-- | Get the current state version number
--
-- Use with 'hasStateChanged' for efficient change detection.
getStateVersion :: GUIBridge state -> IO Int
getStateVersion bridge =
  atomically $ readTVar bridge.gbStateVersion

-- | Check if state has changed since a given version
--
-- @
-- lastSeen <- getStateVersion bridge
-- -- ... later in polling loop ...
-- changed <- hasStateChanged bridge lastSeen
-- when changed $ do
--   refreshUI
--   newVersion <- getStateVersion bridge
--   -- update lastSeen reference
-- @
hasStateChanged :: GUIBridge state -> Int -> IO Bool
hasStateChanged bridge lastSeenVersion = do
  currentVersion <- atomically $ readTVar bridge.gbStateVersion
  pure (currentVersion /= lastSeenVersion)

-- | Mark the current state as observed
--
-- Returns the new version number to track.
markStateObserved :: GUIBridge state -> IO Int
markStateObserved bridge =
  atomically $ readTVar bridge.gbStateVersion

-- | Set the LLM active flag
--
-- When true, the GUI should show a loading indicator.
setLLMActive :: GUIBridge state -> Bool -> IO ()
setLLMActive bridge active =
  atomically $ writeTVar bridge.gbLLMActive active

-- | Run an action with LLM active indicator
--
-- Shows loading during the action, hides it after (even on exception).
withLLMActive :: GUIBridge state -> IO a -> IO a
withLLMActive bridge action =
  bracket_
    (setLLMActive bridge True)
    (setLLMActive bridge False)
    action

-- | Log a debug message
logDebug :: GUIBridge state -> Text -> IO ()
logDebug bridge msg = addDebugEntry bridge Debug msg Nothing

-- | Log an info message
logInfo :: GUIBridge state -> Text -> IO ()
logInfo bridge msg = addDebugEntry bridge Info msg Nothing

-- | Log a warning message
logWarn :: GUIBridge state -> Text -> IO ()
logWarn bridge msg = addDebugEntry bridge Warn msg Nothing

-- | Log an error message
logError :: GUIBridge state -> Text -> IO ()
logError bridge msg = addDebugEntry bridge Error msg Nothing

-- | Log a debug message with JSON context
logDebugWithContext :: GUIBridge state -> Text -> Value -> IO ()
logDebugWithContext bridge msg ctx = addDebugEntry bridge Debug msg (Just ctx)

-- | Log an info message with JSON context
logInfoWithContext :: GUIBridge state -> Text -> Value -> IO ()
logInfoWithContext bridge msg ctx = addDebugEntry bridge Info msg (Just ctx)

-- | Trim the debug log to a maximum number of entries
--
-- Keeps the most recent entries. Useful to prevent memory leaks
-- in long-running sessions.
trimDebugLog :: GUIBridge state -> Int -> IO ()
trimDebugLog bridge maxEntries =
  atomically $ modifyTVar' bridge.gbDebugLog $ \log ->
    if Seq.length log > maxEntries
      then Seq.drop (Seq.length log - maxEntries) log
      else log

-- | Trim the narrative log to a maximum number of entries
--
-- Keeps the most recent entries.
trimNarrativeLog :: GUIBridge state -> Int -> IO ()
trimNarrativeLog bridge maxEntries =
  atomically $ modifyTVar' bridge.gbNarrativeLog $ \log ->
    if Seq.length log > maxEntries
      then Seq.drop (Seq.length log - maxEntries) log
      else log

-- | Default maximum log entries before trimming
--
-- A reasonable default that balances memory usage with history depth.
defaultMaxLogEntries :: Int
defaultMaxLogEntries = 500
