-- | History widget for the DM GUI
--
-- Displays the full scene beat log and session history.
module DM.GUI.Widgets.History
  ( historyTab
  , updateHistoryTab
  , sceneBeatEntry
  , sceneSummaryEntry
  ) where

import Control.Concurrent.STM (atomically, readTVar)
import Control.Monad (void)
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import DM.State
import Tidepool.GUI.Core (GUIBridge(..))

-- | Create the history tab content
historyTab :: GUIBridge WorldState -> UI Element
historyTab bridge = do
  container <- UI.div #. "history-tab"
  updateHistoryTab container bridge
  pure container

-- | Update the history tab with current state
updateHistoryTab :: Element -> GUIBridge WorldState -> UI ()
updateHistoryTab container bridge = do
  -- Clear existing content
  void $ element container # set children []

  -- Get current state
  state <- liftIO $ atomically $ readTVar bridge.gbState

  -- Current scene beats (if any)
  currentSceneSection <- case state.scene of
    Nothing -> UI.div # set text "No active scene"
    Just activeScene -> do
      section <- UI.div #. "history-section"
      sectionTitle <- UI.div #. "stat-label" # set text "Current Scene"

      beats <- mapM sceneBeatEntry (toList activeScene.sceneBeats)

      void $ element section #+ (element sectionTitle : map element beats)
      pure section

  -- Session history
  historySection <- UI.div #. "history-section"
  historyTitle <- UI.div #. "stat-label" # set text "Session History"

  summaries <- mapM sceneSummaryEntry (toList state.sessionHistory)

  void $ element historySection #+ (element historyTitle : map element summaries)

  void $ element container #+ [element currentSceneSection, element historySection]

-- | Create an entry for a scene beat
sceneBeatEntry :: SceneBeat -> UI Element
sceneBeatEntry beat = case beat of
  PlayerAction txt _ ->
    UI.div #. "narrative-entry player-action"
      # set text ("> " <> T.unpack txt)

  NpcAction (NpcId npcName) txt -> do
    entry <- UI.div #. "narrative-entry"
    nameEl <- UI.span #. "npc-name" # set text (T.unpack npcName)
    speechEl <- UI.span # set text (T.unpack txt)
    void $ element entry #+ [element nameEl, UI.string ": ", element speechEl]
    pure entry

  DMNarration txt ->
    UI.div #. "narrative-entry" # set text (T.unpack txt)

  EnvironmentShift txt ->
    UI.div #. "narrative-entry environment-shift"
      # set text (T.unpack txt)

  Revelation _ ->
    UI.div #. "narrative-entry revelation"
      # set text "[A secret is revealed]"

  ClockTick (ClockId clockName) ticks ->
    UI.div #. "narrative-entry clock-tick"
      # set text ("[Clock: " <> T.unpack clockName <> " +" <> show ticks <> "]")

-- | Create an entry for a scene summary
sceneSummaryEntry :: SceneSummary -> UI Element
sceneSummaryEntry summary = do
  entry <- UI.div #. "history-summary"

  summaryText <- UI.p # set text (T.unpack summary.summaryText)

  -- Key beats as a list
  beatsList <- UI.ul #. "key-beats"
  beatItems <- mapM (\b -> UI.li # set text (beatSummaryText b)) summary.summaryKeyBeats
  void $ element beatsList #+ map element beatItems

  void $ element entry #+ [element summaryText, element beatsList]
  pure entry

-- | Get summary text for a beat (shorter than full display)
beatSummaryText :: SceneBeat -> String
beatSummaryText beat = case beat of
  PlayerAction txt _ -> T.unpack $ truncateWithEllipsis 50 txt
  NpcAction (NpcId npc) txt -> T.unpack $ npc <> ": " <> truncateWithEllipsis 30 txt
  DMNarration txt -> T.unpack $ truncateWithEllipsis 50 txt
  EnvironmentShift txt -> T.unpack $ "Environment: " <> truncateWithEllipsis 30 txt
  Revelation _ -> "Secret revealed"
  ClockTick (ClockId name) n -> T.unpack $ name <> " +" <> T.pack (show n)

-- | Truncate text and add ellipsis only if truncated
truncateWithEllipsis :: Int -> Text -> Text
truncateWithEllipsis maxLen txt
  | T.length txt <= maxLen = txt
  | otherwise = T.take maxLen txt <> "..."
