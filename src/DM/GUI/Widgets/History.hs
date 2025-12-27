-- | History widget for the DM GUI
--
-- Displays the full scene beat log and session history.
module DM.GUI.Widgets.History
  ( historyTab
  , sceneBeatEntry
  , sceneSummaryEntry
  ) where

import Control.Concurrent.STM (atomically, readTVar)
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

  -- Get current state
  state <- liftIO $ atomically $ readTVar (gbState bridge)

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
  pure container

-- | Create an entry for a scene beat
sceneBeatEntry :: SceneBeat -> UI Element
sceneBeatEntry beat = do
  entry <- UI.div #. "narrative-entry"

  case beat of
    PlayerAction txt _ -> do
      void $ element entry #. "narrative-entry player-action"
        # set text ("> " <> T.unpack txt)

    NpcAction (NpcId npcName) txt -> do
      nameEl <- UI.span #. "npc-name" # set text (T.unpack npcName)
      speechEl <- UI.span # set text (T.unpack txt)
      void $ element entry #+ [element nameEl, UI.string ": ", element speechEl]

    DMNarration txt ->
      void $ element entry # set text (T.unpack txt)

    EnvironmentShift txt -> do
      void $ element entry # set style [("font-style", "italic")]
        # set text (T.unpack txt)

    Revelation _ -> do
      void $ element entry # set style [("color", "var(--accent)")]
        # set text "[A secret is revealed]"

    ClockTick (ClockId clockName) ticks -> do
      void $ element entry # set style [("color", "var(--warning)")]
        # set text ("[Clock: " <> T.unpack clockName <> " +" <> show ticks <> "]")

  pure entry

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
  PlayerAction txt _ -> T.unpack $ T.take 50 txt <> "..."
  NpcAction (NpcId npc) txt -> T.unpack $ npc <> ": " <> T.take 30 txt <> "..."
  DMNarration txt -> T.unpack $ T.take 50 txt <> "..."
  EnvironmentShift txt -> T.unpack $ "Environment: " <> T.take 30 txt
  Revelation _ -> "Secret revealed"
  ClockTick (ClockId name) n -> T.unpack $ name <> " +" <> T.pack (show n)
