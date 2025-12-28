-- | Rich narrative widget for the DM GUI
--
-- This module provides narrative rendering with:
-- - NPC speech bubbles with character names
-- - Player action styling (italicized, prefixed)
-- - Environment descriptions
-- - Clock tick notifications
-- - Revelation markers
--
-- Unlike the generic 'Tidepool.GUI.Widgets.narrativePane' which renders
-- plain text, this widget understands DM domain types and renders them
-- appropriately.
module DM.GUI.Widgets.Narrative
  ( -- * Rich narrative pane
    dmNarrativePane
  , updateDMNarrative
    -- * Individual entry rendering
  , renderSceneBeat
  , renderNpcSpeech
  , renderPlayerAction
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

-- | Create a rich narrative pane for DM
--
-- This reads from the current scene's beats and renders them with
-- appropriate styling for each beat type.
dmNarrativePane :: GUIBridge WorldState -> UI Element
dmNarrativePane bridge = do
  container <- UI.div #. "narrative-pane dm-narrative"

  -- Initial render
  updateDMNarrative container bridge

  pure container

-- | Update the DM narrative pane with current scene beats
updateDMNarrative :: Element -> GUIBridge WorldState -> UI ()
updateDMNarrative container bridge = do
  state <- liftIO $ atomically $ readTVar bridge.gbState

  -- Clear existing content
  void $ element container # set children []

  -- Render scene beats if we have an active scene
  case state.scene of
    Nothing -> do
      -- No active scene - show a placeholder
      placeholder <- UI.div #. "narrative-placeholder"
        # set text "The story awaits..."
      void $ element container #+ [element placeholder]

    Just activeScene -> do
      -- Render each beat
      beatEls <- mapM renderSceneBeat (toList activeScene.sceneBeats)
      void $ element container #+ map element beatEls

  -- Auto-scroll to bottom
  runFunction $ ffi "$(%1).scrollTop($(%1)[0].scrollHeight)" container

-- | Render a single scene beat with appropriate styling
renderSceneBeat :: SceneBeat -> UI Element
renderSceneBeat beat = case beat of
  PlayerAction txt _intent ->
    renderPlayerAction txt

  NpcAction npcId txt ->
    renderNpcSpeech npcId txt

  DMNarration txt ->
    renderNarration txt

  EnvironmentShift txt ->
    renderEnvironment txt

  Revelation secret ->
    renderRevelation secret

  ClockTick clockId ticks ->
    renderClockTick clockId ticks

-- | Render a player action
--
-- Shows as italicized text with a ">" prefix to indicate player agency.
renderPlayerAction :: Text -> UI Element
renderPlayerAction txt = do
  entry <- UI.div #. "narrative-entry player-action"
  void $ element entry # set text ("> " <> T.unpack txt)
  pure entry

-- | Render NPC speech as a styled bubble
--
-- Creates a speech bubble with the NPC's name as a header.
renderNpcSpeech :: NpcId -> Text -> UI Element
renderNpcSpeech (NpcId npcName) speech = do
  bubble <- UI.div #. "speech-bubble"

  -- NPC name header
  nameEl <- UI.span #. "npc-name"
    # set text (T.unpack $ formatNpcName npcName)

  -- Speech content
  speechEl <- UI.div #. "npc-speech"
    # set text (T.unpack speech)

  void $ element bubble #+ [element nameEl, element speechEl]
  pure bubble

-- | Format an NPC name for display
--
-- Converts snake_case to Title Case
formatNpcName :: Text -> Text
formatNpcName name = T.toTitle $ T.replace "_" " " name

-- | Render DM narration (descriptive text)
renderNarration :: Text -> UI Element
renderNarration txt = do
  entry <- UI.div #. "narrative-entry dm-narration"
  void $ element entry # set text (T.unpack txt)
  pure entry

-- | Render an environment shift
--
-- Styled differently to indicate a change in setting/atmosphere.
renderEnvironment :: Text -> UI Element
renderEnvironment txt = do
  entry <- UI.div #. "narrative-entry environment-shift"
  void $ element entry # set text (T.unpack txt)
  pure entry

-- | Render a revelation marker
--
-- Shows that a secret has been revealed, without spoiling the content.
renderRevelation :: Secret -> UI Element
renderRevelation _secret = do
  entry <- UI.div #. "narrative-entry revelation"
  icon <- UI.span #. "revelation-icon" # set text "★"
  textEl <- UI.span # set text " A secret has been revealed..."
  void $ element entry #+ [element icon, element textEl]
  pure entry

-- | Render a clock tick notification
--
-- Shows when a clock advances, styled as a system message.
renderClockTick :: ClockId -> Int -> UI Element
renderClockTick (ClockId clockName) ticks = do
  entry <- UI.div #. "narrative-entry clock-tick"

  let tickText = if ticks == 1
        then "advances"
        else "advances by " <> show ticks

  void $ element entry # set text
    ("[" <> T.unpack clockName <> " " <> tickText <> "]")
  pure entry
