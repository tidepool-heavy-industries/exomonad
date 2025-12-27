-- | Mood header widget for the DM GUI
--
-- Displays the current DM mood state machine state with description.
module DM.GUI.Widgets.Mood
  ( moodHeader
  , moodLabel
  , moodDescription
  ) where

import Control.Concurrent.STM (atomically, readTVar)
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import DM.State
import Tidepool.GUI.Core (GUIBridge(..))

-- | Create the mood header panel
moodHeader :: GUIBridge WorldState -> UI Element
moodHeader bridge = do
  header <- UI.div #. "mood-header"

  -- Get current state
  state <- liftIO $ atomically $ readTVar (gbState bridge)
  let mood = state.mood
      (label, desc) = moodDisplay mood

  labelEl <- UI.span #. "mood-label" # set text (T.unpack label)
  descEl <- UI.span #. "mood-description" # set text (T.unpack desc)

  void $ element header #+ [element labelEl, UI.string " - ", element descEl]
  pure header

-- | Get the label for a mood
moodLabel :: DMMood -> Text
moodLabel (MoodScene _) = "SCENE"
moodLabel (MoodAction _ _) = "ACTION"
moodLabel (MoodAftermath _) = "AFTERMATH"
moodLabel (MoodTrauma _) = "TRAUMA"

-- | Get the description for a mood
moodDescription :: DMMood -> Text
moodDescription mood = case mood of
  MoodScene sv -> sceneDescription sv
  MoodAction av _ -> actionDescription av
  MoodAftermath av -> aftermathDescription av
  MoodTrauma tv -> traumaDescription tv

-- | Get label and description for a mood
moodDisplay :: DMMood -> (Text, Text)
moodDisplay mood = (moodLabel mood, moodDescription mood)

-- | Scene variant descriptions
sceneDescription :: SceneVariant -> Text
sceneDescription sv = case sv of
  JobOffer{} -> "A job offer is on the table"
  Complication{} -> "A complication has arisen"
  NpcEncounter{} -> "An unexpected encounter"
  ClockTriggered{} -> "A clock has triggered"
  FreePlay{} -> "Free exploration"
  SceneDowntime{} -> "Downtime between scores"

-- | Action variant descriptions
actionDescription :: ActionVariant -> Text
actionDescription av = case av of
  AvControlled{} -> "Controlled - You have the advantage"
  AvRisky{} -> "Risky - Things could go either way"
  AvDesperate{} -> "Desperate - The stakes are high"

-- | Aftermath variant descriptions
aftermathDescription :: AftermathVariant -> Text
aftermathDescription av = case av of
  AmClean{} -> "Clean getaway"
  AmCostly{} -> "Success, but at a cost"
  AmSetback{} -> "Things went wrong"
  AmDisaster{} -> "Disaster strikes"

-- | Trauma variant description
traumaDescription :: TraumaVariant -> Text
traumaDescription tv = "Breaking point: " <> tv.tvWhatBroke
