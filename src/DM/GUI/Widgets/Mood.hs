-- | Mood header widget for the DM GUI
--
-- Displays the current DM mood state machine state with description.
-- Note: App.hs builds the mood header inline because it needs references
-- to the label/description elements for refreshing. This module provides
-- the display logic helpers.
module DM.GUI.Widgets.Mood
  ( moodDisplay
  , moodLabel
  , moodDescription
  ) where

import Data.Text (Text)

import DM.State

-- | Get the label for a mood
moodLabel :: DMMood -> Text
moodLabel (MoodScene _) = "SCENE"
moodLabel (MoodAction _ _) = "ACTION"
moodLabel (MoodAftermath _) = "AFTERMATH"
moodLabel (MoodDowntime _) = "DOWNTIME"
moodLabel (MoodTrauma _) = "TRAUMA"

-- | Get the description for a mood
moodDescription :: DMMood -> Text
moodDescription mood = case mood of
  MoodScene sv -> sceneDescription sv
  MoodAction av _ -> actionDescription av
  MoodAftermath av -> aftermathDescription av
  MoodDowntime dv -> downtimeDescription dv
  MoodTrauma tv -> traumaDescription tv

-- | Get label and description for a mood
moodDisplay :: DMMood -> (Text, Text)
moodDisplay mood = (moodLabel mood, moodDescription mood)

-- | Scene variant descriptions
sceneDescription :: SceneVariant -> Text
sceneDescription sv = case sv of
  Encounter{} -> "Someone demands attention"
  Opportunity{} -> "An opportunity presents itself"
  Discovery{} -> "A discovery has been made"

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

-- | Downtime variant descriptions
downtimeDescription :: DowntimeVariant -> Text
downtimeDescription dv = case dv of
  Recovery{} -> "Time to recover"
  Project{} -> "Working on a long-term project"
  Entanglement{} -> "Heat catches up"

-- | Trauma variant description
traumaDescription :: TraumaVariant -> Text
traumaDescription tv = "Breaking point: " <> tv.tvWhatBroke
