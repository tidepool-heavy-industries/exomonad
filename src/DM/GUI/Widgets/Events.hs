-- | State event rendering for the DM GUI
--
-- This module renders DMEvent state changes as styled elements in the narrative.
-- Events use a special text format that can be detected and rendered with CSS.
--
-- = Event Format
--
-- Events in the narrative log use the format:
-- @[EVENT:TYPE:SEVERITY]content@
--
-- Where:
-- * TYPE is: STRESS, TRAUMA, HEAT, WANTED, COIN, DICE, BARGAIN, CLOCK
-- * SEVERITY is: critical, warning, positive, info
--
-- The renderer detects this format and applies appropriate CSS classes.
module DM.GUI.Widgets.Events
  ( -- * Event formatting (for emitting to narrative log)
    formatEvent
  , formatEventEntry
    -- * Event rendering (for display)
  , isEventEntry
  , renderEventEntry
    -- * Event severity
  , EventSeverity(..)
  , eventSeverity
  ) where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import DM.State (Trauma(..), OutcomeTier(..))
import DM.Tools (DMEvent(..))

-- | Event severity levels for styling
data EventSeverity
  = SeverityCritical  -- ^ Trauma, max stress, disaster outcomes
  | SeverityWarning   -- ^ Stress/heat increases, partial success
  | SeverityPositive  -- ^ Coin gain, stress recovery
  | SeverityInfo      -- ^ Neutral changes, clock advances
  | SeverityBargain   -- ^ Out of dice, bargain required
  deriving (Show, Eq)

-- | Format an event for the narrative log
--
-- Returns Nothing for events that shouldn't be displayed in narrative
formatEvent :: DMEvent -> Maybe Text
formatEvent event = case event of
  -- Internal events - don't display
  RandomChoice _ _ -> Nothing
  MoodTransition _ _ _ -> Nothing  -- Handled by mood header

  -- Die spent - show outcome tier and narrative prominently
  DieSpent dieVal tier narrative ->
    Just $ formatEventEntry "OUTCOME" (tierToSeverity tier) $
      tierLabel tier <> " (die: " <> T.pack (show dieVal) <> ")\n" <> narrative

  -- Compression - shown as divider
  SceneCompressed summary ->
    Just $ formatEventEntry "SCENE" SeverityInfo $
      "Scene compressed: " <> summary

  -- Clock completed - significant event
  ClockCompleted clockId clockName _consequence ->
    Just $ formatEventEntry "CLOCK" SeverityCritical $
      "⚠ " <> clockName <> " — Clock filled! [" <> clockId <> "]"

  -- State changes - always display
  StressChanged from to reason
    | to > from -> Just $ formatEventEntry "STRESS" severity $
        "+" <> T.pack (show (to - from)) <> " stress — " <> reason <>
        if to >= 9 then " — MAXIMUM STRESS" else ""
    | to < from -> Just $ formatEventEntry "STRESS" SeverityPositive $
        T.pack (show (to - from)) <> " stress — " <> reason
    | otherwise -> Nothing
    where
      severity = if to >= 8 then SeverityCritical
                 else if to >= 5 then SeverityWarning
                 else SeverityInfo

  TraumaTriggered (Trauma traumaName) trigger breakingPoint ->
    Just $ formatEventEntry "TRAUMA" SeverityCritical $
      breakingPoint <> "\n\nYou are now: " <> T.toUpper traumaName <>
      "\n\n" <> trigger

  HeatChanged from to reason
    | to > from -> Just $ formatEventEntry "HEAT" severity $
        "+" <> T.pack (show (to - from)) <> " heat — " <> reason <>
        if to >= 10 then " — MAXIMUM HEAT" else ""
    | to < from -> Just $ formatEventEntry "HEAT" SeverityPositive $
        T.pack (show (to - from)) <> " heat — " <> reason
    | otherwise -> Nothing
    where
      severity = if to >= 8 then SeverityCritical
                 else if to >= 5 then SeverityWarning
                 else SeverityInfo

  WantedChanged from to reason
    | to > from -> Just $ formatEventEntry "WANTED" SeverityCritical $
        "Wanted level increased! — " <> reason <>
        " [" <> T.pack (show from) <> " → " <> T.pack (show to) <> "]"
    | otherwise -> Nothing

  CoinChanged from to reason
    | to > from -> Just $ formatEventEntry "COIN" SeverityPositive $
        "+" <> T.pack (show (to - from)) <> " coin — " <> reason
    | to < from -> Just $ formatEventEntry "COIN" SeverityWarning $
        T.pack (show (to - from)) <> " coin — " <> reason
    | otherwise -> Nothing

  DicePoolDepleted context ->
    Just $ formatEventEntry "DICE" SeverityBargain $
      "No dice remaining — " <> context

  BargainOffered context canRetreat ->
    Just $ formatEventEntry "BARGAIN" SeverityBargain $
      context <>
      if canRetreat
        then "\n\nYou can retreat to safety, or make a deal..."
        else "\n\nNo escape. You must bargain or collapse."

  ClockAdvanced _clockId clockName oldFilled newFilled total ->
    Just $ formatEventEntry "CLOCK" severity $
      clockName <> " — " <> T.pack (show newFilled) <> "/" <>
      T.pack (show total) <>
      if newFilled >= total
        then " ⚠ FILLED!"
        else " [+" <> T.pack (show (newFilled - oldFilled)) <> "]"
    where
      severity = if newFilled >= total then SeverityCritical
                 else if newFilled >= total - 1 then SeverityWarning
                 else SeverityInfo

  -- Narrative text - pass through directly (no special formatting)
  NarrativeAdded txt -> Just txt

-- | Format an event entry with type and severity markers
formatEventEntry :: Text -> EventSeverity -> Text -> Text
formatEventEntry eventType severity content =
  "[EVENT:" <> eventType <> ":" <> severityText <> "]" <> content
  where
    severityText = case severity of
      SeverityCritical -> "critical"
      SeverityWarning  -> "warning"
      SeverityPositive -> "positive"
      SeverityInfo     -> "info"
      SeverityBargain  -> "bargain"

-- | Check if a narrative entry is a formatted event
isEventEntry :: Text -> Bool
isEventEntry txt = T.isPrefixOf "[EVENT:" txt

-- | Get the severity of an event from its formatted type
eventSeverity :: Text -> EventSeverity
eventSeverity formatted = case extractSeverity formatted of
  Just "critical" -> SeverityCritical
  Just "warning"  -> SeverityWarning
  Just "positive" -> SeverityPositive
  Just "bargain"  -> SeverityBargain
  _               -> SeverityInfo
  where
    extractSeverity txt =
      let afterType = T.drop 1 $ T.dropWhile (/= ':') $ T.drop 7 txt
          severity = T.takeWhile (/= ']') afterType
      in if T.null severity then Nothing else Just severity

-- | Parse event entry format: [EVENT:TYPE:SEVERITY]content
parseEventEntry :: Text -> (Text, EventSeverity, Text)
parseEventEntry txt =
  let inner = T.drop 7 txt  -- Drop "[EVENT:"
      eventType = T.takeWhile (/= ':') inner
      afterType = T.drop 1 $ T.dropWhile (/= ':') inner
      severityText = T.takeWhile (/= ']') afterType
      content = T.drop 1 $ T.dropWhile (/= ']') afterType
      severity = case severityText of
        "critical" -> SeverityCritical
        "warning"  -> SeverityWarning
        "positive" -> SeverityPositive
        "bargain"  -> SeverityBargain
        _          -> SeverityInfo
  in (eventType, severity, content)

-- | Render a formatted event entry with appropriate CSS styling
renderEventEntry :: Text -> UI Element
renderEventEntry txt = do
  let (eventType, severity, content) = parseEventEntry txt
      baseClass = "state-event"
      severityClass = case severity of
        SeverityCritical -> "state-event-critical"
        SeverityWarning  -> "state-event-warning"
        SeverityPositive -> "state-event-positive"
        SeverityInfo     -> "state-event-info"
        SeverityBargain  -> "state-event-bargain"
      -- Special class for trauma and outcome events
      extraClass = case eventType of
        "TRAUMA"  -> " state-event-trauma"
        "OUTCOME" -> " state-event-outcome"
        _         -> ""

  entry <- UI.div # set (attr "class") (baseClass <> " " <> severityClass <> extraClass)

  -- Icon based on event type (Font Awesome)
  let iconClass = eventTypeIcon eventType
  iconEl <- mkElement "i" # set (attr "class") ("state-event-icon " <> T.unpack iconClass)

  -- Label (event type)
  labelEl <- UI.span #. "state-event-label"
    # set text (T.unpack eventType)

  -- Content (may contain newlines for trauma)
  contentEl <- UI.div #. "state-event-content"

  -- Handle multi-line content (e.g., trauma with breaking point)
  let contentLines = T.lines content
  if length contentLines > 1
    then do
      -- Multi-line: render each line as a paragraph
      lineEls <- mapM renderContentLine contentLines
      void $ element contentEl #+ map element lineEls
    else
      void $ element contentEl # set text (T.unpack content)

  -- Special handling for trauma: add trauma name badge
  case eventType of
    "TRAUMA" -> do
      -- Extract trauma name from content (after "You are now: ")
      let traumaLine = filter (T.isPrefixOf "You are now:") contentLines
      case traumaLine of
        (line:_) -> do
          let traumaName = T.strip $ T.drop 12 line  -- Drop "You are now: "
          traumaBadge <- UI.span #. "trauma-name"
            # set text (T.unpack traumaName)
          void $ element entry #+ [element iconEl, element labelEl, element contentEl, element traumaBadge]
        [] -> void $ element entry #+ [element iconEl, element labelEl, element contentEl]
    _ -> void $ element entry #+ [element iconEl, element labelEl, element contentEl]

  pure entry
  where
    renderContentLine line
      | T.null (T.strip line) = UI.div # set style [("height", "8px")]  -- Empty line = spacer
      | T.isPrefixOf "You are now:" line = UI.div # set style [("display", "none")]  -- Hide this, shown as badge
      | otherwise = UI.div # set text (T.unpack line)

-- | Map outcome tier to display severity
tierToSeverity :: OutcomeTier -> EventSeverity
tierToSeverity tier = case tier of
  Critical -> SeverityPositive   -- Gold/success
  Success  -> SeverityPositive   -- Green/success
  Partial  -> SeverityWarning    -- Amber/warning
  Bad      -> SeverityWarning    -- Red/warning
  Disaster -> SeverityCritical   -- Deep red/critical

-- | Human-readable tier label
tierLabel :: OutcomeTier -> Text
tierLabel tier = case tier of
  Critical -> "CRITICAL SUCCESS"
  Success  -> "SUCCESS"
  Partial  -> "PARTIAL SUCCESS"
  Bad      -> "BAD OUTCOME"
  Disaster -> "DISASTER"

-- | Get Font Awesome icon class for event type
eventTypeIcon :: Text -> Text
eventTypeIcon eventType = case eventType of
  "STRESS"  -> "fa-solid fa-heart-crack"
  "TRAUMA"  -> "fa-solid fa-skull"
  "HEAT"    -> "fa-solid fa-fire"
  "WANTED"  -> "fa-solid fa-bullseye"
  "COIN"    -> "fa-solid fa-coins"
  "DICE"    -> "fa-solid fa-dice"
  "BARGAIN" -> "fa-solid fa-handshake"
  "CLOCK"   -> "fa-solid fa-clock"
  "SCENE"   -> "fa-solid fa-film"
  "OUTCOME" -> "fa-solid fa-dice-d20"   -- D20 for outcome
  _         -> "fa-solid fa-circle"
