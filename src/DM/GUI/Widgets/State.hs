-- | DM-specific state inspector with collapsible sections
--
-- Renders WorldState as a nice tree view instead of raw JSON
module DM.GUI.Widgets.State
  ( dmStateInspector
  , DMStateInspectorHandle(..)
  , refreshDMStateInspector
  ) where

import DM.State
import Tidepool.GUI.Core (GUIBridge(..))

import Control.Concurrent.STM (atomically, readTVar)
import Control.Monad (void, when)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

-- | Handle for refreshing the state inspector
data DMStateInspectorHandle = DMStateInspectorHandle
  { dsiContainer   :: Element
  , dsiContentEl   :: Element     -- Store content element directly
  , dsiExpandedRef :: IORef Bool
  }

-- | Create a DM-specific state inspector with collapsible sections
dmStateInspector :: GUIBridge WorldState -> UI DMStateInspectorHandle
dmStateInspector bridge = do
  container <- UI.div #. "dm-state-inspector"

  -- Main toggle header
  headerEl <- UI.div #. "state-inspector-title" # set text "▶ WORLD STATE"
  contentEl <- UI.div #. "state-inspector-content" # set style [("display", "none")]

  -- Track expanded state
  expandedRef <- liftIO $ newIORef False

  -- Toggle on header click
  on UI.click headerEl $ const $ do
    isExpanded <- liftIO $ readIORef expandedRef
    if isExpanded
      then do
        void $ element contentEl # set style [("display", "none")]
        void $ element headerEl # set text "▶ WORLD STATE"
        liftIO $ writeIORef expandedRef False
      else do
        void $ element contentEl # set style [("display", "block")]
        void $ element headerEl # set text "▼ WORLD STATE"
        -- Refresh content when expanding
        refreshContent contentEl bridge
        liftIO $ writeIORef expandedRef True

  void $ element container #+ [element headerEl, element contentEl]
  pure $ DMStateInspectorHandle container contentEl expandedRef

-- | Refresh the state inspector content
refreshDMStateInspector :: DMStateInspectorHandle -> GUIBridge WorldState -> UI ()
refreshDMStateInspector handle bridge = do
  isExpanded <- liftIO $ readIORef handle.dsiExpandedRef
  when isExpanded $
    refreshContent handle.dsiContentEl bridge

-- | Refresh the content area with current state
refreshContent :: Element -> GUIBridge WorldState -> UI ()
refreshContent contentEl bridge = do
  state <- liftIO $ atomically $ readTVar bridge.gbState
  void $ element contentEl # set children []
  void $ element contentEl #+ buildStateTree state

-- | Build the collapsible tree for world state
buildStateTree :: WorldState -> [UI Element]
buildStateTree state =
  [ playerSection state.player
  , sceneSection (currentScene state)
  , moodSection (currentMood state)
  , factionsSection state.factions
  , npcsSection state.npcs
  , locationsSection state.locations
  , clocksSection state.clocks
  , threadsSection state.threads
  , rumorsSection state.rumors
  , dicePoolSection state.dicePool
  , metaSection state
  ]

-- ══════════════════════════════════════════════════════════════
-- SECTION BUILDERS
-- ══════════════════════════════════════════════════════════════

-- | Player stats section
playerSection :: PlayerState -> UI Element
playerSection p = collapsibleSection "Player" True $
  [ field "Stress" (stressBar p.stress)
  , field "Coin" (showT p.coin)
  , field "Heat" (showT p.heat)
  , field "Wanted" (showT p.wanted)
  ] ++ traumaFields p.trauma
  where
    stressBar n = T.replicate n "█" <> T.replicate (9 - n) "░" <> " (" <> showT n <> "/9)"
    traumaFields ts
      | null ts = []
      | otherwise = [field "Trauma" (T.intercalate ", " [t.unTrauma | t <- ts])]

-- | Current scene section
sceneSection :: Maybe ActiveScene -> UI Element
sceneSection Nothing = collapsibleSection "Scene" False [field "Status" "No active scene"]
sceneSection (Just s) = collapsibleSection "Scene" True $
  [ field "Location" s.sceneLocation.unLocationId
  , field "Stakes" s.sceneStakes.unStakes
  , field "Present" (if null s.scenePresent then "Nobody" else formatPresent s.scenePresent)
  , field "Beats" (showT (Seq.length s.sceneBeats) <> " recorded")
  ]
  where
    formatPresent ps = T.intercalate ", " [nid.unNpcId | nid <- ps]

-- | Mood section (from phase, may not exist)
moodSection :: Maybe DMMood -> UI Element
moodSection Nothing = collapsibleSection "Mood" False
  [ field "Current" "Not playing"
  ]
moodSection (Just m) = collapsibleSection "Mood" False
  [ field "Current" (formatMood m)
  ]
  where
    formatMood (MoodScene enc) = "Scene: " <> formatSceneVariant enc
    formatMood (MoodAction act _) = "Action: " <> formatActionVariant act
    formatMood (MoodAftermath out) = "Aftermath: " <> showT out
    formatMood (MoodTrauma t) = "Trauma: " <> t.tvWhatBroke
    formatMood (MoodBargain _) = "Bargaining"

    formatSceneVariant enc@Encounter{} = enc.svSource
    formatSceneVariant opp@Opportunity{} = opp.svNature
    formatSceneVariant disc@Discovery{} = disc.svWhat

    formatActionVariant AvControlled{} = "Controlled"
    formatActionVariant AvRisky{} = "Risky"
    formatActionVariant AvDesperate{} = "Desperate"

-- | Factions section
factionsSection :: HM.HashMap FactionId Faction -> UI Element
factionsSection factions = collapsibleSection ("Factions (" <> showT (HM.size factions) <> ")") False $
  [factionItem fid f | (fid, f) <- HM.toList factions]

factionItem :: FactionId -> Faction -> UI Element
factionItem fid f = nestedCollapsible (fid.unFactionId <> " - " <> f.factionName) $
  [ field "Attitude" (showT f.factionAttitude)
  , field "Gold" (showT f.factionResources.gold)
  , field "Soldiers" (showT f.factionResources.soldiers)
  , field "Influence" (showT f.factionResources.influence)
  ] ++ goalFields f.factionGoals
  where
    goalFields [] = []
    goalFields gs = [field "Goals" (T.intercalate "; " [g.goalDescription | g <- gs])]

-- | NPCs section
npcsSection :: HM.HashMap NpcId Npc -> UI Element
npcsSection npcs = collapsibleSection ("NPCs (" <> showT (HM.size npcs) <> ")") False $
  [npcItem nid n | (nid, n) <- HM.toList npcs]

npcItem :: NpcId -> Npc -> UI Element
npcItem nid n = nestedCollapsible (nid.unNpcId <> " - " <> n.npcName) $
  [ field "Disposition" (showT n.npcDisposition)
  , field "Faction" (maybe "None" (.unFactionId) n.npcFaction)
  , field "Location" (maybe "Unknown" (.unLocationId) n.npcLocation)
  ] ++ wantFields n.npcWants
  where
    wantFields [] = []
    wantFields ws = [field "Wants" (T.intercalate "; " [w.wantDescription | w <- ws])]

-- | Locations section
locationsSection :: HM.HashMap LocationId Location -> UI Element
locationsSection locs = collapsibleSection ("Locations (" <> showT (HM.size locs) <> ")") False $
  [locationItem lid l | (lid, l) <- HM.toList locs]

locationItem :: LocationId -> Location -> UI Element
locationItem lid l = nestedCollapsible (lid.unLocationId <> " - " <> l.locationName) $
  [ field "Control" (maybe "Neutral" (.unFactionId) l.locationControlledBy)
  , field "Features" (T.intercalate ", " l.locationFeatures)
  ]

-- | Clocks section
clocksSection :: HM.HashMap ClockId Clock -> UI Element
clocksSection clocks = collapsibleSection ("Clocks (" <> showT (HM.size clocks) <> ")") False $
  [clockItem cid c | (cid, c) <- HM.toList clocks]

clockItem :: ClockId -> Clock -> UI Element
clockItem cid c = do
  item <- UI.div #. "state-field"
  let progress = T.replicate c.clockFilled "●" <> T.replicate (c.clockSegments - c.clockFilled) "○"
  let label = cid.unClockId <> ": " <> progress <> " " <> c.clockName
  let visibility = if c.clockVisible then "" else " (hidden)"
  void $ element item # set text (T.unpack $ label <> visibility)
  pure item

-- | Threads section
threadsSection :: [Thread] -> UI Element
threadsSection threads = collapsibleSection ("Threads (" <> showT (length threads) <> ")") False $
  [threadItem t | t <- threads]

threadItem :: Thread -> UI Element
threadItem t = do
  item <- UI.div #. "state-field"
  let tensionStr = case t.threadTension of
        Simmering -> "~"
        Rising -> "↑"
        Urgent -> "!"
        TensionCritical -> "⚠"
  void $ element item # set text (T.unpack $ tensionStr <> " " <> t.threadHook)
  pure item

-- | Rumors section
rumorsSection :: [Rumor] -> UI Element
rumorsSection rumors = collapsibleSection ("Rumors (" <> showT (length rumors) <> ")") False $
  [rumorItem r | r <- rumors]

rumorItem :: Rumor -> UI Element
rumorItem r = do
  item <- UI.div #. "state-field"
  let truthStr = case r.rumorTruthValue of
        TrueRumor -> "✓"
        FalseRumor -> "✗"
        PartiallyTrue _ -> "~"
        Unknown -> "?"
  void $ element item # set text (T.unpack $ truthStr <> " " <> r.rumorContent)
  pure item

-- | Dice pool section
dicePoolSection :: DicePool -> UI Element
dicePoolSection pool = collapsibleSection "Dice Pool" False
  [ field "Available" (formatDice pool.poolDice)
  ]
  where
    formatDice dice = T.intercalate " " [dieChar d | d <- dice]
    dieChar d = case d of
      1 -> "⚀"; 2 -> "⚁"; 3 -> "⚂"; 4 -> "⚃"; 5 -> "⚄"; 6 -> "⚅"; _ -> "?"

-- | Meta info section (tone, goals, pending outcome)
metaSection :: WorldState -> UI Element
metaSection state = collapsibleSection "Meta" False $
  [ field "Tone" (showT state.tone)
  , field "Session Goals" (T.intercalate "; " state.sessionGoals)
  ] ++ pendingFields state.pendingOutcome
  where
    pendingFields Nothing = []
    pendingFields (Just po) =
      [ field "Pending" (po.outcomeContext <> " [" <> showT po.outcomePosition <> "]")
      ]

-- ══════════════════════════════════════════════════════════════
-- HELPERS
-- ══════════════════════════════════════════════════════════════

-- | Create a collapsible section using HTML details/summary
collapsibleSection :: Text -> Bool -> [UI Element] -> UI Element
collapsibleSection title startOpen items = do
  details <- mkElement "details" #. "state-section"
  when startOpen $
    void $ element details # set (attr "open") "open"

  summary <- mkElement "summary" #. "state-section-title"
    # set text (T.unpack title)

  content <- UI.div #. "state-section-content"
  void $ element content #+ items

  void $ element details #+ [element summary, element content]
  pure details

-- | Create a nested collapsible (for items within sections)
nestedCollapsible :: Text -> [UI Element] -> UI Element
nestedCollapsible title items = do
  details <- mkElement "details" #. "state-item"
  summary <- mkElement "summary" #. "state-item-title"
    # set text (T.unpack title)

  content <- UI.div #. "state-item-content"
  void $ element content #+ items

  void $ element details #+ [element summary, element content]
  pure details

-- | Simple field with label and value
field :: Text -> Text -> UI Element
field label value = do
  item <- UI.div #. "state-field"
  labelEl <- UI.span #. "state-field-label" # set text (T.unpack $ label <> ": ")
  valueEl <- UI.span #. "state-field-value" # set text (T.unpack value)
  void $ element item #+ [element labelEl, element valueEl]
  pure item

showT :: Show a => a -> Text
showT = T.pack . show
