-- | DM-Specific Tools (mid-turn capabilities)
module DM.Tools
  ( -- * Tools
    SetSceneStyle(..)
  , Choose(..)
  , SpendDie(..)

    -- * Transition Tools (mood state machine)
  , Engage(..)
  , Resolve(..)
  , Accept(..)

    -- * Tool Inputs/Outputs
  , SetSceneStyleInput(..)
  , ChooseInput(..)
  , ChooseResult(..)
  , SpendDieInput(..)
  , SpendDieResult(..)
  , DieOutcome(..)
  , EngageInput(..)
  , ResolveInput(..)

    -- * Events
  , DMEvent(..)

    -- * Tool Registration
  , DMEffects
  , dmToolList
  , dmTools
  , toolsForMood
  , makeDMDispatcher
  , makeDMDispatcherWithPhase
  ) where

import DM.State
import DM.Output (DieOutcome(..))
import DM.Effect (PlayingState, runPlayingState, putMood, getMood, modifyScene, getScene)
import Tidepool.Tool
import Tidepool.Effect (Emit, RequestInput, Random, State, ToolDispatcher, ToolResult(..)
                       , emit, requestDice, randomDouble, randomInt, get, put, modify)
import Tidepool.Schema (objectSchema, arraySchema, enumSchema, emptySchema, schemaToValue, describeField, SchemaType(..))
import Effectful ((:>), IOE)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (Value, ToJSON, FromJSON)
import Data.List (delete)
import Data.Maybe (fromMaybe)
import Control.Monad (when)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import GHC.Generics (Generic)

-- | Extra effects required by DM tools
-- All DM tools have access to PlayingState for scene/mood management
type DMEffects = '[PlayingState]

-- ══════════════════════════════════════════════════════════════
-- DM EVENTS
-- ══════════════════════════════════════════════════════════════

-- | Events emitted during gameplay
-- These are used for:
-- 1. Logging/debugging
-- 2. GUI notifications (state changes shown in narrative)
-- 3. Training data collection
data DMEvent
  = RandomChoice Text Int
  | DieSpent Int OutcomeTier Text          -- dieValue, outcome, context
  | ClockCompleted Text Text Text          -- clockId, clockName, consequence narrative
  | SceneCompressed Text                   -- summary of what was compressed
  | MoodTransition Text Text Text          -- toolName, fromMood, toMood
  -- State change events (for GUI display)
  | StressChanged
      { seFrom :: Int
      , seTo :: Int
      , seReason :: Text
      }
  | TraumaTriggered
      { ttTrauma :: Trauma
      , ttTrigger :: Text
      , ttBreakingPoint :: Text            -- narrative moment of breaking
      }
  | HeatChanged
      { heFrom :: Int
      , heTo :: Int
      , heReason :: Text
      }
  | WantedChanged
      { weFrom :: Int
      , weTo :: Int
      , weReason :: Text
      }
  | CoinChanged
      { ceFrom :: Int
      , ceTo :: Int
      , ceReason :: Text
      }
  | DicePoolDepleted
      { dpContext :: Text                  -- what action drained the pool
      }
  | BargainOffered
      { boContext :: Text                  -- why they're out of options
      , boCanRetreat :: Bool               -- can they leave?
      }
  -- Narrative output (for GUI display)
  | NarrativeAdded Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


-- ══════════════════════════════════════════════════════════════
-- SET SCENE STYLE
-- ══════════════════════════════════════════════════════════════

data SetSceneStyle = SetSceneStyle
  deriving (Show, Eq, Generic)

data SetSceneStyleInput = SetSceneStyleInput
  { styleAtmosphere :: Maybe Text  -- "mundane", "liminal", "supernatural"
  , stylePressure :: Maybe Text    -- "calm", "watchful", "urgent"
  , styleClass :: Maybe Text       -- "gutter", "street", "salon"
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance Tool SetSceneStyle DMEvent WorldState DMEffects where
  type ToolInput SetSceneStyle = SetSceneStyleInput
  type ToolOutput SetSceneStyle = ()

  toolName = "set_scene_style"
  toolDescription = "Shift the scene's prose style axes. Each axis affects how narration is written. Only specify axes you want to change."
  inputSchema = schemaToValue $ objectSchema
    [ ("styleAtmosphere", describeField "styleAtmosphere"
        "Supernatural presence: mundane (normal world), liminal (wrongness accumulates), supernatural (ghosts press close)"
        (enumSchema ["mundane", "liminal", "supernatural"]))
    , ("stylePressure", describeField "stylePressure"
        "Tension level: calm (breathing room), watchful (attention split), urgent (walls closing in)"
        (enumSchema ["calm", "watchful", "urgent"]))
    , ("styleClass", describeField "styleClass"
        "Social register: gutter (raw survival), street (working criminal), salon (refined daggers)"
        (enumSchema ["gutter", "street", "salon"]))
    ]
    []  -- No required fields - only change what you specify

  executeTool input = do
    scene <- getScene
    let currentStyle = scene.sceneStyle
        newAtmosphere = maybe currentStyle.ssAtmosphere parseAtmosphere input.styleAtmosphere
        newPressure = maybe currentStyle.ssPressure parsePressure input.stylePressure
        newClass = maybe currentStyle.ssClass parseClass input.styleClass
        newStyle = SceneStyle newAtmosphere newPressure newClass
    modifyScene $ \s -> s { sceneStyle = newStyle }
    where
      parseAtmosphere t = case T.toLower t of
        "mundane" -> Mundane
        "liminal" -> Liminal
        "supernatural" -> Supernatural
        _ -> Mundane

      parsePressure t = case T.toLower t of
        "calm" -> PressureCalm
        "watchful" -> PressureWatchful
        "urgent" -> PressureUrgent
        _ -> PressureWatchful

      parseClass t = case T.toLower t of
        "gutter" -> Gutter
        "street" -> Street
        "salon" -> Salon
        _ -> Street

      showStyle s = T.intercalate ", "
        [ T.pack (show s.ssAtmosphere)
        , T.pack (show s.ssPressure)
        , T.pack (show s.ssClass)
        ]

-- ══════════════════════════════════════════════════════════════
-- CHOOSE
-- ══════════════════════════════════════════════════════════════

data Choose = Choose
  deriving (Show, Eq, Generic)

data ChooseInput = ChooseInput
  { options :: [(Double, Text)]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data ChooseResult = ChooseResult
  { chosenIndex :: Int
  , chosenLabel :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance Tool Choose DMEvent WorldState DMEffects where
  type ToolInput Choose = ChooseInput
  type ToolOutput Choose = ChooseResult

  toolName = "choose"
  toolDescription = "Make a weighted random choice. Options are [weight, label] pairs."
  inputSchema = schemaToValue $ objectSchema
    [("options", arraySchema $ arraySchema (emptySchema TNumber))]  -- [[weight, label], ...]
    ["options"]

  executeTool input = do
    let opts = input.options
        totalWeight = sum (map fst opts)
    roll <- randomDouble
    let target = roll * totalWeight
        (idx, label) = pickWeighted 0 target opts
    emit (RandomChoice label idx)
    return (ChooseResult idx label)
    where
      -- Walk through options, accumulating weight until we hit target
      pickWeighted :: Int -> Double -> [(Double, Text)] -> (Int, Text)
      pickWeighted idx target ((w, label) : rest)
        | target <= w = (idx, label)
        | otherwise = pickWeighted (idx + 1) (target - w) rest
      pickWeighted idx _ [] = (idx - 1, "")  -- Fallback (shouldn't happen)

-- ══════════════════════════════════════════════════════════════
-- SPEND DIE
-- ══════════════════════════════════════════════════════════════

data SpendDie = SpendDie
  deriving (Show, Eq, Generic)

-- DieOutcome imported from DM.Output (shared with structured output)

-- | LLM precommits to outcomes before player chooses
-- Player sees hint AND cost preview during choice - full transparency
data SpendDieInput = SpendDieInput
  { situation :: Text           -- What's at stake
  , position :: Position        -- Controlled/Risky/Desperate
  , outcomes :: [DieOutcome]    -- One outcome per die in pool (parallel to pool)
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data SpendDieResult = SpendDieResult
  { chosenDieValue :: Int
  , tier :: OutcomeTier
  , stressApplied :: Int        -- Stress delta that was applied
  , heatApplied :: Int          -- Heat delta that was applied
  , coinApplied :: Int          -- Coin delta that was applied
  , resultNarrative :: Text     -- The precommitted narrative for this outcome
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance Tool SpendDie DMEvent WorldState DMEffects where
  type ToolInput SpendDie = SpendDieInput
  type ToolOutput SpendDie = SpendDieResult

  toolName = "spend_die"
  toolDescription = "THE CORE MECHANIC. Call this for ANY situation with stakes - not just combat. Social pressure? Spend die. Sneaking past? Spend die. Gathering dangerous info? Spend die. Player sees hint + costs BEFORE choosing, so they can weigh risk vs reward. Higher die = better outcome, but they might save it for later."
  inputSchema = schemaToValue $ objectSchema
    [ ("situation", describeField "situation" "Brief description of the action" (emptySchema TString))
    , ("position", describeField "position" "Risk level" (enumSchema ["Controlled", "Risky", "Desperate"]))
    , ("outcomes", describeField "outcomes"
        "Array parallel to dice pool. outcomes[i] is for pool[i]. Each outcome object has: dieValue (int), hint (3-8 words), stressCost (0-3 typical), heatCost (0-2 typical), coinDelta (positive=gain), narrative (1-3 sentences). Player sees hint+costs during choice. Higher die = better outcome with lower costs."
        (arraySchema $ objectSchema
          [ ("dieValue", describeField "dieValue" "The die value" (emptySchema TInteger))
          , ("hint", describeField "hint" "3-8 word preview shown during choice" (emptySchema TString))
          , ("stressCost", describeField "stressCost" "Stress cost (0-3 typical, can be negative for relief)" (emptySchema TInteger))
          , ("heatCost", describeField "heatCost" "Heat cost (0-2 typical)" (emptySchema TInteger))
          , ("coinDelta", describeField "coinDelta" "Coin change (positive=gain, negative=cost)" (emptySchema TInteger))
          , ("narrative", describeField "narrative" "1-3 sentences revealed after choice" (emptySchema TString))
          ]
          ["dieValue", "hint", "stressCost", "heatCost", "coinDelta", "narrative"]))
    ]
    ["situation", "position", "outcomes"]

  executeTool input = do
    state <- get @WorldState
    let pool = state.dicePool.poolDice

    -- Guard: if pool is empty, can't spend a die
    if null pool
      then return SpendDieResult
          { chosenDieValue = 0
          , tier = Partial
          , stressApplied = 0
          , heatApplied = 0
          , coinApplied = 0
          , resultNarrative = "You reach for your reserves, but there's nothing left."
          }
      else do
        -- Build dice with indices and hint+cost preview for the visual dice selector
        -- outcomes array is PARALLEL to pool - outcomes[i] is for pool[i]
        let diceWithHints =
              [ (dieVal, idx, formatHintWithCosts (getOutcomeAt idx input.outcomes))
              | (dieVal, idx) <- zip pool [0..]
              ]

        -- Get player's choice via dice widget (shows hints + costs on each card)
        selectedIdx <- requestDice (formatPrompt input) diceWithHints

        -- Get the chosen outcome and actual die value from pool
        let chosenOutcome = getOutcomeAt selectedIdx input.outcomes
            chosenDie = pool !! selectedIdx  -- Use actual pool value, not LLM's dieValue

        -- Remove die from pool by index (not value, in case of duplicates)
        let newPool = take selectedIdx pool ++ drop (selectedIdx + 1) pool

        -- Calculate outcome tier for the result
        let outcomeTier = calculateOutcome input.position chosenDie

        -- Capture current mood BEFORE any modifications for pendingOutcome
        currentMoodBefore <- getMood
        let (position, stakes) = case currentMoodBefore of
              MoodAction (AvControlled _ risk) _ -> (Controlled, risk)
              MoodAction (AvRisky threat _) _ -> (Risky, threat)
              MoodAction (AvDesperate why stakes' _) _ -> (Desperate, why <> ": " <> stakes')
              _ -> (Risky, input.situation)

        -- Apply the deltas and populate pendingOutcome for resolve tool
        modify @WorldState $ \s -> s
          { dicePool = s.dicePool { poolDice = newPool }
          , player = (s.player)
              { stress = clamp 0 9 (s.player.stress + chosenOutcome.stressCost)
              , heat = clamp 0 10 (s.player.heat + chosenOutcome.heatCost)
              , coin = max 0 (s.player.coin + chosenOutcome.coinDelta)
              }
          , pendingOutcome = Just PendingOutcome
              { outcomeContext = input.situation
              , outcomePosition = position
              , outcomeEffect = Standard  -- Could add to SpendDie input if needed
              , outcomeStakes = stakes
              , chosenDie = Just chosenDie
              , chosenTier = Just outcomeTier
              }
          }

        -- Get updated state for trauma check and event emission
        newState <- get @WorldState

        -- Check trauma FIRST (stress >= 9 takes priority over empty pool)
        -- If stress hits 9, they're breaking regardless of dice remaining
        if newState.player.stress >= 9
          then do
            putMood $ MoodTrauma Breaking
              { tvTraumaType = Trauma "pending"  -- Will be assigned by LLM
              , tvWhatBroke = input.situation
              , tvTrigger = "stress overflow from dice outcome"
              , tvAdrenaline = True  -- Can push through for one more action
              }
            emit (MoodTransition "spend_die" "action" "trauma")
          else
            -- Check if pool is now empty - if so, transition to bargain mood
            when (null newPool) $ do
              putMood $ MoodBargain Bargaining
                { bvWhatDrained = input.situation
                , bvCanRetreat = not (isDesperateMood currentMoodBefore)
                , bvRetreatDesc = "slip away and regroup"
                , bvPassOutDesc = "collapse from exhaustion"
                , bvPreviousMood = currentMoodBefore
                }
              emit (MoodTransition "spend_die" "action" "bargain")

        -- Emit event with the revealed narrative
        emit (DieSpent chosenDie outcomeTier chosenOutcome.narrative)

        -- Emit stress/heat changes if any (from/to are actual values, not deltas)
        when (chosenOutcome.stressCost /= 0) $
          emit (StressChanged state.player.stress newState.player.stress "dice outcome")
        when (chosenOutcome.heatCost /= 0) $
          emit (HeatChanged state.player.heat newState.player.heat "dice outcome")

        return SpendDieResult
          { chosenDieValue = chosenDie
          , tier = outcomeTier
          , stressApplied = chosenOutcome.stressCost
          , heatApplied = chosenOutcome.heatCost
          , coinApplied = chosenOutcome.coinDelta
          , resultNarrative = chosenOutcome.narrative
          }
    where
      clamp lo hi x = max lo (min hi x)
      formatPrompt inp = inp.situation <> " (" <> T.pack (show inp.position) <> ")"

      -- Get outcome at position idx (outcomes is parallel to pool)
      getOutcomeAt :: Int -> [DieOutcome] -> DieOutcome
      getOutcomeAt idx outs
        | idx < length outs = outs !! idx
        | otherwise = DieOutcome 0 "?" 0 0 0 "The outcome unfolds..."

      -- Format hint with cost preview for dice display
      formatHintWithCosts :: DieOutcome -> Text
      formatHintWithCosts o =
        let costs = filter (not . T.null)
              [ if o.stressCost > 0 then "+" <> T.pack (show o.stressCost) <> " stress" else ""
              , if o.stressCost < 0 then T.pack (show o.stressCost) <> " stress" else ""
              , if o.heatCost > 0 then "+" <> T.pack (show o.heatCost) <> " heat" else ""
              , if o.coinDelta > 0 then "+" <> T.pack (show o.coinDelta) <> " coin" else ""
              , if o.coinDelta < 0 then T.pack (show o.coinDelta) <> " coin" else ""
              ]
            costStr = if null costs then "" else " [" <> T.intercalate ", " costs <> "]"
        in o.hint <> costStr

      -- Check if current mood is desperate (no retreat allowed)
      isDesperateMood (MoodAction (AvDesperate _ _ _) _) = True
      isDesperateMood _ = False

-- ══════════════════════════════════════════════════════════════
-- TRANSITION TOOLS (Mood State Machine)
-- ══════════════════════════════════════════════════════════════

-- | Engage: Scene → Action
-- Player commits to a risky action, transitioning from exploration to action
data Engage = Engage
  deriving (Show, Eq, Generic)

data EngageInput = EngageInput
  { engageIntent :: Text          -- What the player is trying to do
  , engageApproach :: Text        -- How they're doing it
  , engagePosition :: Text        -- "controlled", "risky", or "desperate"
  , engageDomain :: Maybe Text    -- "infiltration", "social", "violence", "pursuit", "arcane"
  , engageThreat :: Text          -- What could go wrong
  , engageOpportunity :: Text     -- What could be gained
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance Tool Engage DMEvent WorldState DMEffects where
  type ToolInput Engage = EngageInput
  type ToolOutput Engage = ()

  toolName = "engage"
  toolDescription = "Transition from SCENE to ACTION. Use when the player commits to something risky that requires resolution. This changes the DM's mode - after this tool, narrate the mounting tension before calling spend_die."
  inputSchema = schemaToValue $ objectSchema
    [ ("engageIntent", describeField "engageIntent" "What player is attempting" (emptySchema TString))
    , ("engageApproach", describeField "engageApproach" "How they're doing it" (emptySchema TString))
    , ("engagePosition", describeField "engagePosition" "Risk level" (enumSchema ["controlled", "risky", "desperate"]))
    , ("engageDomain", describeField "engageDomain" "Action type" (enumSchema ["infiltration", "social", "violence", "pursuit", "arcane"]))
    , ("engageThreat", describeField "engageThreat" "What could go wrong" (emptySchema TString))
    , ("engageOpportunity", describeField "engageOpportunity" "What could be gained" (emptySchema TString))
    ]
    ["engageIntent", "engageApproach", "engagePosition", "engageThreat", "engageOpportunity"]

  executeTool input = do
    -- Parse position
    let position = case input.engagePosition of
          "controlled" -> AvControlled input.engageOpportunity input.engageThreat
          "risky" -> AvRisky input.engageThreat input.engageOpportunity
          "desperate" -> AvDesperate input.engageThreat input.engageOpportunity True
          _ -> AvRisky input.engageThreat input.engageOpportunity

    -- Parse domain
    let domain = case input.engageDomain of
          Just "infiltration" -> Just DomainInfiltration
          Just "social" -> Just DomainSocial
          Just "violence" -> Just DomainViolence
          Just "pursuit" -> Just DomainPursuit
          Just "arcane" -> Just DomainArcane
          _ -> Nothing

    -- Transition to Action mood
    putMood (MoodAction position domain)
    emit (MoodTransition "engage" "scene" "action")
    return ()

-- | Resolve: Action → Aftermath
-- Action completes, consequences manifest
data Resolve = Resolve
  deriving (Show, Eq, Generic)

data ResolveInput = ResolveInput
  { resolveOutcome :: Text     -- "clean", "costly", "setback", or "disaster"
  , resolveWhat :: Text        -- What happened / was achieved
  , resolveCosts :: [Text]     -- Costs paid (for costly/setback)
  , resolveComplications :: [Text]  -- New problems introduced
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance Tool Resolve DMEvent WorldState DMEffects where
  type ToolInput Resolve = ResolveInput
  type ToolOutput Resolve = ()

  toolName = "resolve"
  toolDescription = "Transition from ACTION to AFTERMATH. Use after dice/action is resolved to show consequences. Describe what the player achieved and what it cost them."
  inputSchema = schemaToValue $ objectSchema
    [ ("resolveOutcome", describeField "resolveOutcome" "Type of outcome: clean, costly, setback, or disaster" (emptySchema TString))
    , ("resolveWhat", describeField "resolveWhat" "What happened" (emptySchema TString))
    , ("resolveCosts", describeField "resolveCosts" "Costs paid" (arraySchema (emptySchema TString)))
    , ("resolveComplications", describeField "resolveComplications" "New problems" (arraySchema (emptySchema TString)))
    ]
    ["resolveOutcome", "resolveWhat"]

  executeTool input = do
    state <- get @WorldState

    -- Capture action context from current state before transitioning
    let actionCtx = case state.pendingOutcome of
          Just pending ->
            let dieValue = fromMaybe 0 pending.chosenDie
                tier = fromMaybe Partial pending.chosenTier
                otherDice = case pending.chosenDie of
                  Just chosen -> filter (/= chosen) state.dicePool.poolDice
                  Nothing -> state.dicePool.poolDice
                domain = case currentMood state of
                  Just (MoodAction _ d) -> d
                  _ -> Nothing
            in ActionToAftermathContext
              { atacDieChosen = dieValue
              , atacPosition = pending.outcomePosition
              , atacEffect = pending.outcomeEffect
              , atacTier = tier
              , atacOtherDice = otherDice
              , atacDomain = domain
              , atacStakes = pending.outcomeStakes
              }
          Nothing -> emptyActionContext

    -- Build aftermath variant based on outcome, carrying action context
    let aftermath = case input.resolveOutcome of
          "clean" -> AmClean input.resolveWhat actionCtx
          "costly" -> AmCostly input.resolveWhat input.resolveCosts input.resolveComplications actionCtx
          "setback" -> AmSetback input.resolveWhat False "retreat" actionCtx
          "disaster" -> AmDisaster input.resolveWhat True input.resolveComplications actionCtx
          _ -> AmCostly input.resolveWhat input.resolveCosts input.resolveComplications actionCtx

    -- Clear pendingOutcome now that we've consumed it
    modify @WorldState $ \s -> s { pendingOutcome = Nothing }

    -- Transition to Aftermath mood
    putMood (MoodAftermath aftermath)
    emit (MoodTransition "resolve" "action" "aftermath")
    return ()

-- | Accept: Aftermath → Scene
-- Player accepts the new situation, returns to exploration
data Accept = Accept
  deriving (Show, Eq, Generic)

data AcceptInput = AcceptInput
  { acceptTransition :: Text  -- Brief description of moving on
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance Tool Accept DMEvent WorldState DMEffects where
  type ToolInput Accept = AcceptInput
  type ToolOutput Accept = ()

  toolName = "accept"
  toolDescription = "Transition from AFTERMATH back to SCENE. The player accepts consequences and returns to exploration. Use this to close out an action sequence."
  inputSchema = schemaToValue $ objectSchema
    [ ("acceptTransition", describeField "acceptTransition" "Brief transition narration" (emptySchema TString))
    ]
    ["acceptTransition"]

  executeTool input = do
    state <- get @WorldState

    -- Capture aftermath context for the scene
    let entryContext = AftermathToSceneContext
          { atscTransitionNote = input.acceptTransition
          , atscUnresolvedThreats = state.unresolvedThreats
          , atscRecentCosts = state.recentCosts
          }

    -- Store the entry context, clear any stale pendingOutcome, and transition to Scene
    modify @WorldState $ \s -> s
      { sceneEntryContext = Just (EntryFromAftermath entryContext)
      , pendingOutcome = Nothing  -- Clear any stale outcome from action sequence
      }

    -- Return to Scene mood (Encounter variant - default low-urgency continuation)
    putMood (MoodScene (Encounter "continuing" UrgencyLow True))
    emit (MoodTransition "accept" "aftermath" "scene")
    return ()

-- Note: BARGAIN TOOLS (AcceptBargain, Retreat, PassOut) were removed.
-- Bargain mode now uses structured output (BargainLLMOutput) + requestChoice pattern.
-- This is more reliable than hoping the LLM will call tools.
-- See DM.Output for BargainLLMOutput, DM.Loop for handleBargainTurn.

-- ══════════════════════════════════════════════════════════════
-- TOOL REGISTRATION
-- ══════════════════════════════════════════════════════════════

-- | All DM tools as a type-safe list (including transition tools)
-- Note: SpeakAsNPC, ThinkAsDM, AskPlayer removed - they add noise without value
-- Player clarification now happens via suggestedActions in structured output
-- Note: Bargain tools (AcceptBargain, Retreat, PassOut) removed - bargain mode now uses
-- structured output + requestChoice instead of tool calls
dmToolList :: ToolList DMEvent WorldState DMEffects '[SetSceneStyle, Choose, SpendDie, Engage, Resolve, Accept]
dmToolList = TCons (Proxy @SetSceneStyle)
           $ TCons (Proxy @Choose)
           $ TCons (Proxy @SpendDie)
           $ TCons (Proxy @Engage)
           $ TCons (Proxy @Resolve)
           $ TCons (Proxy @Accept)
           $ TNil

-- | All DM tools as JSON for API (used for dispatcher, not for LLM calls)
dmTools :: [Value]
dmTools = toolListToJSON dmToolList

-- ══════════════════════════════════════════════════════════════
-- PER-MOOD TOOL LISTS
-- ══════════════════════════════════════════════════════════════

-- | Scene tools: explore, transition to action, modify style
sceneToolList :: ToolList DMEvent WorldState DMEffects '[SetSceneStyle, Choose, Engage]
sceneToolList = TCons (Proxy @SetSceneStyle)
              $ TCons (Proxy @Choose)
              $ TCons (Proxy @Engage)
              $ TNil

-- | Action tools: spend dice, resolve to aftermath
actionToolList :: ToolList DMEvent WorldState DMEffects '[SetSceneStyle, SpendDie, Resolve]
actionToolList = TCons (Proxy @SetSceneStyle)
               $ TCons (Proxy @SpendDie)
               $ TCons (Proxy @Resolve)
               $ TNil

-- | Aftermath tools: accept and return to scene
aftermathToolList :: ToolList DMEvent WorldState DMEffects '[SetSceneStyle, Accept, Choose]
aftermathToolList = TCons (Proxy @SetSceneStyle)
                  $ TCons (Proxy @Accept)
                  $ TCons (Proxy @Choose)
                  $ TNil

-- | Trauma tools: accept aftermath of breaking
traumaToolList :: ToolList DMEvent WorldState DMEffects '[SetSceneStyle, Accept, Choose]
traumaToolList = TCons (Proxy @SetSceneStyle)
               $ TCons (Proxy @Accept)
               $ TCons (Proxy @Choose)
               $ TNil

-- Note: bargainToolList removed - bargain mode uses structured output + requestChoice

-- | Get tool list for a specific mood
toolsForMood :: DMMood -> [Value]
toolsForMood = \case
  MoodScene _     -> toolListToJSON sceneToolList
  MoodAction _ _  -> toolListToJSON actionToolList
  MoodAftermath _ -> toolListToJSON aftermathToolList
  MoodTrauma _    -> toolListToJSON traumaToolList
  MoodBargain _   -> []  -- No tools - bargain uses structured output + requestChoice

-- | Names of transition tools that change mood state
transitionToolNames :: [Text]
transitionToolNames = ["engage", "resolve"]
-- Note: accept_bargain, retreat, pass_out removed - bargain mode uses structured output
-- Note: "accept" is NOT a transition - it completes the turn, not restarts it

-- | Create a DM dispatcher that detects mood transitions
-- When a transition tool is called, it returns ToolBreak to restart the turn
-- Requires PlayingState effect to be provided by the caller
makeDMDispatcher
  :: (State WorldState :> es, Emit DMEvent :> es, RequestInput :> es, Random :> es, PlayingState :> es)
  => ToolDispatcher DMEvent es
makeDMDispatcher name input = do
  -- Record mood before tool execution (from PlayingState, not WorldState!)
  moodBefore <- getMood

  -- Dispatch to the regular tool dispatcher
  result <- makeDispatcher dmToolList name input

  -- Check if mood changed (for transition tools)
  if name `elem` transitionToolNames
    then case result of
      Left err -> return (Left err)
      Right (ToolSuccess val) -> do
        -- Check if mood actually changed (from PlayingState)
        moodAfter <- getMood
        if moodChanged moodBefore moodAfter
          then return (Right (ToolBreak ("mood transition: " <> name)))
          else return (Right (ToolSuccess val))
      Right (ToolBreak reason) -> return (Right (ToolBreak reason))
    else return result  -- Non-transition tools pass through unchanged
  where
    moodChanged :: DMMood -> DMMood -> Bool
    moodChanged (MoodScene _) (MoodScene _) = False
    moodChanged (MoodAction _ _) (MoodAction _ _) = False
    moodChanged (MoodAftermath _) (MoodAftermath _) = False
    moodChanged (MoodTrauma _) (MoodTrauma _) = False
    moodChanged (MoodBargain _) (MoodBargain _) = False
    moodChanged _ _ = True  -- Different constructors = mood changed

-- | DM dispatcher that automatically provides PlayingState from current phase
-- This is the main dispatcher to use - it extracts scene/mood from PhasePlaying,
-- runs tools with PlayingState effect, and writes back any changes.
-- Returns error if called outside PhasePlaying.
makeDMDispatcherWithPhase
  :: (State WorldState :> es, Emit DMEvent :> es, RequestInput :> es, Random :> es, IOE :> es)
  => ToolDispatcher DMEvent es
makeDMDispatcherWithPhase name input = do
  state <- get @WorldState
  case state.phase of
    PhasePlaying scene mood -> do
      -- Run dispatcher with PlayingState effect provided
      (result, scene', mood') <- runPlayingState scene mood $
        makeDMDispatcher name input
      -- Update only the phase, preserving all other state changes made by tools
      modify @WorldState $ \s -> s { phase = PhasePlaying scene' mood' }
      return result
    _ ->
      -- Not in playing phase - tools requiring PlayingState can't be dispatched
      pure $ Left $ "Tool " <> name <> " called outside of PhasePlaying phase"
