-- | DM-Specific Tools (mid-turn capabilities)
module DM.Tools
  ( -- * Tools
    ThinkAsDM(..)
  , SpeakAsNPC(..)
  , Choose(..)
  , SpendDie(..)

    -- * Transition Tools (mood state machine)
  , Engage(..)
  , Resolve(..)
  , Accept(..)

    -- * Bargain Tools (out of dice)
  , AcceptBargain(..)
  , AcceptBargainInput(..)
  , AcceptBargainResult(..)
  , Retreat(..)
  , RetreatInput(..)
  , PassOut(..)
  , PassOutInput(..)
  , PassOutResult(..)

    -- * Tool Inputs/Outputs
  , ThinkInput(..)
  , SpeakInput(..)
  , ChooseInput(..)
  , ChooseResult(..)
  , SpendDieInput(..)
  , SpendDieResult(..)
  , EngageInput(..)
  , ResolveInput(..)

    -- * Events
  , DMEvent(..)

    -- * Tool Registration
  , dmToolList
  , dmTools
  , makeDMDispatcher
  ) where

import DM.State
import Tidepool.Tool
import Tidepool.Effect (Emit, RequestInput, Random, State, ToolDispatcher, ToolResult(..)
                       , emit, requestDice, randomDouble, randomInt, get, put, modify)
import Tidepool.Schema (objectSchema, arraySchema, enumSchema, emptySchema, schemaToValue, describeField, SchemaType(..))
import Effectful ((:>))
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

-- ══════════════════════════════════════════════════════════════
-- DM EVENTS
-- ══════════════════════════════════════════════════════════════

-- | Events emitted during gameplay
-- These are used for:
-- 1. Logging/debugging
-- 2. GUI notifications (state changes shown in narrative)
-- 3. Training data collection
data DMEvent
  = DMThought Text
  | NPCSpoke NpcId Text
  | PlayerAsked Text
  | RandomChoice Text Int
  | DieSpent Int OutcomeTier Text          -- dieValue, outcome, context
  | ClockCompleted Text Text Consequence   -- clockId, clockName, consequence
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
  | ClockAdvanced
      { caClockId :: Text
      , caClockName :: Text
      , caOldFilled :: Int
      , caNewFilled :: Int
      , caTotal :: Int
      }
  -- Narrative output (for GUI display)
  | NarrativeAdded Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


-- ══════════════════════════════════════════════════════════════
-- THINK AS DM
-- ══════════════════════════════════════════════════════════════

data ThinkAsDM = ThinkAsDM
  deriving (Show, Eq, Generic)

data ThinkInput = ThinkInput { thought :: Text }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance Tool ThinkAsDM DMEvent WorldState where
  type ToolInput ThinkAsDM = ThinkInput
  type ToolOutput ThinkAsDM = ()

  toolName = "think_as_dm"
  toolDescription = "Internal reasoning as the DM. Not visible to players."
  inputSchema = schemaToValue $ objectSchema
    [("thought", emptySchema TString)]
    ["thought"]

  executeTool input = emit (DMThought input.thought)

-- ══════════════════════════════════════════════════════════════
-- SPEAK AS NPC
-- ══════════════════════════════════════════════════════════════

data SpeakAsNPC = SpeakAsNPC
  deriving (Show, Eq, Generic)

data SpeakInput = SpeakInput
  { speakNpc :: NpcId
  , utterance :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance Tool SpeakAsNPC DMEvent WorldState where
  type ToolInput SpeakAsNPC = SpeakInput
  type ToolOutput SpeakAsNPC = ()

  toolName = "speak_as_npc"
  toolDescription = "Voice a specific NPC character."
  inputSchema = schemaToValue $ objectSchema
    [ ("speakNpc", emptySchema TString)
    , ("utterance", emptySchema TString)
    ]
    ["speakNpc", "utterance"]

  executeTool input = emit (NPCSpoke input.speakNpc input.utterance)

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

instance Tool Choose DMEvent WorldState where
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

-- | LLM precommits to outcomes before player chooses
-- Each outcome is (dieValue, hint, narrative) - hint shown during choice, narrative revealed after
data SpendDieInput = SpendDieInput
  { situation :: Text                       -- What's at stake
  , position :: Position                    -- Controlled/Risky/Desperate
  , outcomes :: [(Int, Text, Text)]         -- [(dieValue, hint, narrative)] for each die in pool
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data SpendDieResult = SpendDieResult
  { dieValue :: Int
  , tier :: OutcomeTier
  , narrative :: Text           -- The precommitted narrative for this tier
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance Tool SpendDie DMEvent WorldState where
  type ToolInput SpendDie = SpendDieInput
  type ToolOutput SpendDie = SpendDieResult

  toolName = "spend_die"
  toolDescription = "Request player spend a die. Precommit outcomes array parallel to dice pool - outcomes[0] is for pool[0], etc. Each die gets a UNIQUE outcome even if same value. Player sees hints during choice, narrative revealed after."
  inputSchema = schemaToValue $ objectSchema
    [ ("situation", describeField "situation" "Brief description of the action" (emptySchema TString))
    , ("position", describeField "position" "Risk level" (enumSchema ["Controlled", "Risky", "Desperate"]))
    , ("outcomes", describeField "outcomes" "Array parallel to dice pool. Each element is [dieValue, hint, narrative]. outcomes[i] is for pool[i]. EACH die gets unique outcome even if same value! hint: 3-8 words shown during choice. narrative: 1-3 sentences revealed after."
        (arraySchema (arraySchema (emptySchema TString))))  -- JSON: [[4, "hint1", "narr1"], [4, "hint2", "narr2"], ...]
    ]
    ["situation", "position", "outcomes"]

  executeTool input = do
    state <- get
    let pool = state.dicePool.poolDice

    -- Guard: if pool is empty, can't spend a die
    if null pool
      then do
        emit (DMThought "Tried to spend die but pool is empty")
        -- Return a fallback result - LLM shouldn't have called this
        return SpendDieResult
          { dieValue = 0
          , tier = Partial
          , narrative = "You reach for your reserves, but there's nothing left."
          }
      else do
        -- Build dice with indices and hints for the visual dice selector
        -- outcomes array is PARALLEL to pool - outcomes[i] is for pool[i]
        -- Each die gets its own unique precommitted outcome
        let diceWithHints =
              [ (dieVal, idx, getHintAt idx input.outcomes)
              | (dieVal, idx) <- zip pool [0..]
              ]

        -- Get player's choice via dice widget (shows hints on each card)
        selectedIdx <- requestDice (formatPrompt input) diceWithHints

        -- Safely get the chosen die value
        let chosenDie = if selectedIdx < length pool
                        then pool !! selectedIdx
                        else head pool  -- Fallback to first die if index invalid

        -- Get the narrative for the selected position (not by die value!)
        let selectedNarrative = getNarrativeAt selectedIdx input.outcomes

        -- Remove die from pool
        let newPool = delete chosenDie pool

        -- Calculate outcome tier for the result
        let outcomeTier = calculateOutcome input.position chosenDie

        -- Check if pool is now empty - if so, queue transition to bargain
        let moodMaybe = currentMood state
            theMood = fromMaybe defaultMood moodMaybe
            newState = if null newPool
              then updateMood (MoodBargain Bargaining
                    { bvWhatDrained = input.situation
                    , bvCanRetreat = not (isDesperateMood theMood)
                    , bvRetreatDesc = "slip away and regroup"
                    , bvPassOutDesc = "collapse from exhaustion"
                    , bvPreviousMood = theMood
                    }) state { dicePool = state.dicePool { poolDice = newPool } }
              else state { dicePool = state.dicePool { poolDice = newPool } }

        put newState

        -- Emit event with the revealed narrative
        emit (DieSpent chosenDie outcomeTier selectedNarrative)

        -- If we just emptied the pool, also emit that
        when (null newPool) $
          emit (MoodTransition "spend_die" "action" "bargain")

        return SpendDieResult
          { dieValue = chosenDie
          , tier = outcomeTier
          , narrative = selectedNarrative
          }
    where
      formatPrompt inp = inp.situation <> " (" <> T.pack (show inp.position) <> ")"

      -- Get hint at position idx (outcomes is parallel to pool)
      getHintAt :: Int -> [(Int, Text, Text)] -> Text
      getHintAt idx outcomes
        | idx < length outcomes = let (_, hint, _) = outcomes !! idx in hint
        | otherwise = ""  -- Fallback if out of bounds

      -- Get narrative at position idx (outcomes is parallel to pool)
      getNarrativeAt :: Int -> [(Int, Text, Text)] -> Text
      getNarrativeAt idx outcomes
        | idx < length outcomes = let (_, _, narrative) = outcomes !! idx in narrative
        | otherwise = "The outcome unfolds..."  -- Fallback if out of bounds

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

instance Tool Engage DMEvent WorldState where
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
    modify $ updateMood (MoodAction position domain)
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

instance Tool Resolve DMEvent WorldState where
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
    -- Build aftermath variant based on outcome
    let aftermath = case input.resolveOutcome of
          "clean" -> AmClean input.resolveWhat
          "costly" -> AmCostly input.resolveWhat input.resolveCosts input.resolveComplications
          "setback" -> AmSetback input.resolveWhat False "retreat"
          "disaster" -> AmDisaster input.resolveWhat True input.resolveComplications
          _ -> AmCostly input.resolveWhat input.resolveCosts input.resolveComplications

    -- Transition to Aftermath mood
    modify $ updateMood (MoodAftermath aftermath)
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

instance Tool Accept DMEvent WorldState where
  type ToolInput Accept = AcceptInput
  type ToolOutput Accept = ()

  toolName = "accept"
  toolDescription = "Transition from AFTERMATH back to SCENE. The player accepts consequences and returns to exploration. Use this to close out an action sequence."
  inputSchema = schemaToValue $ objectSchema
    [ ("acceptTransition", describeField "acceptTransition" "Brief transition narration" (emptySchema TString))
    ]
    ["acceptTransition"]

  executeTool _input = do
    -- Return to Scene mood (Encounter variant - default low-urgency continuation)
    modify @WorldState $ updateMood (MoodScene (Encounter "continuing" UrgencyLow True))
    emit (MoodTransition "accept" "aftermath" "scene")
    return ()

-- ══════════════════════════════════════════════════════════════
-- BARGAIN TOOLS (out of dice)
-- ══════════════════════════════════════════════════════════════

-- | AcceptBargain: Player accepts a deal to refresh dice
data AcceptBargain = AcceptBargain
  deriving (Show, Eq, Generic)

data AcceptBargainInput = AcceptBargainInput
  { bargainDescription :: Text      -- "Owe Bazso Baz a favor"
  , bargainCostType :: Text         -- "stress", "heat", "wanted", "clock", "faction", "trauma", "item"
  , bargainCostAmount :: Int        -- For stress/heat: amount. For clock: ticks. Otherwise ignored.
  , bargainCostTarget :: Maybe Text -- Clock ID or Faction ID if applicable
  , bargainDiceGained :: Int        -- 1-3
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data AcceptBargainResult = AcceptBargainResult
  { newPoolSize :: Int
  , costApplied :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance Tool AcceptBargain DMEvent WorldState where
  type ToolInput AcceptBargain = AcceptBargainInput
  type ToolOutput AcceptBargain = AcceptBargainResult

  toolName = "accept_bargain"
  toolDescription = "Accept a bargain to refresh dice. Applies the mechanical cost and adds dice to pool. Use only in BARGAIN mood."
  inputSchema = schemaToValue $ objectSchema
    [ ("bargainDescription", describeField "bargainDescription" "What the bargain is" (emptySchema TString))
    , ("bargainCostType", describeField "bargainCostType" "Type of cost" (enumSchema ["stress", "heat", "wanted", "clock", "faction", "trauma", "item"]))
    , ("bargainCostAmount", describeField "bargainCostAmount" "Amount for stress/heat, ticks for clock" (emptySchema TNumber))
    , ("bargainCostTarget", describeField "bargainCostTarget" "Clock ID or Faction ID if applicable" (emptySchema TString))
    , ("bargainDiceGained", describeField "bargainDiceGained" "Dice to add (1-3)" (emptySchema TNumber))
    ]
    ["bargainDescription", "bargainCostType", "bargainDiceGained"]

  executeTool input = do
    state <- get @WorldState

    -- Apply the cost based on type
    let costDesc = applyCost state input
        newDice = replicate (min 3 (max 1 input.bargainDiceGained)) 0  -- Placeholder values, will be rolled
        updatedPool = state.dicePool.poolDice ++ newDice

    -- Roll fresh dice values (1-6)
    rolledDice <- mapM (\_ -> randomInt 1 6) newDice
    let finalPool = state.dicePool.poolDice ++ rolledDice

    -- Get previous mood to return to (from MoodBargain if we're in it)
    let returnMood = case currentMood state of
          Just (MoodBargain bv) -> bv.bvPreviousMood
          Just m -> m  -- Keep current mood if not bargaining
          Nothing -> MoodScene (Encounter "continuing" UrgencyLow True)

    -- Update state: apply cost, add dice, return to previous mood
    modify @WorldState $ \s -> applyCostToState s input
    modify @WorldState $ \s -> updateMood returnMood $
      s { dicePool = s.dicePool { poolDice = finalPool } }

    emit (MoodTransition "accept_bargain" "bargain" (moodName returnMood))

    return AcceptBargainResult
      { newPoolSize = length finalPool
      , costApplied = costDesc
      }
    where
      applyCost :: WorldState -> AcceptBargainInput -> Text
      applyCost _ inp = case inp.bargainCostType of
        "stress" -> "Took " <> T.pack (show inp.bargainCostAmount) <> " stress"
        "heat" -> "Took " <> T.pack (show inp.bargainCostAmount) <> " heat"
        "wanted" -> "Increased wanted level"
        "clock" -> "Advanced clock: " <> fromMaybe "unknown" inp.bargainCostTarget
        "faction" -> "Owe favor to: " <> fromMaybe "unknown" inp.bargainCostTarget
        "trauma" -> "Accepted a trauma"
        "item" -> "Burned: " <> fromMaybe "an item" inp.bargainCostTarget
        _ -> "Paid a price"

      applyCostToState :: WorldState -> AcceptBargainInput -> WorldState
      applyCostToState s inp = case inp.bargainCostType of
        "stress" -> s { player = s.player { stress = min 9 (s.player.stress + inp.bargainCostAmount) } }
        "heat" -> s { player = s.player { heat = min 10 (s.player.heat + inp.bargainCostAmount) } }
        "wanted" -> s { player = s.player { wanted = min 4 (s.player.wanted + 1) } }
        "trauma" -> s { player = s.player { trauma = Trauma "bargained" : s.player.trauma, stress = 0 } }
        -- Clock and faction costs would need more sophisticated handling
        _ -> s

      moodName :: DMMood -> Text
      moodName (MoodScene _) = "scene"
      moodName (MoodAction _ _) = "action"
      moodName (MoodAftermath _) = "aftermath"
      moodName (MoodDowntime _) = "downtime"
      moodName (MoodTrauma _) = "trauma"
      moodName (MoodBargain _) = "bargain"

      fromMaybe :: Text -> Maybe Text -> Text
      fromMaybe def Nothing = def
      fromMaybe _ (Just x) = x

-- | Retreat: Player chooses to leave and rest (if available)
data Retreat = Retreat
  deriving (Show, Eq, Generic)

data RetreatInput = RetreatInput
  { retreatNarration :: Text  -- "You slip away through the back alleys..."
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance Tool Retreat DMEvent WorldState where
  type ToolInput Retreat = RetreatInput
  type ToolOutput Retreat = ()

  toolName = "retreat"
  toolDescription = "Leave the scene to rest and recover. Only available in BARGAIN mood when retreat is possible. Ends the current scene, time passes, dice refresh on next score."
  inputSchema = schemaToValue $ objectSchema
    [ ("retreatNarration", describeField "retreatNarration" "How they slip away" (emptySchema TString))
    ]
    ["retreatNarration"]

  executeTool _input = do
    -- End scene, transition to BetweenScenes phase
    -- Player will choose what to do (lay low, recover, work goal, new scene)
    let retreatContext = BetweenScenesContext
          { bscClocks = []
          , bscTransitionNarration = "You retreat to safety, leaving the scene behind..."
          }
    modify @WorldState $ \s -> s
      { phase = PhaseBetweenScenes retreatContext
      , dicePool = DicePool [4, 4, 4] -- Refresh with 3 average dice
      }
    emit (MoodTransition "retreat" "bargain" "between_scenes")
    return ()

-- | PassOut: Involuntary collapse when no retreat possible
data PassOut = PassOut
  deriving (Show, Eq, Generic)

data PassOutInput = PassOutInput
  { passOutNarration :: Text  -- "Your legs give out..."
  , clocksToAdvance :: [Text] -- Clock IDs to tick
  , wakeUpLocation :: Text    -- Where they come to
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data PassOutResult = PassOutResult
  { clocksAdvanced :: [Text]
  , wokeUpAt :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance Tool PassOut DMEvent WorldState where
  type ToolInput PassOut = PassOutInput
  type ToolOutput PassOut = PassOutResult

  toolName = "pass_out"
  toolDescription = "Collapse from exhaustion. Advances threat clocks, player wakes up somewhere (captured, rescued, in the gutter). Use when retreat is not possible."
  inputSchema = schemaToValue $ objectSchema
    [ ("passOutNarration", describeField "passOutNarration" "How they collapse" (emptySchema TString))
    , ("clocksToAdvance", describeField "clocksToAdvance" "Clock IDs to advance by 1" (arraySchema (emptySchema TString)))
    , ("wakeUpLocation", describeField "wakeUpLocation" "Where they wake up" (emptySchema TString))
    ]
    ["passOutNarration", "wakeUpLocation"]

  executeTool input = do
    -- Advance each specified clock by 1
    let clockIds = map ClockId input.clocksToAdvance
    modify @WorldState $ \s -> s
      { clocks = foldr advanceClock s.clocks clockIds
      }

    -- Create new scene at wake-up location
    let wakeUpScene = ActiveScene
          { sceneLocation = LocationId (T.toLower $ T.replace " " "_" input.wakeUpLocation)
          , scenePresent = []
          , sceneStakes = Stakes "Figure out what happened"
          , sceneBeats = mempty
          }
        wakeUpMood = MoodScene (Encounter "waking up" UrgencyMedium False)  -- Can't just walk away

    modify @WorldState $ \s -> s
      { phase = PhasePlaying wakeUpScene wakeUpMood
      , dicePool = DicePool [3, 3]  -- Minimal dice, still drained
      , player = s.player { stress = min 9 (s.player.stress + 2) }  -- Passing out is stressful
      }

    emit (MoodTransition "pass_out" "bargain" "scene")

    return PassOutResult
      { clocksAdvanced = input.clocksToAdvance
      , wokeUpAt = input.wakeUpLocation
      }
    where
      advanceClock :: ClockId -> HashMap ClockId Clock -> HashMap ClockId Clock
      advanceClock cid clocks = case HM.lookup cid clocks of
        Nothing -> clocks
        Just clock -> HM.insert cid (clock { clockFilled = clock.clockFilled + 1 }) clocks

-- ══════════════════════════════════════════════════════════════
-- TOOL REGISTRATION
-- ══════════════════════════════════════════════════════════════

-- | All DM tools as a type-safe list (including transition tools)
-- Note: SpeakAsNPC, ThinkAsDM, AskPlayer removed - they add noise without value
-- Player clarification now happens via suggestedActions in structured output
dmToolList :: ToolList DMEvent WorldState '[Choose, SpendDie, Engage, Resolve, Accept, AcceptBargain, Retreat, PassOut]
dmToolList = TCons (Proxy @Choose)
           $ TCons (Proxy @SpendDie)
           $ TCons (Proxy @Engage)
           $ TCons (Proxy @Resolve)
           $ TCons (Proxy @Accept)
           $ TCons (Proxy @AcceptBargain)
           $ TCons (Proxy @Retreat)
           $ TCons (Proxy @PassOut)
           $ TNil

-- | All DM tools as JSON for API
dmTools :: [Value]
dmTools = toolListToJSON dmToolList

-- | Bargain-only tools (for filtering when in bargain mood)
bargainToolNames :: [Text]
bargainToolNames = ["accept_bargain", "retreat", "pass_out"]

-- | Core action tools (not available in bargain mood)
actionToolNames :: [Text]
actionToolNames = ["choose", "spend_die", "engage", "resolve", "accept"]

-- | Names of transition tools that change mood state
transitionToolNames :: [Text]
transitionToolNames = ["engage", "resolve", "accept_bargain", "retreat", "pass_out"]
-- Note: "accept" is NOT a transition - it completes the turn, not restarts it

-- | Create a DM dispatcher that detects mood transitions
-- When a transition tool is called, it returns ToolBreak to restart the turn
makeDMDispatcher
  :: (State WorldState :> es, Emit DMEvent :> es, RequestInput :> es, Random :> es)
  => ToolDispatcher DMEvent es
makeDMDispatcher name input = do
  -- Record mood before tool execution (from phase if playing)
  moodBefore <- get @WorldState >>= \s -> return (currentMood s)

  -- Dispatch to the regular tool dispatcher
  result <- makeDispatcher dmToolList name input

  -- Check if mood changed (for transition tools)
  if name `elem` transitionToolNames
    then case result of
      Left err -> return (Left err)
      Right (ToolSuccess val) -> do
        -- Check if mood actually changed (from phase if playing)
        moodAfter <- get @WorldState >>= \s -> return (currentMood s)
        -- Debug: log mood comparison
        emit (DMThought $ "Dispatcher: " <> name <> " before=" <> T.pack (show moodBefore) <> " after=" <> T.pack (show moodAfter) <> " changed=" <> T.pack (show $ moodMaybeChanged moodBefore moodAfter))
        if moodMaybeChanged moodBefore moodAfter
          then return (Right (ToolBreak ("mood transition: " <> name)))
          else return (Right (ToolSuccess val))
      Right (ToolBreak reason) -> return (Right (ToolBreak reason))
    else return result  -- Non-transition tools pass through unchanged
  where
    -- Simple check if mood changed (handles Maybe from currentMood)
    moodMaybeChanged :: Maybe DMMood -> Maybe DMMood -> Bool
    moodMaybeChanged Nothing Nothing = False
    moodMaybeChanged Nothing (Just _) = True
    moodMaybeChanged (Just _) Nothing = True
    moodMaybeChanged (Just m1) (Just m2) = moodChanged m1 m2

    moodChanged :: DMMood -> DMMood -> Bool
    moodChanged (MoodScene _) (MoodScene _) = False
    moodChanged (MoodAction _ _) (MoodAction _ _) = False
    moodChanged (MoodAftermath _) (MoodAftermath _) = False
    moodChanged (MoodTrauma _) (MoodTrauma _) = False
    moodChanged (MoodBargain _) (MoodBargain _) = False
    moodChanged (MoodDowntime _) (MoodDowntime _) = False
    moodChanged _ _ = True  -- Different constructors = mood changed
