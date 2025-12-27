-- | DM-Specific Tools (mid-turn capabilities)
module DM.Tools
  ( -- * Tools
    ThinkAsDM(..)
  , SpeakAsNPC(..)
  , AskPlayer(..)
  , Choose(..)
  , SpendDie(..)

    -- * Transition Tools (mood state machine)
  , Engage(..)
  , Resolve(..)
  , Accept(..)

    -- * Tool Inputs/Outputs
  , ThinkInput(..)
  , SpeakInput(..)
  , AskInput(..)
  , AskResult(..)
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
                       , emit, requestChoice, requestText, randomDouble, get, put, modify)
import Tidepool.Schema (objectSchema, arraySchema, enumSchema, emptySchema, schemaToValue, describeField, SchemaType(..))
import Effectful (Eff, (:>))
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (Value, ToJSON, FromJSON, toJSON)
import Data.List (delete)
import GHC.Generics (Generic)

-- ══════════════════════════════════════════════════════════════
-- DM EVENTS
-- ══════════════════════════════════════════════════════════════

data DMEvent
  = DMThought Text
  | NPCSpoke NpcId Text
  | PlayerAsked Text
  | RandomChoice Text Int
  | DieSpent Int OutcomeTier Text          -- dieValue, outcome, context
  | ClockCompleted Text Text Consequence   -- clockId, clockName, consequence
  | SceneCompressed Text                   -- summary of what was compressed
  | MoodTransition Text Text Text          -- toolName, fromMood, toMood
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
-- ASK PLAYER
-- ══════════════════════════════════════════════════════════════

data AskPlayer = AskPlayer
  deriving (Show, Eq, Generic)

data AskInput = AskInput
  { question :: Text
  , choices :: Maybe [Text]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data AskResult = AskResult { playerResponse :: Text }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance Tool AskPlayer DMEvent WorldState where
  type ToolInput AskPlayer = AskInput
  type ToolOutput AskPlayer = AskResult

  toolName = "ask_player"
  toolDescription = "Pause and ask the player a question."
  inputSchema = schemaToValue $ objectSchema
    [ ("question", emptySchema TString)
    , ("choices", arraySchema (emptySchema TString))
    ]
    ["question"]  -- choices is optional

  executeTool input = do
    emit (PlayerAsked input.question)
    response <- case input.choices of
      Nothing -> requestText input.question
      Just cs -> requestChoice input.question [(c, c) | c <- cs]
    return (AskResult response)

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
data SpendDieInput = SpendDieInput
  { situation :: Text           -- What's at stake
  , position :: Position        -- Controlled/Risky/Desperate
  , outcomeGood :: Text         -- Narrative for Critical/Success
  , outcomePartial :: Text      -- Narrative for Partial
  , outcomeBad :: Text          -- Narrative for Bad/Disaster
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
  toolDescription = "Request player spend a die. YOU MUST precommit to all three outcomes BEFORE player chooses. Player sees only tier labels, not your prewritten narratives. Returns the narrative matching their choice."
  inputSchema = schemaToValue $ objectSchema
    [ ("situation", describeField "situation" "Brief description of the action" (emptySchema TString))
    , ("position", describeField "position" "Risk level" (enumSchema ["Controlled", "Risky", "Desperate"]))
    , ("outcomeGood", describeField "outcomeGood" "Narrative if Critical/Success (player won't see until chosen)" (emptySchema TString))
    , ("outcomePartial", describeField "outcomePartial" "Narrative if Partial success (player won't see until chosen)" (emptySchema TString))
    , ("outcomeBad", describeField "outcomeBad" "Narrative if Bad/Disaster (player won't see until chosen)" (emptySchema TString))
    ]
    ["situation", "position", "outcomeGood", "outcomePartial", "outcomeBad"]

  executeTool input = do
    state <- get
    let pool = state.dicePool.poolDice

    -- Build choices showing each die and its tier (but NOT the narrative)
    let choices = [(formatDieChoice d input.position, d) | d <- pool]

    -- Get player's choice
    chosenDie <- requestChoice (formatPrompt input) choices

    -- Remove die from pool
    let newPool = delete chosenDie pool
    put state { dicePool = state.dicePool { poolDice = newPool } }

    -- Calculate outcome tier
    let outcomeTier = calculateOutcome input.position chosenDie

    -- Select the precommitted narrative based on tier
    let selectedNarrative = case outcomeTier of
          Critical -> input.outcomeGood
          Success  -> input.outcomeGood
          Partial  -> input.outcomePartial
          Bad      -> input.outcomeBad
          Disaster -> input.outcomeBad

    -- Emit event with the revealed narrative
    emit (DieSpent chosenDie outcomeTier selectedNarrative)

    return SpendDieResult
      { dieValue = chosenDie
      , tier = outcomeTier
      , narrative = selectedNarrative
      }
    where
      formatPrompt inp = inp.situation <> " (" <> T.pack (show inp.position) <> ")"

      formatDieChoice :: Int -> Position -> Text
      formatDieChoice die pos =
        let dieChar = case die of
              1 -> "⚀"; 2 -> "⚁"; 3 -> "⚂"; 4 -> "⚃"; 5 -> "⚄"; 6 -> "⚅"; _ -> "?"
            outcomeTier = calculateOutcome pos die
            tierText = case outcomeTier of
              Critical -> "Critical!"
              Success  -> "Good"
              Partial  -> "Partial"
              Bad      -> "Bad"
              Disaster -> "Disaster"
        in dieChar <> " (" <> T.pack (show die) <> ") → " <> tierText

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
    modify $ \s -> s { mood = MoodAction position domain }
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
    modify $ \s -> s { mood = MoodAftermath aftermath }
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
    modify @WorldState $ \s -> s { mood = MoodScene (Encounter "continuing" UrgencyLow True) }
    emit (MoodTransition "accept" "aftermath" "scene")
    return ()

-- ══════════════════════════════════════════════════════════════
-- TOOL REGISTRATION
-- ══════════════════════════════════════════════════════════════

-- | All DM tools as a type-safe list (including transition tools)
-- Note: SpeakAsNPC removed - dialogue should be part of narration, not a separate tool
dmToolList :: ToolList DMEvent WorldState '[ThinkAsDM, AskPlayer, Choose, SpendDie, Engage, Resolve, Accept]
dmToolList = TCons (Proxy @ThinkAsDM)
           $ TCons (Proxy @AskPlayer)
           $ TCons (Proxy @Choose)
           $ TCons (Proxy @SpendDie)
           $ TCons (Proxy @Engage)
           $ TCons (Proxy @Resolve)
           $ TCons (Proxy @Accept)
           $ TNil

-- | All DM tools as JSON for API
dmTools :: [Value]
dmTools = toolListToJSON dmToolList

-- | Names of transition tools that change mood state
transitionToolNames :: [Text]
transitionToolNames = ["engage", "resolve"]
-- Note: "accept" is NOT a transition - it completes the turn, not restarts it

-- | Create a DM dispatcher that detects mood transitions
-- When a transition tool is called, it returns ToolBreak to restart the turn
makeDMDispatcher
  :: (State WorldState :> es, Emit DMEvent :> es, RequestInput :> es, Random :> es)
  => ToolDispatcher DMEvent es
makeDMDispatcher name input = do
  -- Record mood before tool execution
  moodBefore <- get @WorldState >>= \s -> return s.mood

  -- Dispatch to the regular tool dispatcher
  result <- makeDispatcher dmToolList name input

  -- Check if mood changed (for transition tools)
  if name `elem` transitionToolNames
    then case result of
      Left err -> return (Left err)
      Right (ToolSuccess val) -> do
        -- Check if mood actually changed
        moodAfter <- get @WorldState >>= \s -> return s.mood
        if moodChanged moodBefore moodAfter
          then return (Right (ToolBreak ("mood transition: " <> name)))
          else return (Right (ToolSuccess val))
      Right (ToolBreak reason) -> return (Right (ToolBreak reason))
    else return result  -- Non-transition tools pass through unchanged
  where
    -- Simple check if mood changed
    moodChanged :: DMMood -> DMMood -> Bool
    moodChanged (MoodScene _) (MoodScene _) = False
    moodChanged (MoodAction _ _) (MoodAction _ _) = False
    moodChanged (MoodAftermath _) (MoodAftermath _) = False
    moodChanged (MoodTrauma _) (MoodTrauma _) = False
    moodChanged _ _ = True  -- Different constructors = mood changed
