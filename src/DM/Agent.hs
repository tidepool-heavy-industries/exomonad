{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
-- | DM Agent
--
-- The Dungeon Master agent owns its full lifecycle via 'dmRun'.
-- It handles character creation, scene management, phase transitions,
-- and player turns - the runner just interprets effects.
--
module DM.Agent
  ( dm
  , DMM
  , dmRun
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import Data.Aeson (Value(..), toJSON)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import Control.Monad (when, replicateM)
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq

import Tidepool
import DM.State
import DM.Loop
    ( dmTurn
    , PlayerInput(..)
    , Response(..)
    , checkClockConsequences
    , scenarioInitSchemaJSON
    )
import DM.Tools (DMEvent(..), makeDMDispatcher)
import DM.CharacterCreation
    ( CharacterChoices(..)
    , scenarioInitPrompt
    , ScenarioInit(..)
    , ClockInit(..)
    , archetypeName
    )

-- | The DM agent monad (no extra effects)
type DMM = AgentM WorldState DMEvent '[]

-- | The DM agent
--
-- A Blades in the Dark-style dungeon master agent with:
-- - Mood-based state machine (Scene → Action → Aftermath → Downtime)
-- - FitD dice mechanics
-- - Tool support (ThinkAsDM, SpeakAsNPC, SpendDie, etc.)
--
-- The agent owns its lifecycle. It requests input when it needs it,
-- emits narrative when it has output, and returns when the session ends.
--
-- Usage:
--
-- @
-- main = do
--   finalState <- tidepool config dm
--   putStrLn "Game over"
-- @
dm :: SimpleAgent WorldState DMEvent
dm = Agent
  { agentName       = "dungeon-master"
  , agentInit       = initialWorld
  , agentRun        = dmRun
  , agentDispatcher = dmDispatcher
  }

-- ════════════════════════════════════════════════════════════════════════════
-- DM RUN: The agent's full lifecycle
-- ════════════════════════════════════════════════════════════════════════════

-- | The DM agent's full lifecycle
--
-- Handles startup, main loop, and shutdown. Uses RequestInput when it needs
-- player input, Emit for narrative output, returns when session ends.
dmRun :: DMM ()
dmRun = do
  -- Main loop - the intro/greeting happens as part of scene setup
  mainLoop

  -- Shutdown
  emit $ NarrativeAdded "Session ends. The city remembers..."

-- | Main game loop - runs until session ends
mainLoop :: DMM ()
mainLoop = do
  state <- get @WorldState

  -- Check for session end
  case state.phase of
    PhaseSessionEnded -> pure ()  -- Done!
    _ -> continueLoop state

-- | Continue the main loop based on current state
continueLoop :: WorldState -> DMM ()
continueLoop state = case state.scene of
  Nothing -> handleNoScene state
  Just _ -> handleActiveScene

-- | Handle the no-scene case (fresh game or between scenes)
handleNoScene :: WorldState -> DMM ()
handleNoScene state = do
  -- Check if this is a fresh game via chat history
  history <- getHistory
  let isFreshGame = null history

  -- Initialize dice pool for fresh games
  when isFreshGame $ do
    startingDice <- rollStartingDice
    modify @WorldState $ \s -> s { dicePool = DicePool startingDice }
    logInfo $ "[Loop] Fresh game - initialized dice pool: " <> T.pack (show startingDice)

  case (isFreshGame, state.characterChoices) of
    (True, Nothing) -> do
      -- Fresh game without character - request character creation
      logInfo "[Loop] Fresh game - requesting character creation..."
      handleCharacterCreation
      mainLoop

    (True, Just choices) -> do
      -- Fresh game with character - generate opening scenario
      logInfo "[Loop] Generating opening scenario from character choices..."
      generateOpeningScenario choices
      mainLoop

    (False, _) -> do
      -- Scene ended (not fresh game) - handle between scenes
      logInfo "[Loop] Scene ended, entering BetweenScenes..."
      handleBetweenScenes
      mainLoop

-- | Handle character creation via GUI
handleCharacterCreation :: DMM ()
handleCharacterCreation = do
  -- Request character creation from GUI
  resultJson <- requestCustom "character-creation" (toJSON ())

  -- Parse the result
  case Aeson.fromJSON resultJson of
    Aeson.Success (Just choices) -> do
      logInfo $ "[CharacterCreation] Character created: " <> choices.ccName
      modify @WorldState $ \s -> s { characterChoices = Just choices }

      -- Add intro narrative
      emit $ NarrativeAdded "DOSKVOL. Industrial sprawl. Eternal night."
      emit $ NarrativeAdded "The ghosts press against the lightning barriers."
      emit $ NarrativeAdded "The gangs carve up the districts."
      emit $ NarrativeAdded $ "You are " <> choices.ccName <> ", a " <> archetypeName choices.ccArchetype <> "."
      emit $ NarrativeAdded "Your story begins..."

    Aeson.Success Nothing -> do
      -- Cancelled - end session
      logInfo "[CharacterCreation] Cancelled by user"
      modify @WorldState $ \s -> s { phase = PhaseSessionEnded }

    Aeson.Error err -> do
      logWarn $ "[CharacterCreation] Parse error: " <> T.pack err
      modify @WorldState $ \s -> s { phase = PhaseSessionEnded }

-- | Generate opening scenario from character choices
generateOpeningScenario :: CharacterChoices -> DMM ()
generateOpeningScenario choices = do
  let prompt = scenarioInitPrompt choices
  logInfo $ "[Loop] Scenario prompt length: " <> T.pack (show $ T.length prompt)

  -- Call LLM for scenario generation
  result <- runTurn @ScenarioInit prompt "Generate the opening scenario." scenarioInitSchemaJSON []

  case result of
    TurnCompleted (TurnParsed tr) -> do
      let scenario = tr.trOutput
      logInfo "[Loop] Scenario generated successfully"

      -- Get current state for modifications
      state <- get @WorldState

      -- Build new player state
      let newPlayer = state.player
            { stress = scenario.siStartingStress
            , coin = scenario.siStartingCoin
            , heat = scenario.siStartingHeat
            , wanted = scenario.siStartingWanted
            , trauma = maybe [] (\t -> [Trauma t]) scenario.siStartingTrauma
            }

      -- Convert ClockInit to Clock
      let clocksFromInit = HM.fromList
            [ (ClockId (T.toLower $ T.replace " " "_" ci.ciName), initToClock ci)
            | ci <- scenario.siStartingClocks
            ]

      -- Create intro scene
      let introScene = ActiveScene
            { sceneLocation = LocationId (T.toLower $ T.replace " " "_" scenario.siSceneLocation)
            , scenePresent = []
            , sceneStakes = Stakes scenario.siSceneStakes
            , sceneBeats = Seq.empty
            }

      -- Update state
      modify @WorldState $ \s -> s
        { phase = PhasePlaying
        , player = newPlayer
        , scene = Just introScene
        , clocks = clocksFromInit
        , characterChoices = Nothing  -- Clear so we don't regenerate on scene end
        , suggestedActions = scenario.siSuggestedActions
        }

      -- Emit narrative
      emit $ NarrativeAdded scenario.siFateNarration
      emit $ NarrativeAdded scenario.siSceneNarration
      emit $ NarrativeAdded $ ">> " <> scenario.siOpeningHook

    TurnCompleted (TurnParseFailed{..}) -> do
      logWarn $ "Failed to generate opening scenario: " <> T.pack tpfError
      emit $ NarrativeAdded "Something went wrong. The city's secrets elude you..."
      modify @WorldState $ \s -> s { phase = PhaseSessionEnded }

    TurnBroken reason -> do
      logWarn $ "LLM failed during scenario generation: " <> reason
      emit $ NarrativeAdded "The spirits are silent. Try again later..."
      modify @WorldState $ \s -> s { phase = PhaseSessionEnded }

-- | Handle between-scenes phase
handleBetweenScenes :: DMM ()
handleBetweenScenes = do
  logInfo "[BetweenScenes] Entering between-scenes phase..."

  -- Tick threat clocks (time passes)
  tickThreatClocks

  -- Check for completed clocks
  checkClockConsequences

  state <- get @WorldState

  -- Generate transition narration
  transitionText <- generateTransitionNarration state

  -- Build clock summaries and options
  let clockSummaries = buildClockSummaries state.clocks
      options = buildAvailableOptions state

  -- Set display context for GUI
  let ctx = BetweenScenesContext
        { bscClocks = clockSummaries
        , bscTransitionNarration = transitionText
        }
  modify @WorldState $ \s -> s { betweenScenesDisplay = Just ctx }

  -- Request choice from player
  let labeledOptions = [(optionLabel opt, opt) | opt <- options]
  chosenOption <- requestChoice "What do you do?" labeledOptions

  -- Clear display context
  modify @WorldState $ \s -> s { betweenScenesDisplay = Nothing }

  logInfo $ "[BetweenScenes] Player chose: " <> T.pack (show chosenOption)

  -- Apply the choice
  applyBetweenScenesChoice chosenOption

-- | Handle active scene - wait for input, run turn, loop
handleActiveScene :: DMM ()
handleActiveScene = do
  logInfo "[Loop] Waiting for player input..."
  input <- requestText "> "
  logInfo $ "[Loop] Got input: " <> T.take 50 input

  -- Check for quit commands
  case T.toLower (T.strip input) of
    "quit" -> modify @WorldState $ \s -> s { phase = PhaseSessionEnded }
    "exit" -> modify @WorldState $ \s -> s { phase = PhaseSessionEnded }
    "q" -> modify @WorldState $ \s -> s { phase = PhaseSessionEnded }
    "" -> mainLoop  -- Empty input, continue
    _ -> do
      logInfo "[Loop] Starting dmTurn..."
      let playerInput = PlayerInput input []
      response <- dmTurn playerInput
      logInfo $ "[Loop] dmTurn completed: " <> T.take 100 response.responseText

      -- Emit narrative
      emit $ NarrativeAdded response.responseText

      -- Store suggested actions
      modify @WorldState $ \s -> s { suggestedActions = response.responseSuggestedActions }

      mainLoop

-- ════════════════════════════════════════════════════════════════════════════
-- BETWEEN-SCENES HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Tick threat clocks (time passes between scenes)
tickThreatClocks :: DMM ()
tickThreatClocks = modify @WorldState $ \s ->
  s { clocks = HM.map tickIfThreat s.clocks }
  where
    tickIfThreat clock = case clock.clockType of
      ThreatClock -> clock { clockFilled = min clock.clockSegments (clock.clockFilled + 1) }
      GoalClock -> clock

-- | Generate transition narration via LLM
generateTransitionNarration :: WorldState -> DMM Text
generateTransitionNarration state = do
  let prompt = T.unlines
        [ "You are a noir narrator for a Blades in the Dark game."
        , "The current scene has ended. Generate 1-2 sentences (~50 words) of transition narration."
        , "Evoke passage of time, mood of the city, character's state of mind."
        , "Tight. Atmospheric. No dialogue."
        , ""
        , "Character status:"
        , "- Stress: " <> T.pack (show state.player.stress) <> "/9"
        , "- Heat: " <> T.pack (show state.player.heat) <> "/10"
        , "- Coin: " <> T.pack (show state.player.coin)
        ]

  let schema = Object $ KM.fromList
        [ ("type", String "object")
        , ("additionalProperties", Bool False)
        , ("properties", Object $ KM.fromList
            [ ("text", Object $ KM.fromList
                [ ("type", String "string")
                , ("description", String "1-2 sentences of transition narration (~50 words)")
                ])
            ])
        , ("required", Array $ V.fromList [String "text"])
        ]

  result <- runTurn @TransitionOutput prompt "Generate transition." schema []

  case result of
    TurnCompleted (TurnParsed tr) -> pure tr.trOutput.toText
    TurnCompleted (TurnParseFailed{..}) -> do
      logWarn $ "Transition parse failed: " <> T.pack tpfError
      pure "Time passes in the eternal dark of Doskvol..."
    TurnBroken reason -> do
      logWarn $ "Transition generation failed: " <> reason
      pure "The city waits..."

-- | Wrapper for transition output
data TransitionOutput = TransitionOutput { toText :: Text }
  deriving (Show, Eq)

instance Aeson.FromJSON TransitionOutput where
  parseJSON = Aeson.withObject "TransitionOutput" $ \o ->
    TransitionOutput <$> o Aeson..: "text"

-- | Build clock summaries for display
buildClockSummaries :: HM.HashMap ClockId Clock -> [ClockSummary]
buildClockSummaries clockMap =
  [ ClockSummary
      { csName = clock.clockName
      , csFilled = clock.clockFilled
      , csSegments = clock.clockSegments
      , csIsThreat = clock.clockType == ThreatClock
      }
  | clock <- HM.elems clockMap
  , clock.clockVisible
  ]

-- | Build available options based on state
buildAvailableOptions :: WorldState -> [BetweenScenesOption]
buildAvailableOptions state = concat
  [ [ BSLayLow | state.player.heat > 0 ]
  , [ BSRecover | state.player.coin > 0 && state.player.stress > 0 ]
  , [ BSWorkGoal name
    | clock <- HM.elems state.clocks
    , clock.clockType == GoalClock
    , clock.clockVisible
    , let name = clock.clockName
    ]
  , [ BSNewScene ]
  , [ BSEndSession ]
  ]

-- | Apply player's between-scenes choice
applyBetweenScenesChoice :: BetweenScenesOption -> DMM ()
applyBetweenScenesChoice choice = case choice of
  BSLayLow -> do
    logInfo "[BetweenScenes] Laying low - reducing heat"
    modify @WorldState $ \s -> s
      { player = s.player { heat = max 0 (s.player.heat - 1) }
      }
    emit $ NarrativeAdded "You lay low. The heat fades, but time passes..."
    createNewScene

  BSRecover -> do
    logInfo "[BetweenScenes] Recovering - spending coin for stress"
    modify @WorldState $ \s -> s
      { player = s.player
          { coin = max 0 (s.player.coin - 1)
          , stress = max 0 (s.player.stress - 1)
          }
      }
    emit $ NarrativeAdded "You spend coin on small comforts. The stress eases..."
    createNewScene

  BSWorkGoal goalName -> do
    logInfo $ "[BetweenScenes] Working goal: " <> goalName
    modify @WorldState $ \s -> s
      { clocks = HM.map (tickGoal goalName) s.clocks
      }
    checkClockConsequences
    emit $ NarrativeAdded $ "You work toward your goal: " <> goalName <> "..."
    createNewScene
    where
      tickGoal name clock
        | clock.clockName == name && clock.clockType == GoalClock =
            clock { clockFilled = min clock.clockSegments (clock.clockFilled + 1) }
        | otherwise = clock

  BSNewScene -> do
    logInfo "[BetweenScenes] Starting new scene directly"
    createNewScene

  BSEndSession -> do
    logInfo "[BetweenScenes] Player chose to end session"
    emit $ NarrativeAdded "You fade into the city's eternal night. Until next time..."
    modify @WorldState $ \s -> s { phase = PhaseSessionEnded }

-- | Create a new scene after between-scenes
createNewScene :: DMM ()
createNewScene = do
  logInfo "[BetweenScenes] Creating continuation scene..."

  let contScene = ActiveScene
        { sceneLocation = LocationId "doskvol"
        , scenePresent = []
        , sceneStakes = Stakes "What happens next?"
        , sceneBeats = Seq.empty
        }

  modify @WorldState $ \s -> s
    { phase = PhasePlaying
    , scene = Just contScene
    , mood = MoodScene (Encounter "continuing" UrgencyLow True)
    }

  emit $ NarrativeAdded "---"
  emit $ NarrativeAdded "The city awaits your next move."

-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Roll starting dice pool (5 dice)
rollStartingDice :: DMM [Int]
rollStartingDice = replicateM 5 (randomInt 1 6)

-- | Convert ClockInit to Clock
initToClock :: ClockInit -> Clock
initToClock ci = Clock
  { clockName = ci.ciName
  , clockSegments = ci.ciSegments
  , clockFilled = ci.ciFilled
  , clockVisible = True
  , clockType = ci.ciType
  , clockConsequence = OpenOpportunity ci.ciConsequenceDesc
  , clockTriggers = []
  }

-- | DM dispatcher wrapping makeDMDispatcher
dmDispatcher :: AgentDispatcher WorldState DMEvent
dmDispatcher = AgentDispatcher $ \name input ->
  makeDMDispatcher name input
