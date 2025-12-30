{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
-- | DM Game Loop
module DM.Loop
  ( -- * Main Loop
    dmTurn
  , runDMGame
  , runDMGameWithDB

    -- * GUI Integration
  , gameLoopWithGUI
  , gameLoopWithGUIAndDB
  , waitForPlayerInput

    -- * Turn Operations
  , handlePlayerAction
  , checkClockConsequences
  , compressIfNeeded

    -- * Scene Management
  , startScene
  , endCurrentScene

    -- * Types
  , PlayerInput(..)
  , Response(..)
  ) where

import DM.State
import qualified DM.State
import DM.Context (buildDMContext, buildCompressionContext, DMContext(..))
import DM.Output (TurnOutput(..), CompressionOutput(..), applyTurnOutput, applyCompression, BargainLLMOutput(..), BargainLLMOption(..))
import DM.Templates (renderForMood, renderCompression, turnOutputSchema, compressionOutputSchema, bargainSchema, bargainTemplate)
import Tidepool.Template (render)
import DM.Tools (DMEvent(..), toolsForMood, makeDMDispatcherWithPhase)
import DM.GUI.Widgets.Events (formatEvent)
import Tidepool.Effect hiding (ToolResult)
import Tidepool.Template (Schema(..))

import Effectful
import Control.Concurrent.MVar (takeMVar)
import Control.Concurrent.STM (atomically, writeTVar, readTVar)
import Control.Monad (when, replicateM, forM_)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import System.IO (hFlush, stdout)
import System.Environment (lookupEnv)
import Database.SQLite.Simple (Connection)
import qualified Tidepool.Storage as Storage

import DM.CharacterCreation (CharacterChoices(..), scenarioInitPrompt, ScenarioInit(..), ClockInit(..), LocationInit(..), FactionInit(..), NpcInit(..))
import qualified DM.CharacterCreation as CC

import Tidepool.GUI.Core (GUIBridge(..), PendingRequest(..),
                          updateState, addNarrative, setLLMActive, setSuggestedActions)
import Tidepool.Effect (LLMHooks(..), runLLMWithToolsHooked)
import qualified Tidepool.GUI.Core as GUI (logInfo)
import Tidepool.GUI.Handler (makeGUIHandler)
import Tidepool.Anthropic.Http (Message(..), ContentBlock(..), Role(..))
import Data.Aeson (Value(..))
import Data.Maybe (catMaybes)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as Key
import qualified Data.Vector as V
import Prelude

-- ══════════════════════════════════════════════════════════════
-- TYPES
-- ══════════════════════════════════════════════════════════════

data PlayerInput = PlayerInput
  { piActionText :: Text
  , piActionTags :: [Tag]
  }
  deriving (Show, Eq)

data Response = Response
  { responseText :: Text
  , responseStressDelta :: Int
  , responseCoinDelta :: Int
  , responseHeatDelta :: Int
  , responseSuggestedActions :: [Text]
  }
  deriving (Show, Eq)

-- ══════════════════════════════════════════════════════════════
-- MAIN LOOP
-- ══════════════════════════════════════════════════════════════

-- | Run a single DM turn
-- Uses the new effect API: build context, call runTurn, apply output
-- When a tool triggers a state transition (TurnBroken), recursively restarts
-- with the new mood state.
dmTurn
  :: ( State WorldState :> es
     , Random :> es
     , LLM :> es
     , Emit DMEvent :> es
     , RequestInput :> es
     , Log :> es
     )
  => PlayerInput
  -> Eff es Response
dmTurn input = do
  -- Capture starting mood - used to detect trauma completion
  startingMood <- gets @WorldState currentMood

  -- 1. Record player action as scene beat (only on first call, not restarts)
  handlePlayerAction input

  -- 2. Run the turn (may restart on mood transition)
  response <- runMoodAwareTurn input.piActionText

  -- 3. If we STARTED in MoodTrauma, the turn just processed it - return to scene
  -- This ensures trauma gets one full turn to be narrated before transitioning
  case startingMood of
    Just (MoodTrauma traumaVariant) -> do
      logInfo "Trauma turn completed, returning to scene"
      -- Build context for the Weaver about what just happened
      let traumaContext = TraumaToSceneContext
            { ttscTraumaGained = traumaVariant.tvTraumaType.unTrauma
            , ttscAdrenalineActive = traumaVariant.tvAdrenaline
            }
      modify @WorldState $ \s -> s { sceneEntryContext = Just (EntryFromTrauma traumaContext) }
      modify @WorldState $ updateMood (MoodScene (Encounter "aftermath of breakdown" UrgencyLow True))
    _ -> return ()

  return response

  where
    runMoodAwareTurn userAction = do
      -- Get current mood and scene from phase
      state <- get @WorldState
      -- Extract scene and mood from PhasePlaying (required for turn)
      case state.phase of
        PhasePlaying scene mood -> do
          -- Special handling for MoodBargain: use structured output + requestChoice
          case mood of
            MoodBargain bargainVariant -> handleBargainTurn scene bargainVariant
            _ -> handleNormalTurn scene mood userAction

        otherPhase -> do
          -- Not in PhasePlaying - log warning and return empty response
          logWarn $ "runMoodAwareTurn called in wrong phase: " <> T.pack (show otherPhase)
          return Response
            { responseText = "*Something went wrong. Please continue.*"
            , responseStressDelta = 0
            , responseCoinDelta = 0
            , responseHeatDelta = 0
            , responseSuggestedActions = ["Continue"]
            }

    -- | Handle normal turns (Scene, Action, Aftermath, Trauma)
    handleNormalTurn scene mood userAction = do
          state <- get @WorldState
          let context = buildDMContext scene mood state
          let loc = context.ctxLocation
          let npcs = context.ctxPresentNpcs
          let beats = context.ctxSceneBeats
          logDebug $ "Current mood: " <> T.pack (show mood)
          logDebug $ "Context location: " <> loc.locationName
          logDebug $ "Context stakes: " <> context.ctxStakes
          logDebug $ "Context NPCs present: " <> T.pack (show (length npcs))
          logDebug $ "Context scene beats: " <> T.pack (show (length beats))
          logDebug $ "User action: " <> userAction

          -- Build system prompt (rules + mood guidance + world state)
          let systemPrompt = renderForMood mood context

          -- Log prompt details (visible in debug panel)
          logDebug $ "System prompt length: " <> T.pack (show $ T.length systemPrompt) <> " chars"
          logDebug $ "Prompt preview: " <> T.take 200 systemPrompt <> "..."

          -- Call LLM with system prompt and user action (tools filtered by current mood)
          let Schema{schemaJSON = outputSchema} = turnOutputSchema
          outcome <- runTurn @TurnOutput systemPrompt userAction outputSchema (toolsForMood mood)

          case outcome of
            -- Tool triggered state transition - restart with new mood
            TurnBroken reason -> do
              logInfo $ "Mood transition: " <> reason
              runMoodAwareTurn userAction  -- Recursive call with new mood

            -- Turn completed - handle parse result
            TurnCompleted parseResult -> case parseResult of
              -- Parse failed - log warning and use fallback
              TurnParseFailed{..} -> do
                logWarn $ "Failed to parse LLM output: " <> T.pack tpfError
                logWarn $ "Raw JSON: " <> TE.decodeUtf8 (LBS.toStrict $ Aeson.encode tpfRawJson)
                logWarn $ "Tools invoked: " <> T.pack (show $ map (\ti -> ti.tiName) tpfToolsInvoked)
                logWarn $ "Narrative was: " <> (if T.null tpfNarrative then "(empty)" else T.take 100 tpfNarrative)

                -- Try to extract narration from raw JSON first (don't show raw JSON to user!)
                let fallbackNarration = case extractNarrationFromJson tpfRawJson of
                      Just txt -> txt
                      Nothing -> "*The world waits for your next move.*"
                return Response
                  { responseText = fallbackNarration
                  , responseStressDelta = 0
                  , responseCoinDelta = 0
                  , responseHeatDelta = 0
                  , responseSuggestedActions = extractSuggestionsFromJson tpfRawJson
                  }

              -- Parsed successfully - apply output and continue
              TurnParsed result -> do
                -- Log LLM result
                logInfo $ "LLM turn complete - tools: " <> T.pack (show $ length result.trToolsInvoked)
                logDebug $ "Narrative: " <> T.take 150 result.trNarrative <> "..."

                -- Capture state BEFORE applying output (for accurate delta display)
                stateBefore <- get @WorldState

                -- Apply structured output to world state
                -- Note: Dice mechanics are handled by the SpendDie tool, not inline structured output.
                -- The flow is: Action → spend_die tool → resolve tool → Aftermath → accept → Scene
                modify (applyTurnOutput result.trOutput)

                -- Record DM response as scene beat (for history)
                let narration = result.trOutput.narration
                modify $ \s -> case s.phase of
                  PhasePlaying activeScene m ->
                    let beat = DMNarration narration
                        newBeats = activeScene.sceneBeats Seq.|> beat
                    in s { phase = PhasePlaying (activeScene { sceneBeats = newBeats }) m }
                  _ -> s

                -- Check clock consequences
                checkClockConsequences

                -- Check if stress hit max (trauma trigger)
                -- Pass current mood to detect if trauma happened during combat
                checkTraumaTrigger mood stateBefore.player.stress

                -- Compress if scene is getting long
                compressIfNeeded

                -- Calculate ACTUAL deltas (not LLM's claimed deltas, which ignore clamping)
                stateAfter <- get @WorldState
                let actualStressDelta = stateAfter.player.stress - stateBefore.player.stress
                    actualCoinDelta = stateAfter.player.coin - stateBefore.player.coin
                    actualHeatDelta = stateAfter.player.heat - stateBefore.player.heat

                -- Emit state change events for GUI display
                -- Use narration as context (truncated if too long)
                let eventContext = if T.length narration > 50
                      then T.take 47 narration <> "..."
                      else narration
                emitStateChangeEvents stateBefore stateAfter eventContext

                -- Return response with narrative and actual mechanical changes
                return Response
                  { responseText = narration
                  , responseStressDelta = actualStressDelta
                  , responseCoinDelta = actualCoinDelta
                  , responseHeatDelta = actualHeatDelta
                  , responseSuggestedActions = result.trOutput.suggestedActions
                  }

    -- | Handle bargain turn: LLM generates options, player picks via requestChoice
    handleBargainTurn scene bargainVariant = do
      stateBefore <- get @WorldState
      let mood = MoodBargain bargainVariant
      let context = buildDMContext scene mood stateBefore

      logInfo $ "BARGAIN - Out of dice: " <> bargainVariant.bvWhatDrained

      -- Render bargain template and call LLM (no tools needed)
      let systemPrompt = render bargainTemplate context
      let Schema{schemaJSON = outputSchema} = bargainSchema

      logDebug $ "Bargain prompt length: " <> T.pack (show $ T.length systemPrompt) <> " chars"

      -- Call LLM with bargain schema (no tools)
      outcome <- runTurn @BargainLLMOutput systemPrompt "waiting for options" outputSchema []

      case outcome of
        TurnBroken reason -> do
          -- Shouldn't happen with no tools, but handle gracefully
          logWarn $ "Bargain turn broken (unexpected): " <> reason
          return Response
            { responseText = "*The city waits. Something went wrong.*"
            , responseStressDelta = 0
            , responseCoinDelta = 0
            , responseHeatDelta = 0
            , responseSuggestedActions = ["Try again"]
            }

        TurnCompleted parseResult -> case parseResult of
          TurnParseFailed{..} -> do
            logWarn $ "Failed to parse bargain output: " <> T.pack tpfError
            return Response
              { responseText = "*The city grows impatient. Something went wrong.*"
              , responseStressDelta = 0
              , responseCoinDelta = 0
              , responseHeatDelta = 0
              , responseSuggestedActions = ["Try again"]
              }

          TurnParsed result -> do
            let bargainOutput = result.trOutput
            logInfo $ "Bargain LLM returned " <> T.pack (show $ length bargainOutput.bargainOptions) <> " options"

            -- Emit the opening narration
            emit (NarrativeAdded bargainOutput.bargainNarration)

            -- Build choice list: bargain options + retreat (if available) + pass out
            let bargainChoices = [(opt.bloLabel <> " — " <> opt.bloHint, Left opt) | opt <- bargainOutput.bargainOptions]
            let retreatChoice = case (bargainOutput.bargainCanRetreat, bargainOutput.bargainRetreatDesc) of
                  (True, Just desc) -> [("Retreat: " <> desc, Right "retreat")]
                  (True, Nothing) -> [("Retreat: slip away", Right "retreat")]
                  _ -> []
            let passOutChoice = [("Pass out", Right "passout")]
            let allChoices = bargainChoices ++ retreatChoice ++ passOutChoice

            -- Present choice via requestChoice
            choice <- requestChoice "The city waits. What will you trade?" allChoices

            case choice of
              Left chosenOption -> do
                -- Player chose a bargain option - apply costs and grant dice
                logInfo $ "Player chose bargain: " <> chosenOption.bloLabel

                -- Show player's choice
                emit (NarrativeAdded $ "> " <> chosenOption.bloLabel)

                -- Reveal the narrative (consequences land)
                emit (NarrativeAdded chosenOption.bloNarrative)

                -- Apply the cost
                applyBargainCost chosenOption

                -- Grant dice
                modify @WorldState $ \s -> s
                  { dicePool = addDiceToPool chosenOption.bloDiceGained s.dicePool }

                -- Check clock consequences (bargain may have advanced a clock to completion)
                checkClockConsequences

                -- Transition back to the scene that led to bargain
                let returnMood = MoodScene (Encounter "after the bargain" UrgencyMedium True)
                modify @WorldState $ updateMood returnMood

                stateAfter <- get @WorldState
                let actualStressDelta = stateAfter.player.stress - stateBefore.player.stress
                    actualCoinDelta = stateAfter.player.coin - stateBefore.player.coin
                    actualHeatDelta = stateAfter.player.heat - stateBefore.player.heat

                -- Emit state change events for GUI event log
                let eventContext = "bargain: " <> chosenOption.bloLabel
                emitStateChangeEvents stateBefore stateAfter eventContext

                return Response
                  { responseText = chosenOption.bloNarrative
                  , responseStressDelta = actualStressDelta
                  , responseCoinDelta = actualCoinDelta
                  , responseHeatDelta = actualHeatDelta
                  , responseSuggestedActions = ["Continue", "Take stock", "Press on"]
                  }

              Right "retreat" -> do
                -- Player chose to retreat - transition to BetweenScenes
                logInfo "Player chose to retreat from bargain"

                emit (NarrativeAdded "> Retreat")
                emit (NarrativeAdded "*You slip away. The debt remains unpaid.*")

                -- Reset dice to 3 (retreat benefit)
                modify @WorldState $ \s -> s
                  { dicePool = resetDicePool 3 s.dicePool }

                -- Transition to BetweenScenes
                let bsContext = BetweenScenesContext
                      { bscClocks = []  -- Will be filled by handleBetweenScenes
                      , bscTransitionNarration = "You retreat into the city's embrace."
                      }
                modify @WorldState $ \s -> s { phase = PhaseBetweenScenes bsContext }

                return Response
                  { responseText = "*You slip away. The debt remains unpaid. Dice restored.*"
                  , responseStressDelta = 0
                  , responseCoinDelta = 0
                  , responseHeatDelta = 0
                  , responseSuggestedActions = ["Find safety", "Lay low", "Regroup"]
                  }

              Right "passout" -> do
                -- Player chose to pass out
                logInfo "Player chose to pass out"

                emit (NarrativeAdded "> Pass out")
                emit (NarrativeAdded "*Darkness takes you. You wake somewhere else.*")

                let stressBeforePassout = stateBefore.player.stress

                -- Apply pass out costs: +2 stress, tick 1-2 threat clocks
                modify @WorldState $ \s -> s
                  { player = s.player { stress = min 9 (s.player.stress + 2) }
                  , dicePool = resetDicePool 2 s.dicePool  -- Minimal dice
                  }

                -- Tick threat clocks (1-2 random)
                tickRandomThreatClocks 1

                -- Emit stress change event
                stateAfterPassout <- get @WorldState
                let actualStressDelta = stateAfterPassout.player.stress - stressBeforePassout
                when (actualStressDelta /= 0) $
                  emit (StressChanged stressBeforePassout stateAfterPassout.player.stress "passed out")

                -- Transition to BetweenScenes
                let bsContext = BetweenScenesContext
                      { bscClocks = []
                      , bscTransitionNarration = "You wake somewhere unfamiliar. Time has passed."
                      }
                modify @WorldState $ \s -> s { phase = PhaseBetweenScenes bsContext }

                return Response
                  { responseText = "*Darkness takes you. You wake somewhere else. +2 stress.*"
                  , responseStressDelta = actualStressDelta
                  , responseCoinDelta = 0
                  , responseHeatDelta = 0
                  , responseSuggestedActions = ["Get your bearings", "Find out what happened"]
                  }

              Right other -> do
                logWarn $ "Unknown bargain choice: " <> other
                return Response
                  { responseText = "*Something went wrong.*"
                  , responseStressDelta = 0
                  , responseCoinDelta = 0
                  , responseHeatDelta = 0
                  , responseSuggestedActions = ["Try again"]
                  }

    -- | Apply bargain cost based on cost type
    applyBargainCost opt = case opt.bloCostType of
      "stress" -> modify @WorldState $ \s -> s
        { player = s.player { stress = min 9 (s.player.stress + opt.bloCostAmount) } }
      "heat" -> modify @WorldState $ \s -> s
        { player = s.player { heat = min 10 (s.player.heat + opt.bloCostAmount) } }
      "clock" -> case opt.bloCostTarget of
        Just clockIdText -> do
          let cid = ClockId clockIdText
          modify @WorldState $ \s -> s
            { clocks = HM.adjust (\c -> c { clockFilled = min c.clockSegments (c.clockFilled + opt.bloCostAmount) }) cid s.clocks }
        Nothing -> logWarn "Clock bargain without target - ignoring"
      "faction" -> case opt.bloCostTarget of
        Just factionIdText -> do
          let fid = FactionId factionIdText
          modify @WorldState $ \s -> s
            { factions = HM.adjust (\f -> f { factionAttitude = worsenAttitude f.factionAttitude }) fid s.factions }
        Nothing -> logWarn "Faction bargain without target - ignoring"
      "trauma" -> modify @WorldState $ \s -> s
        { player = s.player
          { stress = 0  -- Stress resets with trauma
          , trauma = Trauma "Desperate Bargain" : s.player.trauma
          }
        }
      other -> logWarn $ "Unknown bargain cost type: " <> other

    -- | Add dice to pool
    addDiceToPool n pool =
      let currentLen = length pool.poolDice
          -- Use varied values based on current pool size (deterministic but varied)
          newDice = take n $ drop (currentLen `mod` 6) $ cycle [4, 5, 3, 6, 2, 1]
      in pool { poolDice = pool.poolDice ++ newDice }

    -- | Reset dice pool to N dice with varied values
    resetDicePool n pool = pool { poolDice = take n [4, 3, 5, 2, 6, 1, 4, 3] }

    -- | Tick a random threat clock by n segments
    tickRandomThreatClocks n = do
      state <- get @WorldState
      let threatClocks = HM.filter (\c -> c.clockType == ThreatClock) state.clocks
      let clockIds = HM.keys threatClocks
      case clockIds of
        [] -> pure ()
        [cid] -> tickClock cid n  -- Only one clock, tick it
        _ -> do
          -- Multiple clocks: pick one at random
          idx <- randomInt 0 (length clockIds - 1)
          let cid = clockIds !! idx
          tickClock cid n
      where
        tickClock cid amount = modify @WorldState $ \s -> s
          { clocks = HM.adjust (\c -> c { clockFilled = min c.clockSegments (c.clockFilled + amount) }) cid s.clocks }

    -- | Make a faction attitude worse (Allied -> Favorable -> Neutral -> Wary -> Hostile)
    worsenAttitude :: Attitude -> Attitude
    worsenAttitude Allied = Favorable
    worsenAttitude Favorable = Neutral
    worsenAttitude Neutral = Wary
    worsenAttitude Wary = Hostile
    worsenAttitude Hostile = Hostile  -- Can't get worse

-- NOTE: Dice selection is now handled by the SpendDie tool during LLM turn
-- The tool modifies state directly and returns the outcome to the LLM

handlePlayerAction
  :: State WorldState :> es
  => PlayerInput
  -> Eff es ()
handlePlayerAction input = modify $ \state ->
  case state.phase of
    PhasePlaying activeScene mood ->
      let beat = PlayerAction input.piActionText input.piActionTags
          newBeats = activeScene.sceneBeats Seq.|> beat
      in state { phase = PhasePlaying (activeScene { sceneBeats = newBeats }) mood }
    _ -> state  -- Not playing, nothing to do

checkClockConsequences
  :: ( State WorldState :> es
     , Emit DMEvent :> es
     )
  => Eff es ()
checkClockConsequences = do
  state <- get @WorldState
  let allClocks = HM.toList state.clocks
      (completed, remaining) = foldr partitionClock ([], HM.empty) allClocks

  -- Emit events for each completed clock (LLM interprets narrative consequences)
  forM_ completed $ \(ClockId clockId, clock) -> do
    emit $ ClockCompleted clockId clock.clockName clock.clockConsequence

  -- Remove completed clocks from state
  modify @WorldState $ \s -> s { clocks = remaining }
  where
    partitionClock (clockId, clock) (done, keep)
      | clock.clockFilled >= clock.clockSegments = ((clockId, clock) : done, keep)
      | otherwise = (done, HM.insert clockId clock keep)

-- | Check if stress just hit max (trauma trigger)
-- If player crossed from <9 to 9, transition to trauma mood
checkTraumaTrigger
  :: ( State WorldState :> es
     , Log :> es
     , Emit DMEvent :> es
     )
  => DMMood  -- The mood when trauma was triggered
  -> Int     -- Stress BEFORE this turn
  -> Eff es ()
checkTraumaTrigger triggerMood stressBefore = do
  state <- get @WorldState
  let stressNow = state.player.stress
  when (stressNow >= 9 && stressBefore < 9) $ do
    logWarn "TRAUMA TRIGGERED: Stress hit maximum"
    -- Don't emit TraumaTriggered yet - wait for LLM to determine the actual trauma
    -- The trauma turn will narrate the breaking point and assign the trauma
    -- TraumaTriggered event will be emitted AFTER the trauma turn completes
    let inCombat = case triggerMood of
          MoodAction _ _ -> True  -- In action = adrenaline is flowing
          _ -> False
    let traumaVariant = Breaking
          { tvWhatBroke = "stress overflow"  -- LLM will elaborate
          , tvTraumaType = Trauma "pending"  -- LLM will determine actual trauma
          , tvTrigger = "accumulated pressure"
          , tvAdrenaline = inCombat  -- True if trauma triggered during action
          }
    modify @WorldState $ updateMood (MoodTrauma traumaVariant)

-- | Emit state change events by comparing before/after states
--
-- This function examines the differences between two world states and
-- emits appropriate events for any significant changes. These events
-- are displayed in the GUI narrative with distinctive styling.
emitStateChangeEvents
  :: Emit DMEvent :> es
  => WorldState  -- ^ State before the turn
  -> WorldState  -- ^ State after the turn
  -> Text        -- ^ Reason/context for the changes
  -> Eff es ()
emitStateChangeEvents before after reason = do
  let pb = before.player
      pa = after.player

  -- Stress changes
  when (pa.stress /= pb.stress) $
    emit $ StressChanged
      { seFrom = pb.stress
      , seTo = pa.stress
      , seReason = reason
      }

  -- Heat changes
  when (pa.heat /= pb.heat) $
    emit $ HeatChanged
      { heFrom = pb.heat
      , heTo = pa.heat
      , heReason = reason
      }

  -- Wanted changes
  when (pa.wanted /= pb.wanted) $
    emit $ WantedChanged
      { weFrom = pb.wanted
      , weTo = pa.wanted
      , weReason = reason
      }

  -- Coin changes
  when (pa.coin /= pb.coin) $
    emit $ CoinChanged
      { ceFrom = pb.coin
      , ceTo = pa.coin
      , ceReason = reason
      }

  -- Dice pool depletion
  let poolBefore = length before.dicePool.poolDice
      poolAfter = length after.dicePool.poolDice
  when (poolBefore > 0 && poolAfter == 0) $
    emit $ DicePoolDepleted { dpContext = reason }

  -- Bargain mood transition
  case (currentMood before, currentMood after) of
    (beforeMood, Just (MoodBargain bv)) | not (isBargainMood beforeMood) ->
      emit $ BargainOffered
        { boContext = bv.bvWhatDrained
        , boCanRetreat = bv.bvCanRetreat
        }
    _ -> pure ()

  -- Trauma assigned (new trauma in list that wasn't there before)
  -- pa = after, pb = before (from the let binding at top)
  let newTraumas = filter (`notElem` pb.trauma) pa.trauma
  case newTraumas of
    (newTrauma:_) -> emit $ TraumaTriggered
      { ttTrauma = newTrauma
      , ttTrigger = reason
      , ttBreakingPoint = "The pressure finally became too much."
      }
    [] -> pure ()
  where
    isBargainMood (Just (MoodBargain _)) = True
    isBargainMood _ = False

-- | Compress scene if it has too many beats
-- Threshold: 20 beats triggers compression
compressIfNeeded
  :: ( State WorldState :> es
     , LLM :> es
     , Emit DMEvent :> es
     , Log :> es
     )
  => Eff es ()
compressIfNeeded = do
  state <- get
  case currentScene state of
    Nothing -> return ()  -- No active scene
    Just activeScene ->
      when (Seq.length activeScene.sceneBeats >= compressionThreshold) $ do
        -- Build compression context
        let ctx = buildCompressionContext activeScene state
            -- For compression, the "system prompt" is the compression template
            -- and the "user action" is just a trigger phrase
            systemPrompt = renderCompression ctx
            userAction = "Compress this scene."

        -- Call LLM to compress the scene
        let Schema{schemaJSON = compressSchema} = compressionOutputSchema
        outcome <- runTurn @CompressionOutput systemPrompt userAction compressSchema []

        -- Compression shouldn't break, but handle it gracefully
        case outcome of
          TurnBroken reason -> do
            logWarn $ "Compression unexpectedly broke: " <> reason
            -- Continue without compressing

          TurnCompleted parseResult -> case parseResult of
            TurnParseFailed{..} -> do
              logWarn $ "Compression parse failed: " <> T.pack tpfError
              logWarn $ "Raw JSON: " <> TE.decodeUtf8 (LBS.toStrict $ Aeson.encode tpfRawJson)
              -- Continue without compressing

            TurnParsed result -> do
              -- Build scene summary from compression output
              let sceneSummary = SceneSummary
                    { summaryText = result.trOutput.summary
                    , summaryKeyMoments = result.trOutput.keyMoments
                    }

              -- Add to session history
              modify $ \s -> s { sessionHistory = s.sessionHistory Seq.|> sceneSummary }

              -- Apply compression output to world state
              modify (applyCompression result.trOutput)

              -- Clear scene beats (keep scene active with empty beats)
              modify $ \s -> case s.phase of
                PhasePlaying scene m -> s { phase = PhasePlaying (scene { sceneBeats = Seq.empty }) m }
                _ -> s

              -- Emit compression event
              emit $ SceneCompressed result.trOutput.summary
  where
    compressionThreshold = 20


-- ══════════════════════════════════════════════════════════════
-- SCENE MANAGEMENT
-- ══════════════════════════════════════════════════════════════

-- | Start a new scene at a location (transitions to PhasePlaying)
startScene
  :: State WorldState :> es
  => LocationId
  -> [NpcId]
  -> Stakes
  -> Eff es ()
startScene locationId npcsPresent stakes = modify $ \state ->
  let newScene = ActiveScene
        { sceneLocation = locationId
        , scenePresent = npcsPresent
        , sceneStakes = stakes
        , sceneBeats = Seq.empty
        , sceneStyle = defaultSceneStyle
        }
      -- Start with default scene mood
      newMood = defaultMood
  in state { phase = PhasePlaying newScene newMood }

-- | End the current scene, transitioning to BetweenScenes
-- Note: Call compressIfNeeded before this to preserve scene summary
-- The context should be populated by the caller with LLM-generated transition text
endCurrentScene
  :: State WorldState :> es
  => BetweenScenesContext
  -> Eff es ()
endCurrentScene ctx = modify $ \state -> state { phase = PhaseBetweenScenes ctx }

-- ══════════════════════════════════════════════════════════════
-- RUNNING THE GAME
-- ══════════════════════════════════════════════════════════════

-- | Run the DM game with terminal I/O
-- Returns the final WorldState for persistence
-- Takes a save callback that's called after each turn for auto-save
runDMGame
  :: WorldState
  -> (DMEvent -> IO ())
  -> (WorldState -> IO ())  -- Save callback, called after each turn
  -> IO WorldState
runDMGame initialState handleEvent saveState = do
  -- Get API key from environment
  maybeKey <- lookupEnv "ANTHROPIC_API_KEY"
  case maybeKey of
    Nothing -> do
      TIO.putStrLn "Error: ANTHROPIC_API_KEY not set"
      TIO.putStrLn "Run: export ANTHROPIC_API_KEY=your-key-here"
      return initialState  -- Return unchanged state
    Just apiKey -> do
      -- Set up the LLM config
      let llmConfig = LLMConfig
            { llmApiKey = T.pack apiKey
            , llmModel = "claude-haiku-4-5-20251001"
            , llmMaxTokens = 4096
            , llmThinkingBudget = Nothing  -- Disabled: faster, fixes structured output + tool use  -- Enable extended thinking
            }

      -- Set up terminal input handler
      let inputHandler = InputHandler
            { ihChoice = terminalChoice
            , ihText = terminalText
            , ihDice = terminalDice
            }

      -- Start with an intro scene in PhasePlaying
      let introScene = ActiveScene
            { sceneLocation = LocationId "intro"
            , scenePresent = []
            , sceneStakes = Stakes "Establish who you are in Doskvol"
            , sceneBeats = Seq.empty
            , sceneStyle = defaultSceneStyle
            }
          stateWithScene = initialState
            { phase = PhasePlaying introScene defaultMood
            }

      TIO.putStrLn "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
      TIO.putStrLn "DOSKVOL. Industrial sprawl. Eternal night."
      TIO.putStrLn "The ghosts press against the lightning barriers."
      TIO.putStrLn "The gangs carve up the districts."
      TIO.putStrLn "You're about to step into the streets."
      TIO.putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
      TIO.putStrLn "\n[Type 'quit' to exit]\n"
      TIO.putStrLn "Who are you? What brings you to Crow's Foot tonight?\n"

      -- Run the game loop with auto-save (Debug level shows all log messages)
      -- Use runLLMWithTools with real DM dispatcher (not stub executor!)
      ((), finalState) <- runEff
        . runRandom
        . runEmit handleEvent
        . runState stateWithScene
        . runChatHistory
        . runLog Debug
        . runRequestInput inputHandler
        . runLLMWithTools @_ @DMEvent llmConfig makeDMDispatcherWithPhase
        $ gameLoopWithSave saveState

      TIO.putStrLn "Game ended."
      return finalState

-- | Run the DM game with SQLite database persistence
-- Uses DB-backed chat history and saves world state after each turn
runDMGameWithDB
  :: Connection
  -> Storage.GameId
  -> Maybe Int         -- ^ Compression cursor (load messages after this)
  -> WorldState
  -> (DMEvent -> IO ())
  -> IO WorldState
runDMGameWithDB conn gameId mCursor initialState handleEvent = do
  -- Get API key from environment
  maybeKey <- lookupEnv "ANTHROPIC_API_KEY"
  case maybeKey of
    Nothing -> do
      TIO.putStrLn "Error: ANTHROPIC_API_KEY not set"
      TIO.putStrLn "Run: export ANTHROPIC_API_KEY=your-key-here"
      return initialState
    Just apiKey -> do
      let llmConfig = LLMConfig
            { llmApiKey = T.pack apiKey
            , llmModel = "claude-haiku-4-5-20251001"
            , llmMaxTokens = 4096
            , llmThinkingBudget = Nothing  -- Disabled: faster, fixes structured output + tool use
            }

      let inputHandler = InputHandler
            { ihChoice = terminalChoice
            , ihText = terminalText
            , ihDice = terminalDice
            }

      -- Preserve loaded phase, or create intro scene if not playing
      let stateWithScene = case initialState.phase of
            PhasePlaying _ _ -> initialState  -- Keep the loaded scene
            _ -> initialState
              { phase = PhasePlaying
                  (ActiveScene
                    { sceneLocation = LocationId "intro"
                    , scenePresent = []
                    , sceneStakes = Stakes "Establish who you are in Doskvol"
                    , sceneBeats = Seq.empty
                    , sceneStyle = defaultSceneStyle
                    })
                  defaultMood
              }

      TIO.putStrLn "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
      case initialState.phase of
        PhasePlaying _ _ -> TIO.putStrLn "Resuming your story in Doskvol..."
        _ -> do
          TIO.putStrLn "DOSKVOL. Industrial sprawl. Eternal night."
          TIO.putStrLn "The ghosts press against the lightning barriers."
          TIO.putStrLn "The gangs carve up the districts."
          TIO.putStrLn "You're about to step into the streets."
      TIO.putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
      TIO.putStrLn "\n[Type 'quit' to exit]\n"
      case currentScene initialState of
        Just _ -> return ()  -- Don't repeat intro for resumed games
        Nothing -> TIO.putStrLn "Who are you? What brings you to Crow's Foot tonight?\n"

      -- Run with DB-backed chat history
      ((), finalState) <- runEff
        . runRandom
        . runEmit handleEvent
        . runState stateWithScene
        . runChatHistoryWithDB conn gameId mCursor
        . runLog Debug
        . runRequestInput inputHandler
        . runLLMWithTools @_ @DMEvent llmConfig makeDMDispatcherWithPhase
        $ gameLoopWithDB conn gameId

      TIO.putStrLn "Game ended."
      return finalState

-- | Game loop that saves to database after each turn
gameLoopWithDB
  :: Connection
  -> Storage.GameId
  -> Eff (GameEffects WorldState DMEvent) ()
gameLoopWithDB conn gameId = loop []
  where
    loop :: [Text] -> Eff (GameEffects WorldState DMEvent) ()
    loop lastSuggestions = do
      state <- get @WorldState
      case currentScene state of
        Nothing -> do
          liftIO $ TIO.putStrLn "\n[No active scene. Use startScene to begin.]"
          return ()

        Just _ -> do
          currentState <- get @WorldState
          let p = currentState.player
              statusLine = "Stress: " <> T.pack (show p.stress) <> "/9"
                        <> " | Coin: " <> T.pack (show p.coin)
                        <> " | Heat: " <> T.pack (show p.heat) <> "/10"
                        <> " | Dice: " <> renderDice currentState.dicePool.poolDice
              moodLabel = case currentMood currentState of
                Just (MoodScene _)     -> "SCENE"
                Just (MoodAction _ _)  -> "ACTION"
                Just (MoodAftermath _) -> "AFTERMATH"
                Just (MoodTrauma _)    -> "TRAUMA"
                Just (MoodBargain _)   -> "BARGAIN"
                Nothing                -> "---"
          liftIO $ TIO.putStrLn $ "\n[" <> statusLine <> "]"
          liftIO $ TIO.putStrLn $ "[" <> moodLabel <> "]"

          when (not $ null lastSuggestions) $
            liftIO $ TIO.putStr $ renderSuggestedActions lastSuggestions

          liftIO $ TIO.putStr "> "
          liftIO $ hFlush stdout
          inputText <- liftIO TIO.getLine

          if T.toLower (T.strip inputText) `elem` ["quit", "exit", "q"]
            then return ()
            else do
              let resolvedInput = resolvePlayerInput lastSuggestions inputText
                  playerInput = PlayerInput
                    { piActionText = resolvedInput
                    , piActionTags = []
                    }

              response <- dmTurn playerInput

              liftIO $ TIO.putStrLn ""
              liftIO $ TIO.putStrLn response.responseText

              updatedState <- get @WorldState
              let mechanicalChanges = renderMechanicalChanges response updatedState
              when (not $ T.null mechanicalChanges) $
                liftIO $ TIO.putStrLn mechanicalChanges

              -- Save world state to database
              liftIO $ Storage.saveGameState conn gameId updatedState

              loop response.responseSuggestedActions

-- | Game loop with auto-save after each turn
-- The save callback is captured in the closure
-- Threads last suggestions through for numeric input resolution
gameLoopWithSave
  :: (WorldState -> IO ())  -- Save callback
  -> Eff (GameEffects WorldState DMEvent) ()
gameLoopWithSave saveCallback = loop []
  where
    loop :: [Text] -> Eff (GameEffects WorldState DMEvent) ()
    loop lastSuggestions = do
      -- Check if we have an active scene
      state <- get @WorldState
      case currentScene state of
        Nothing -> do
          -- No scene - prompt to start one or quit
          liftIO $ TIO.putStrLn "\n[No active scene. Use startScene to begin.]"
          return ()  -- Exit for now - real game would prompt for scene setup

        Just _ -> do
          -- Show current status and mood
          currentState <- get @WorldState
          let p = currentState.player
              statusLine = "Stress: " <> T.pack (show p.stress) <> "/9"
                        <> " | Coin: " <> T.pack (show p.coin)
                        <> " | Heat: " <> T.pack (show p.heat) <> "/10"
                        <> " | Dice: " <> renderDice currentState.dicePool.poolDice
              moodLabel = case currentMood currentState of
                Just (MoodScene _)     -> "SCENE"
                Just (MoodAction _ _)  -> "ACTION"
                Just (MoodAftermath _) -> "AFTERMATH"
                Just (MoodTrauma _)    -> "TRAUMA"
                Just (MoodBargain _)   -> "BARGAIN"
                Nothing                -> "---"
          liftIO $ TIO.putStrLn $ "\n[" <> statusLine <> "]"
          liftIO $ TIO.putStrLn $ "[" <> moodLabel <> "]"

          -- Display last suggestions if any
          when (not $ null lastSuggestions) $
            liftIO $ TIO.putStr $ renderSuggestedActions lastSuggestions

          -- Get player input
          liftIO $ TIO.putStr "> "
          liftIO $ hFlush stdout
          inputText <- liftIO TIO.getLine

          -- Check for quit command
          if T.toLower (T.strip inputText) `elem` ["quit", "exit", "q"]
            then return ()  -- Exit loop
            else do
              -- Resolve input (numeric selection or freeform)
              let resolvedInput = resolvePlayerInput lastSuggestions inputText
                  playerInput = PlayerInput
                    { piActionText = resolvedInput
                    , piActionTags = []  -- Could parse tags from input
                    }

              -- Run a turn
              response <- dmTurn playerInput

              -- Display response
              liftIO $ TIO.putStrLn ""
              liftIO $ TIO.putStrLn response.responseText

              -- Display mechanical state changes
              updatedState <- get @WorldState
              let mechanicalChanges = renderMechanicalChanges response updatedState
              when (not $ T.null mechanicalChanges) $
                liftIO $ TIO.putStrLn mechanicalChanges
              liftIO $ saveCallback updatedState

              -- Continue loop with new suggestions
              loop response.responseSuggestedActions

-- | Render mechanical state changes for display
-- Shows deltas and current values for stress, coin, heat
renderMechanicalChanges :: Response -> WorldState -> Text
renderMechanicalChanges resp state =
  let parts = filter (not . T.null)
        [ renderDelta "Stress" resp.responseStressDelta state.player.stress 9
        , renderDelta "Coin" resp.responseCoinDelta state.player.coin (-1)  -- No max
        , renderDelta "Heat" resp.responseHeatDelta state.player.heat 10
        ]
  in if null parts
     then ""
     else "\n" <> T.intercalate " | " parts
  where
    renderDelta :: Text -> Int -> Int -> Int -> Text
    renderDelta name delta current maxVal
      | delta == 0 = ""
      | delta > 0  = name <> ": " <> T.pack (show (current - delta)) <> " → " <> T.pack (show current)
                     <> " (+" <> T.pack (show delta) <> ")"
                     <> if maxVal > 0 && current >= maxVal then " ⚠️" else ""
      | otherwise  = name <> ": " <> T.pack (show (current - delta)) <> " → " <> T.pack (show current)
                     <> " (" <> T.pack (show delta) <> ")"

-- | Render dice pool for display
renderDice :: [Int] -> Text
renderDice dice = T.intercalate " " $ map dieFace dice
  where
    dieFace n = case n of
      1 -> "⚀"
      2 -> "⚁"
      3 -> "⚂"
      4 -> "⚃"
      5 -> "⚄"
      6 -> "⚅"
      _ -> T.pack (show n)

-- | Show phase for logging
showPhase :: GamePhase -> String
showPhase PhaseCharacterCreation = "CharacterCreation"
showPhase (PhaseScenarioInit _) = "ScenarioInit"
showPhase (PhasePlaying _ _) = "Playing"
showPhase (PhaseBetweenScenes _) = "BetweenScenes"
showPhase PhaseSessionEnded = "SessionEnded"

-- | Render suggested actions for display
renderSuggestedActions :: [Text] -> Text
renderSuggestedActions [] = ""
renderSuggestedActions actions = T.unlines $
  [""] ++
  zipWith (\i a -> "[" <> T.pack (show i) <> "] " <> a) [1 :: Int ..] actions

-- | Resolve player input - if numeric, select from suggestions
-- Otherwise use as freeform text
resolvePlayerInput :: [Text] -> Text -> Text
resolvePlayerInput suggestions input =
  let stripped = T.strip input
  in case reads (T.unpack stripped) :: [(Int, String)] of
    [(n, "")] | n >= 1 && n <= length suggestions ->
      suggestions !! (n - 1)
    _ -> input  -- Use as-is if not a valid number

-- | Terminal choice handler - display options and get selection
terminalChoice :: Text -> [(Text, a)] -> IO a
terminalChoice prompt options = do
  TIO.putStrLn prompt
  mapM_ printOption (zip [1..] options)
  TIO.putStr "Enter choice (number): "
  hFlush stdout
  input <- TIO.getLine
  case reads (T.unpack input) of
    [(n, "")] | n >= 1 && n <= length options ->
      return $ snd (options !! (n - 1))
    _ -> do
      TIO.putStrLn "Invalid choice, try again."
      terminalChoice prompt options
  where
    printOption (n :: Int, (label, _)) =
      TIO.putStrLn $ "  " <> T.pack (show n) <> ". " <> label

-- | Terminal text input handler
terminalText :: Text -> IO Text
terminalText prompt = do
  TIO.putStr prompt
  TIO.putStr " "
  hFlush stdout
  TIO.getLine

-- | Terminal dice selection handler
-- Now includes LLM-generated hints for each outcome
terminalDice :: Text -> [(Int, Int, Text)] -> IO Int
terminalDice prompt diceWithHints = do
  TIO.putStrLn prompt
  mapM_ printDie (zip [1..] diceWithHints)
  TIO.putStr "Enter choice (number): "
  hFlush stdout
  input <- TIO.getLine
  case reads (T.unpack input) of
    [(n, "")] | n >= 1 && n <= length diceWithHints ->
      let (_, idx, _) = diceWithHints !! (n - 1)
      in return idx
    _ -> do
      TIO.putStrLn "Invalid choice, try again."
      terminalDice prompt diceWithHints
  where
    printDie (n :: Int, (dieValue, _idx, hint)) =
      let hintPart = if T.null hint then "" else " - " <> hint
      in TIO.putStrLn $ "  " <> T.pack (show n) <> ". Die showing " <> T.pack (show dieValue) <> hintPart

-- ══════════════════════════════════════════════════════════════
-- GUI INTEGRATION
-- ══════════════════════════════════════════════════════════════

-- | Wait for player input via GUI
--
-- Uses the RequestInput effect to get text input from the player.
-- Returns the input as a PlayerInput record.
waitForPlayerInput :: RequestInput :> es => Eff es PlayerInput
waitForPlayerInput = do
  txt <- requestText "What do you do?"
  pure $ PlayerInput txt []

-- | Run the game loop with GUI integration
--
-- This function runs in a background thread and:
-- 1. Waits for player input from GUI
-- 2. Runs dmTurn with GUI handler
-- 3. Syncs state to bridge (triggers GUI refresh)
-- 4. Pushes narrative to bridge
-- 5. Loops
gameLoopWithGUI
  :: GUIBridge WorldState
  -> (DMEvent -> IO ())
  -> IO ()
gameLoopWithGUI bridge handleEvent = do
  -- Get API key from environment
  maybeKey <- lookupEnv "ANTHROPIC_API_KEY"
  case maybeKey of
    Nothing -> do
      addNarrative bridge "Error: ANTHROPIC_API_KEY not set"
      return ()
    Just apiKey -> do
      -- Set up the LLM config
      let llmConfig = LLMConfig
            { llmApiKey = T.pack apiKey
            , llmModel = "claude-haiku-4-5-20251001"
            , llmMaxTokens = 4096
            , llmThinkingBudget = Nothing  -- Disabled: faster, fixes structured output + tool use
            }

      -- Set up GUI input handler
      let inputHandler = makeGUIHandler bridge

      -- Get initial state from bridge
      initialState <- atomically $ readTVar bridge.gbState

      -- Add intro narrative
      addNarrative bridge "DOSKVOL. Industrial sprawl. Eternal night."
      addNarrative bridge "The ghosts press against the lightning barriers."
      addNarrative bridge "The gangs carve up the districts."
      addNarrative bridge "You're about to step into the streets."

      -- Spinner hooks - show/hide loading indicator around LLM calls
      let spinnerHooks = LLMHooks
            { onTurnStart = setLLMActive bridge True
            , onTurnEnd = setLLMActive bridge False
            }

      -- Run the game loop (logs go to GUI debug panel)
      ((), _finalState) <- runEff
        . runRandom
        . runEmit handleEvent
        . runState initialState
        . runChatHistory
        . runLogWithBridge bridge Debug
        . runRequestInput inputHandler
        . runLLMWithToolsHooked @_ @DMEvent spinnerHooks llmConfig makeDMDispatcherWithPhase
        $ guiGameLoop bridge

      return ()

-- | Game loop for GUI - runs inside effect stack
guiGameLoop
  :: GUIBridge WorldState
  -> Eff (GameEffects WorldState DMEvent) ()
guiGameLoop bridge = loop
  where
    loop :: Eff (GameEffects WorldState DMEvent) ()
    loop = do
      -- Check if we have an active scene
      state <- get @WorldState

      -- Sync current state to bridge (so GUI shows latest)
      liftIO $ updateState bridge (const state)

      case state.phase of
        PhasePlaying _ _ -> do
          -- Active scene - wait for player input
          playerInput <- waitForPlayerInput

          -- Check for quit
          if T.toLower (T.strip playerInput.piActionText) `elem` ["quit", "exit", "q"]
            then return ()
            else do
              -- Show player's action in narrative
              emit (NarrativeAdded $ "> " <> playerInput.piActionText)

              -- Run a turn (spinner handled by LLM interpreter hooks)
              response <- dmTurn playerInput

              -- Push narrative via Emit effect
              emit (NarrativeAdded response.responseText)

              -- Sync updated state to bridge
              updatedState <- get @WorldState
              liftIO $ updateState bridge (const updatedState)

              -- Continue loop
              loop

        PhaseCharacterCreation -> do
          -- Need to create intro scene first
          let introScene = ActiveScene
                { sceneLocation = LocationId "crows_foot"
                , scenePresent = []
                , sceneStakes = Stakes "Establish who you are in Doskvol"
                , sceneBeats = Seq.empty
                , sceneStyle = defaultSceneStyle
                }
          modify @WorldState $ \s -> s { phase = PhasePlaying introScene defaultMood }
          emit (NarrativeAdded "Who are you? What brings you to Crow's Foot tonight?")
          loop

        _ -> do
          -- Other phases - for now just loop (will be handled by more complete dispatch)
          loop

-- ══════════════════════════════════════════════════════════════
-- GUI WITH DATABASE PERSISTENCE
-- ══════════════════════════════════════════════════════════════

-- | Run the game loop with GUI and database persistence
--
-- This is like 'gameLoopWithGUI' but:
-- 1. Loads history from database (respecting compression cursor)
-- 2. Populates narrative log with past conversation
-- 3. Persists new messages to database
-- 4. Saves world state after each turn
gameLoopWithGUIAndDB
  :: Connection
  -> Storage.GameId
  -> Maybe Int           -- ^ Compression cursor (messages before this are compressed)
  -> GUIBridge WorldState
  -> (DMEvent -> IO ())
  -> IO ()
gameLoopWithGUIAndDB conn gameId mCursor bridge handleEvent = do
  GUI.logInfo bridge "[Startup] gameLoopWithGUIAndDB starting..."

  -- Get API key from environment
  maybeKey <- lookupEnv "ANTHROPIC_API_KEY"
  case maybeKey of
    Nothing -> do
      addNarrative bridge "Error: ANTHROPIC_API_KEY not set"
      GUI.logInfo bridge "[Startup] ERROR: No API key!"
      return ()
    Just apiKey -> do
      GUI.logInfo bridge "[Startup] API key found, configuring..."

      -- Set up the LLM config
      let llmConfig = LLMConfig
            { llmApiKey = T.pack apiKey
            , llmModel = "claude-haiku-4-5-20251001"
            , llmMaxTokens = 4096
            , llmThinkingBudget = Nothing  -- Disabled: faster, fixes structured output + tool use
            }

      -- Set up GUI input handler
      let inputHandler = makeGUIHandler bridge

      -- Get initial state from bridge
      initialState <- atomically $ readTVar bridge.gbState
      GUI.logInfo bridge $ "[Startup] Initial state loaded, phase: " <> T.pack (showPhase initialState.phase)

      -- Load past messages and populate narrative log
      pastMessages <- Storage.loadMessages conn gameId
      GUI.logInfo bridge $ "[Startup] Loaded " <> T.pack (show $ length pastMessages) <> " past messages"
      let narrativeTexts = extractNarrativeTexts pastMessages
      GUI.logInfo bridge $ "[Startup] Extracted " <> T.pack (show $ length narrativeTexts) <> " narrative texts"
      mapM_ (addNarrative bridge) narrativeTexts

      -- Restore suggested actions from WorldState to bridge
      setSuggestedActions bridge initialState.suggestedActions
      GUI.logInfo bridge $ "[Startup] Restored " <> T.pack (show $ length initialState.suggestedActions) <> " suggested actions"

      -- Check if character creation is needed (fresh game)
      stateAfterCreation <- case initialState.phase of
        PhaseCharacterCreation -> do
          -- New game - trigger character creation
          GUI.logInfo bridge "[Startup] Fresh game - starting character creation..."
          runCharacterCreation bridge initialState
        _ -> do
          -- Character already created, use existing state
          GUI.logInfo bridge "[Startup] Character already exists, skipping creation"
          return initialState

      -- Update bridge with potentially modified state
      atomically $ writeTVar bridge.gbState stateAfterCreation

      GUI.logInfo bridge "[Startup] Starting game loop..."

      -- Spinner hooks - show/hide loading indicator around LLM calls
      let spinnerHooks = LLMHooks
            { onTurnStart = setLLMActive bridge True
            , onTurnEnd = setLLMActive bridge False
            }

      -- Run the game loop with DB persistence
      ((), finalState) <- runEff
        . runRandom
        . runEmit handleEvent
        . runState stateAfterCreation
        . runChatHistoryWithDB conn gameId mCursor
        . runLogWithBridge bridge Debug
        . runRequestInput inputHandler
        . runLLMWithToolsHooked @_ @DMEvent spinnerHooks llmConfig makeDMDispatcherWithPhase
        $ guiGameLoopWithDB conn gameId bridge

      -- Save final state
      GUI.logInfo bridge "[Startup] Game loop ended, saving state..."
      Storage.saveGameState conn gameId finalState

      return ()

-- | Run character creation flow via GUI
--
-- Posts PendingCharacterCreation request, waits for result from GUI,
-- updates state with character choices, and adds intro narrative.
runCharacterCreation :: GUIBridge WorldState -> WorldState -> IO WorldState
runCharacterCreation bridge initialState = do
  -- Post character creation request to GUI
  atomically $ writeTVar bridge.gbPendingRequest (Just PendingCharacterCreation)

  GUI.logInfo bridge "[CharacterCreation] Waiting for player to create character..."

  -- Wait for character creation result from GUI
  resultJson <- takeMVar bridge.gbCharacterCreationResult

  -- Clear the pending request
  atomically $ writeTVar bridge.gbPendingRequest Nothing

  -- Parse the JSON result back to CharacterChoices
  case Aeson.fromJSON resultJson of
    Aeson.Success (Just choices) -> do
      GUI.logInfo bridge $ "[CharacterCreation] Character created: " <> choices.ccName
      -- Transition to PhaseScenarioInit with character choices
      let newState = initialState { phase = PhaseScenarioInit choices }

      -- Add intro narrative with character info
      addNarrative bridge "DOSKVOL. Industrial sprawl. Eternal night."
      addNarrative bridge "The ghosts press against the lightning barriers."
      addNarrative bridge "The gangs carve up the districts."
      addNarrative bridge $ "You are " <> choices.ccName <> ", a " <> archetypeLabel choices.ccArchetype <> "."
      addNarrative bridge "Your story begins..."

      return newState

    Aeson.Success Nothing -> do
      -- Cancelled - return unchanged state
      GUI.logInfo bridge "[CharacterCreation] Cancelled by user"
      return initialState

    Aeson.Error err -> do
      GUI.logInfo bridge $ "[CharacterCreation] Parse error: " <> T.pack err
      return initialState
  where
    archetypeLabel :: CC.Archetype -> Text
    archetypeLabel = \case
      CC.Cutter -> "Cutter"
      CC.Hound -> "Hound"
      CC.Leech -> "Leech"
      CC.Lurk -> "Lurk"
      CC.Slide -> "Slide"
      CC.Spider -> "Spider"
      CC.Whisper -> "Whisper"

-- | Extract narrative content from conversation history
-- Includes both player actions and DM responses for context
extractNarrativeTexts :: [Message] -> [Text]
extractNarrativeTexts msgs = concatMap extractFromMessage msgs
  where
    extractFromMessage :: Message -> [Text]
    extractFromMessage msg = case msg.role of
      User -> extractUserText msg.content
      Assistant -> extractAssistantText msg.content

    -- Extract player action text from user messages
    extractUserText :: [ContentBlock] -> [Text]
    extractUserText blocks =
      [ "> " <> txt  -- Prefix with > to show it's player action
      | TextBlock txt <- blocks
      , not (T.null $ T.strip txt)
      ]

    -- Extract DM response from assistant messages
    -- Can be in TextBlock or JsonBlock (structured output)
    extractAssistantText :: [ContentBlock] -> [Text]
    extractAssistantText blocks = concatMap extractBlock blocks

    extractBlock :: ContentBlock -> [Text]
    extractBlock (TextBlock txt)
      | T.null (T.strip txt) = []
      -- Try to parse as JSON and replay events through formatEvent
      | Just val <- Aeson.decode (LBS.fromStrict $ TE.encodeUtf8 txt)
      = replayEventsFromJson val
      -- Not JSON - return raw text
      | otherwise = [txt]
    extractBlock (JsonBlock val) = replayEventsFromJson val
    extractBlock _ = []  -- Ignore tool use, thinking, etc.

    -- Reconstruct DMEvents from JSON and format them the same way as live play
    replayEventsFromJson :: Value -> [Text]
    replayEventsFromJson val =
      let events = reconstructEvents val
          formatted = catMaybes $ map formatEvent events
          -- Also include raw narrative if present
          narrative = maybeToList $ extractNarration val
      in narrative ++ formatted

    -- Reconstruct DMEvent values from stored JSON
    reconstructEvents :: Value -> [DMEvent]
    reconstructEvents (Object obj) =
      let stressDelta = getIntField "stressDelta" obj
          heatDelta = getIntField "heatDelta" obj
          coinDelta = getIntField "coinDelta" obj
      in catMaybes
           [ if stressDelta /= 0
             then Just $ StressChanged 0 stressDelta ""
             else Nothing
           , if heatDelta /= 0
             then Just $ HeatChanged 0 heatDelta ""
             else Nothing
           , if coinDelta /= 0
             then Just $ CoinChanged 0 coinDelta ""
             else Nothing
           ]
    reconstructEvents _ = []

    getIntField :: Text -> KM.KeyMap Value -> Int
    getIntField key obj = case KM.lookup (Key.fromText key) obj of
      Just (Aeson.Number n) -> round n
      _ -> 0

    -- Extract just the narrative text field
    extractNarration :: Value -> Maybe Text
    extractNarration (Object obj) =
      -- Try regular turn output field first
      case KM.lookup (Key.fromText "narration") obj of
        Just (String t) | not (T.null $ T.strip t) -> Just t
        _ -> -- Try scenario init fields
          case KM.lookup (Key.fromText "siSceneNarration") obj of
            Just (String t) | not (T.null $ T.strip t) -> Just t
            _ -> Nothing
    extractNarration _ = Nothing

    maybeToList :: Maybe a -> [a]
    maybeToList Nothing = []
    maybeToList (Just x) = [x]

-- | Game loop for GUI with DB persistence - saves state after each turn
guiGameLoopWithDB
  :: Connection
  -> Storage.GameId
  -> GUIBridge WorldState
  -> Eff (GameEffects WorldState DMEvent) ()
guiGameLoopWithDB conn gameId bridge = loop
  where
    loop :: Eff (GameEffects WorldState DMEvent) ()
    loop = do
      -- Check if we have an active scene
      state <- get @WorldState

      -- Sync current state to bridge (so GUI shows latest)
      liftIO $ updateState bridge (const state)

      -- Check if session was ended by player
      case state.phase of
        PhaseSessionEnded -> do
          logInfo "[Loop] Session ended by player, exiting loop"
          return ()
        _ -> continueLoop state

    continueLoop :: WorldState -> Eff (GameEffects WorldState DMEvent) ()
    continueLoop state = case currentScene state of
        Nothing -> do
          -- No scene - check if this is a fresh game (never had scenario init)
          -- Use chat history to detect: empty = never played, non-empty = scene ended
          history <- getHistory
          let isFreshGame = null history

          -- Initialize dice pool once for fresh games
          when isFreshGame $ do
            startingDice <- rollStartingDice
            modify $ \s -> s { dicePool = DicePool startingDice }
            logInfo $ "[Loop] Fresh game - initialized dice pool: " <> T.pack (show startingDice)

          -- Phase-based dispatch
          case state.phase of
            PhaseScenarioInit choices -> do
              -- Fresh game with character - generate opening scenario
              logInfo "[Loop] Generating opening scenario from character choices..."

              let prompt = scenarioInitPrompt choices
              logInfo $ "[Loop] Scenario prompt length: " <> T.pack (show $ T.length prompt)

              -- Call LLM for scenario init (spinner handled by interpreter hooks)
              result <- runTurn @ScenarioInit prompt "Generate the opening scenario." scenarioInitSchemaJSON []

              case result of
                TurnCompleted (TurnParsed tr) -> do
                  let scenario = tr.trOutput
                  logInfo "[Loop] Scenario generated successfully"

                  -- Apply scenario to state (dice already initialized above)
                  let newPlayer = state.player
                        { stress = scenario.siStartingStress
                        , coin = scenario.siStartingCoin
                        , heat = scenario.siStartingHeat
                        , wanted = scenario.siStartingWanted
                        , trauma = maybe [] (\t -> [Trauma t]) scenario.siStartingTrauma
                        }

                      -- Convert ClockInit to Clock
                      clocksFromInit = HM.fromList
                        [ (ClockId (T.toLower $ T.replace " " "_" ci.ciName), initToClock ci)
                        | ci <- scenario.siStartingClocks
                        ]

                      -- Convert LocationInit to Location
                      locationsFromInit = HM.fromList
                        [ (LocationId li.liId, initToLocation li)
                        | li <- scenario.siLocations
                        ]

                      -- Convert FactionInit to Faction
                      factionsFromInit = HM.fromList
                        [ (FactionId fi.fiId, initToFaction fi)
                        | fi <- scenario.siFactions
                        ]

                      -- Convert NpcInit to Npc
                      npcsFromInit = HM.fromList
                        [ (NpcId ni.niId, initToNpc ni)
                        | ni <- scenario.siNpcs
                        ]

                      -- NPCs present in opening scene
                      scenePresentNpcs = map NpcId scenario.siScenePresentNpcs

                      introScene = ActiveScene
                        { sceneLocation = LocationId scenario.siSceneLocation
                        , scenePresent = scenePresentNpcs
                        , sceneStakes = Stakes scenario.siSceneStakes
                        , sceneBeats = Seq.empty  -- Narration flows through NarrativeAdded events
                        , sceneStyle = defaultSceneStyle
                        }

                  modify $ \s -> s
                    { phase = PhasePlaying introScene defaultMood
                    , player = newPlayer
                    , clocks = clocksFromInit
                    , locations = locationsFromInit
                    , factions = factionsFromInit
                    , npcs = npcsFromInit
                    , DM.State.suggestedActions = scenario.siSuggestedActions
                    }

                  -- Add opening thread (consolidated 3-tweet narrative)
                  emit (NarrativeAdded scenario.siOpeningThread)

                  -- Set suggested actions in bridge for UI
                  liftIO $ setSuggestedActions bridge scenario.siSuggestedActions

                TurnCompleted (TurnParseFailed{..}) -> do
                  logWarn $ "Failed to parse scenario: " <> T.pack tpfError
                  emit (NarrativeAdded "*The city's threads tangle. Something went wrong generating the scenario. Try again.*")
                  -- Stay in PhaseScenarioInit so user can retry

                TurnBroken reason -> do
                  logWarn $ "LLM failed during scenario generation: " <> reason
                  emit (NarrativeAdded "*The oracle falls silent. Something went wrong. Try again.*")
                  -- Stay in PhaseScenarioInit so user can retry

              loop

            PhaseCharacterCreation -> do
              -- Character creation not yet done - user needs to complete it
              logWarn "Game loop called in PhaseCharacterCreation - waiting for character setup"
              -- Don't crash, just wait for character creation to complete
              loop

            PhaseBetweenScenes _ctx -> do
              -- Handle between-scenes decision (user chose from menu)
              logInfo "[Loop] In BetweenScenes phase, handling transition..."
              handleBetweenScenes bridge
              -- Save state after BetweenScenes
              updatedState <- get @WorldState
              liftIO $ updateState bridge (const updatedState)
              liftIO $ Storage.saveGameState conn gameId updatedState
              loop

            PhasePlaying _ _ -> do
              -- Scene active but no scene in phase? This shouldn't happen
              -- But handle gracefully by prompting BetweenScenes
              logInfo "[Loop] Scene ended, entering BetweenScenes..."
              handleBetweenScenes bridge
              updatedState <- get @WorldState
              liftIO $ updateState bridge (const updatedState)
              liftIO $ Storage.saveGameState conn gameId updatedState
              loop

            PhaseSessionEnded -> do
              -- Game is over
              logInfo "[Loop] Session ended."
              return ()

        Just _ -> do
          -- Wait for player input via GUI (spinner is OFF during input)
          logInfo "[Loop] Waiting for player input..."
          playerInput <- waitForPlayerInput
          logInfo $ "[Loop] Got input: " <> T.take 50 playerInput.piActionText

          -- Check for quit
          if T.toLower (T.strip playerInput.piActionText) `elem` ["quit", "exit", "q"]
            then return ()
            else do
              -- Show player's action in narrative (before DM response)
              emit (NarrativeAdded $ "> " <> playerInput.piActionText)

              logInfo "[Loop] Starting dmTurn..."

              -- Run a turn (spinner handled by LLM interpreter hooks)
              response <- dmTurn playerInput

              logInfo $ "[Loop] dmTurn completed: " <> T.take 100 response.responseText

              -- Push narrative via Emit effect (handled by event handler)
              emit (NarrativeAdded response.responseText)

              -- Store suggested actions in WorldState (for persistence) and bridge (for UI)
              let newSuggestions = response.responseSuggestedActions
              modify @WorldState $ \s -> s { DM.State.suggestedActions = newSuggestions }
              liftIO $ setSuggestedActions bridge newSuggestions

              -- Sync updated state to bridge and save to DB
              updatedState <- get @WorldState
              liftIO $ updateState bridge (const updatedState)
              liftIO $ Storage.saveGameState conn gameId updatedState

              -- Continue loop
              loop

-- ══════════════════════════════════════════════════════════════
-- BETWEENSCENES PHASE
-- ══════════════════════════════════════════════════════════════

-- | Handle the BetweenScenes phase
--
-- This is the pause between scenes where:
-- 1. Threat clocks tick (time passes)
-- 2. Player sees clock status
-- 3. Player chooses what to do next (lay low, recover, work goal, new scene, quit)
--
-- Uses the RequestChoice effect for typed choice handling - no manual index handling.
handleBetweenScenes
  :: GUIBridge WorldState
  -> Eff (GameEffects WorldState DMEvent) ()
handleBetweenScenes bridge = do
  logInfo "[BetweenScenes] Entering between-scenes phase..."

  -- 0. Check for heat-triggered entanglement FIRST (before clocks tick)
  initialState <- get @WorldState
  when (initialState.player.heat >= 10) $ do
    handleHeatEscalation bridge

  -- 1. Tick all threat clocks (time passes between scenes)
  tickThreatClocks

  -- 2. Check for completed clocks and trigger consequences
  checkClockConsequences

  state <- get @WorldState

  -- 3. Generate transition narration via LLM (spinner handled by interpreter hooks)
  transitionText <- generateTransitionNarration state

  -- 4. Build clock summaries for display
  let clockSummaries = buildClockSummaries state.clocks

  -- 5. Build available options based on current state
  let options = buildAvailableOptions state

  -- 6. Update phase with new context (GUI reads from phase)
  let ctx = BetweenScenesContext
        { bscClocks = clockSummaries
        , bscTransitionNarration = transitionText
        }
  modify @WorldState $ \s -> s { phase = PhaseBetweenScenes ctx }

  -- Sync state so GUI sees the context
  currentState <- get @WorldState
  liftIO $ updateState bridge (const currentState)

  logInfo $ "[BetweenScenes] Requesting choice with " <> T.pack (show $ length options) <> " options"

  -- 7. Use typed effect for choice - index handling is in Tidepool
  let labeledOptions = [(optionLabel opt, opt) | opt <- options]
  chosenOption <- requestChoice "What do you do?" labeledOptions

  -- 8. Show player's choice in narrative
  emit (NarrativeAdded $ "> " <> optionLabel chosenOption)

  logInfo $ "[BetweenScenes] Player chose: " <> T.pack (show chosenOption)

  -- 9. Apply the chosen option
  applyBetweenScenesChoice chosenOption

-- | Tick all threat clocks by 1 (time passes)
tickThreatClocks
  :: State WorldState :> es
  => Eff es ()
tickThreatClocks = modify @WorldState $ \s ->
  s { clocks = HM.map tickIfThreat s.clocks }
  where
    tickIfThreat clock = case clock.clockType of
      ThreatClock -> clock { clockFilled = min clock.clockSegments (clock.clockFilled + 1) }
      GoalClock -> clock  -- Goal clocks don't tick automatically

-- | Generate brief transition narration via LLM
generateTransitionNarration
  :: ( LLM :> es
     , Log :> es
     )
  => WorldState
  -> Eff es Text
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

  -- Simple text generation (no structured output, no tools)
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
      logWarn $ "Raw JSON: " <> TE.decodeUtf8 (LBS.toStrict $ Aeson.encode tpfRawJson)
      pure "Time passes in the eternal dark of Doskvol..."
    TurnBroken reason -> do
      logWarn $ "Transition generation failed: " <> reason
      pure "The city waits..."

-- | Simple wrapper for transition output
data TransitionOutput = TransitionOutput { toText :: Text }
  deriving (Show, Eq)

instance Aeson.FromJSON TransitionOutput where
  parseJSON = Aeson.withObject "TransitionOutput" $ \o ->
    TransitionOutput <$> o Aeson..: "text"

-- ══════════════════════════════════════════════════════════════
-- HEAT ESCALATION
-- ══════════════════════════════════════════════════════════════

-- | Handle heat-triggered entanglement when heat reaches max (10)
-- Called at the START of BetweenScenes, before normal options.
-- Flow:
--   1. LLM generates entanglement (type, narration, escape options)
--   2. Player chooses an escape option
--   3. Apply the option's costs
--   4. Wanted++, heat resets to 0
--   5. Continue to normal BetweenScenes
handleHeatEscalation
  :: GUIBridge WorldState
  -> Eff (GameEffects WorldState DMEvent) ()
handleHeatEscalation _bridge = do
  logInfo "[Escalation] Heat at max, triggering entanglement..."
  state <- get @WorldState

  -- Build context-aware prompt
  let prompt = buildEntanglementPrompt state

  -- Generate entanglement via LLM
  result <- runTurn @EntanglementOutput prompt "Generate entanglement." entanglementSchemaJSON []

  case result of
    TurnCompleted (TurnParsed tr) -> do
      let entanglement = tr.trOutput

      -- Emit the narration (dramatic moment!)
      emit (NarrativeAdded "")
      emit (NarrativeAdded "---")
      emit (NarrativeAdded $ "**ENTANGLEMENT: " <> T.toUpper entanglement.eoType <> "**")
      emit (NarrativeAdded entanglement.eoNarration)

      -- Build choices from options (include cost in label)
      let choices = [(formatEscapeOption eo, eo) | eo <- entanglement.eoOptions]

      -- Get player choice
      chosen <- requestChoice "How do you handle this?" choices

      -- Show player's choice
      emit (NarrativeAdded $ "> " <> chosen.eoLabel)

      -- Apply the chosen option's costs
      applyEscapeOption chosen

      -- Emit resolution
      emit (NarrativeAdded chosen.eoResolution)

      -- Escalate: wanted++, heat reset
      stateBefore <- get @WorldState
      modify @WorldState $ \s -> s
        { player = s.player
            { wanted = min 4 (s.player.wanted + 1)
            , heat = 0
            }
        }
      stateAfter <- get @WorldState

      -- Emit state change events
      emit $ WantedChanged
        { weFrom = stateBefore.player.wanted
        , weTo = stateAfter.player.wanted
        , weReason = "Heat escalation - " <> entanglement.eoType
        }
      emit $ HeatChanged
        { heFrom = stateBefore.player.heat
        , heTo = 0
        , heReason = "Entanglement resolved"
        }

      emit (NarrativeAdded "---")
      logInfo "[Escalation] Entanglement resolved, continuing to BetweenScenes..."

    TurnCompleted (TurnParseFailed{..}) -> do
      logWarn $ "Entanglement generation failed: " <> T.pack tpfError
      -- Fallback: just reset heat and bump wanted without narration
      applyEscalationFallback

    TurnBroken reason -> do
      logWarn $ "Entanglement LLM broke: " <> reason
      applyEscalationFallback

-- | Fallback when entanglement generation fails
applyEscalationFallback :: Eff (GameEffects WorldState DMEvent) ()
applyEscalationFallback = do
  stateBefore <- get @WorldState
  modify @WorldState $ \s -> s
    { player = s.player
        { wanted = min 4 (s.player.wanted + 1)
        , heat = 0
        }
    }
  stateAfter <- get @WorldState
  emit (NarrativeAdded "The heat catches up. You're more wanted now.")
  emit $ WantedChanged
    { weFrom = stateBefore.player.wanted
    , weTo = stateAfter.player.wanted
    , weReason = "Heat escalation"
    }
  emit $ HeatChanged
    { heFrom = stateBefore.player.heat
    , heTo = 0
    , heReason = "Heat reset"
    }

-- | Build the prompt for entanglement generation
buildEntanglementPrompt :: WorldState -> Text
buildEntanglementPrompt state = T.unlines
  [ "You are a noir narrator for a Blades in the Dark game."
  , "The player's heat has reached maximum. The Bluecoats are closing in."
  , "Generate an ENTANGLEMENT - a consequence that catches up with them."
  , ""
  , "Current state:"
  , "- Heat: " <> T.pack (show state.player.heat) <> "/10 (MAXIMUM)"
  , "- Wanted: " <> T.pack (show state.player.wanted) <> "/4"
  , "- Stress: " <> T.pack (show state.player.stress) <> "/9"
  , "- Coin: " <> T.pack (show state.player.coin)
  , ""
  , "Entanglement types (pick one that fits):"
  , "- raid: Bluecoats search their hideout/stash"
  , "- shakedown: Corrupt official demands payment"
  , "- warrant: Now officially wanted, can't move freely"
  , "- informant: Someone sold them out"
  , "- reprisal: Someone they wronged strikes back"
  , ""
  , "Provide 3 escape options with escalating costs:"
  , "- One cheap option (1-2 coin or 1 stress)"
  , "- One moderate option (2-3 coin or 2 stress)"
  , "- One desperate option (high stress, 3-4, but no coin)"
  , ""
  , "Keep narration tight and atmospheric (2-3 sentences)."
  , "Each resolution should be 1 sentence."
  ]

-- | Apply the costs from the chosen escape option
applyEscapeOption :: EscapeOption -> Eff (GameEffects WorldState DMEvent) ()
applyEscapeOption opt = do
  let amount = opt.eoCostAmount
  case opt.eoCostType of
    "coin" -> modify @WorldState $ \s ->
      s { player = s.player { coin = max 0 (s.player.coin - amount) } }
    "stress" -> modify @WorldState $ \s ->
      s { player = s.player { stress = min 9 (s.player.stress + amount) } }
    "heat" -> modify @WorldState $ \s ->
      s { player = s.player { heat = min 10 (s.player.heat + amount) } }
    _ -> pure ()  -- Unknown cost type, ignore

-- | Format escape option for display (include cost in label)
formatEscapeOption :: EscapeOption -> Text
formatEscapeOption opt =
  opt.eoLabel <> " (" <> costText <> ")"
  where
    costText = case opt.eoCostType of
      "coin" -> T.pack (show opt.eoCostAmount) <> " coin"
      "stress" -> "+" <> T.pack (show opt.eoCostAmount) <> " stress"
      "heat" -> "+" <> T.pack (show opt.eoCostAmount) <> " heat"
      other -> other <> " " <> T.pack (show opt.eoCostAmount)

-- | JSON Schema for entanglement generation
entanglementSchemaJSON :: Value
entanglementSchemaJSON = Object $ KM.fromList
  [ ("type", String "object")
  , ("additionalProperties", Bool False)
  , ("properties", Object $ KM.fromList
      [ ("eoType", Object $ KM.fromList
          [ ("type", String "string")
          , ("enum", Array $ V.fromList
              [String "raid", String "shakedown", String "warrant", String "informant", String "reprisal"])
          , ("description", String "Type of entanglement")
          ])
      , ("eoNarration", Object $ KM.fromList
          [ ("type", String "string")
          , ("description", String "2-3 sentences describing what's happening, tight and atmospheric")
          ])
      , ("eoOptions", Object $ KM.fromList
          [ ("type", String "array")
          , ("items", escapeOptionSchemaJSON)
          , ("minItems", Number 3)
          , ("maxItems", Number 3)
          , ("description", String "3 escape options with escalating costs")
          ])
      ])
  , ("required", Array $ V.fromList [String "eoType", String "eoNarration", String "eoOptions"])
  ]

-- | JSON Schema for escape option
escapeOptionSchemaJSON :: Value
escapeOptionSchemaJSON = Object $ KM.fromList
  [ ("type", String "object")
  , ("additionalProperties", Bool False)
  , ("properties", Object $ KM.fromList
      [ ("eoLabel", Object $ KM.fromList
          [ ("type", String "string")
          , ("description", String "Short action label, e.g. 'Pay the bribe', 'Fight your way out'")
          ])
      , ("eoCostType", Object $ KM.fromList
          [ ("type", String "string")
          , ("enum", Array $ V.fromList [String "coin", String "stress", String "heat"])
          , ("description", String "What resource this costs")
          ])
      , ("eoCostAmount", Object $ KM.fromList
          [ ("type", String "integer")
          , ("description", String "Amount of the resource consumed (1-4)")
          ])
      , ("eoResolution", Object $ KM.fromList
          [ ("type", String "string")
          , ("description", String "1 sentence describing what happens if they choose this")
          ])
      ])
  , ("required", Array $ V.fromList [String "eoLabel", String "eoCostType", String "eoCostAmount", String "eoResolution"])
  ]

-- | Build clock summaries for BetweenScenes display
buildClockSummaries :: HM.HashMap ClockId Clock -> [ClockSummary]
buildClockSummaries clocks =
  [ ClockSummary
      { csName = clock.clockName
      , csFilled = clock.clockFilled
      , csSegments = clock.clockSegments
      , csIsThreat = clock.clockType == ThreatClock
      }
  | clock <- HM.elems clocks
  , clock.clockVisible
  ]

-- | Build available options based on current state
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

-- | Apply the player's choice from BetweenScenes
applyBetweenScenesChoice
  :: BetweenScenesOption
  -> Eff (GameEffects WorldState DMEvent) ()
applyBetweenScenesChoice choice = case choice of

  BSLayLow -> do
    -- Reduce heat by 1, but threat clocks already ticked
    logInfo "[BetweenScenes] Laying low - reducing heat"
    modify @WorldState $ \s -> s
      { player = s.player { heat = max 0 (s.player.heat - 1) }
      }
    emit (NarrativeAdded "You lay low. The heat fades, but time passes...")
    -- Create new scene
    createNewScene

  BSRecover -> do
    -- Spend 1 coin to reduce 1 stress (simple for now)
    logInfo "[BetweenScenes] Recovering - spending coin for stress"
    modify @WorldState $ \s -> s
      { player = s.player
          { coin = max 0 (s.player.coin - 1)
          , stress = max 0 (s.player.stress - 1)
          }
      }
    emit (NarrativeAdded "You spend coin on small comforts. The stress eases...")
    createNewScene

  BSWorkGoal goalName -> do
    -- Tick the goal clock (player is working toward it)
    logInfo $ "[BetweenScenes] Working goal: " <> goalName
    modify @WorldState $ \s -> s
      { clocks = HM.map (tickGoal goalName) s.clocks
      }
    -- Check if goal clock completed
    checkClockConsequences
    emit (NarrativeAdded $ "You work toward your goal: " <> goalName <> "...")
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
    emit (NarrativeAdded "You fade into the city's eternal night. Until next time...")
    -- Set phase to SessionEnded - the loop will check for this and exit
    modify @WorldState $ \s -> s { phase = PhaseSessionEnded }

-- | Create a new scene after BetweenScenes
createNewScene :: Eff (GameEffects WorldState DMEvent) ()
createNewScene = do
  logInfo "[BetweenScenes] Creating continuation scene..."

  -- Set phase back to Playing and create a basic scene
  -- The LLM will flesh out the scene on the next turn
  let contScene = ActiveScene
        { sceneLocation = LocationId "doskvol"
        , scenePresent = []
        , sceneStakes = Stakes "What happens next?"
        , sceneBeats = Seq.empty
        , sceneStyle = defaultSceneStyle
        }

  let newMood = MoodScene (Encounter "continuing" UrgencyLow True)
  modify @WorldState $ \s -> s
    { phase = PhasePlaying contScene newMood
    }

  emit (NarrativeAdded "---")
  emit (NarrativeAdded "The city awaits your next move.")

-- | Roll starting dice pool (5 dice, values 1-6)
rollStartingDice :: Random :> es => Eff es [Int]
rollStartingDice = replicateM 5 (randomInt 1 6)

-- | Convert ClockInit from scenario generation to a Clock
initToClock :: ClockInit -> Clock
initToClock ci = Clock
  { clockName = ci.ciName
  , clockSegments = ci.ciSegments
  , clockFilled = ci.ciFilled
  , clockVisible = True  -- All clocks visible per design
  , clockType = ci.ciType
  , clockConsequence = ci.ciConsequence
  }

-- | Convert LocationInit from scenario generation to Location
initToLocation :: LocationInit -> Location
initToLocation li = Location
  { locationName = li.liName
  , locationDescription = li.liDescription
  , locationControlledBy = FactionId <$> li.liControlledBy
  , locationFeatures = li.liFeatures
  }

-- | Parse attitude string to Attitude type
parseAttitude :: Text -> Attitude
parseAttitude t = case T.toLower t of
  "hostile"   -> Hostile
  "wary"      -> Wary
  "neutral"   -> Neutral
  "favorable" -> Favorable
  "allied"    -> Allied
  _           -> Neutral  -- Default

-- | Convert FactionInit from scenario generation to Faction
initToFaction :: FactionInit -> Faction
initToFaction fi = Faction
  { factionName = fi.fiName
  , factionAttitude = parseAttitude fi.fiAttitude
  , factionGoals = [Goal (GoalId "main") fi.fiGoalDescription Pursuing]
  , factionResources = ResourcePool
      { gold = 5
      , soldiers = 5
      , influence = 5
      , specialResources = [(fi.fiResources, 1)]  -- Store description as a special resource
      }
  , factionSecrets = []
  , factionKnownFacts = []
  }

-- | Parse disposition string to Disposition type
parseDisposition :: Text -> Disposition
parseDisposition t = case T.toLower t of
  "disphostile" -> DispHostile
  "hostile"     -> DispHostile
  "suspicious"  -> Suspicious
  "dispneutral" -> DispNeutral
  "neutral"     -> DispNeutral
  "friendly"    -> Friendly
  "loyal"       -> Loyal
  _             -> DispNeutral  -- Default

-- | Convert NpcInit from scenario generation to Npc
initToNpc :: NpcInit -> Npc
initToNpc ni = Npc
  { npcName = ni.niName
  , npcFaction = FactionId <$> ni.niFaction
  , npcDisposition = parseDisposition ni.niDisposition
  , npcWants = [Want { wantDescription = ni.niWant, wantUrgency = Medium }]
  , npcFears = []
  , npcKnows = []
  , npcLocation = LocationId <$> ni.niLocation
  , npcVoiceNotes = ni.niVoiceNotes
  }

-- | JSON schema for scenario initialization response
scenarioInitSchemaJSON :: Value
scenarioInitSchemaJSON = Object $ KM.fromList
  [ ("type", String "object")
  , ("additionalProperties", Bool False)
  , ("properties", Object $ KM.fromList
      [ ("siFateNarration", Object $ KM.fromList
          [ ("type", String "string")
          , ("description", String "Fate's interpretation of the tarot spread - noir oracle voice, 1-2 sentences per card, names each clock")
          ])
      , ("siStartingClocks", Object $ KM.fromList
          [ ("type", String "array")
          , ("description", String "3 clocks seeded from tarot: 2 threats (past/present), 1 goal (future)")
          , ("items", Object $ KM.fromList
              [ ("type", String "object")
              , ("additionalProperties", Bool False)
              , ("properties", Object $ KM.fromList
                  [ ("ciName", Object $ KM.fromList [("type", String "string")])
                  , ("ciSegments", Object $ KM.fromList [("type", String "integer")])
                  , ("ciFilled", Object $ KM.fromList [("type", String "integer")])
                  , ("ciFromCard", Object $ KM.fromList
                      [ ("type", String "string")
                      , ("enum", Array $ V.fromList [String "TarotPast", String "TarotPresent", String "TarotFuture"])
                      ])
                  , ("ciType", Object $ KM.fromList
                      [ ("type", String "string")
                      , ("enum", Array $ V.fromList [String "ThreatClock", String "GoalClock"])
                      ])
                  , ("ciConsequence", Object $ KM.fromList
                      [ ("type", String "string")
                      , ("description", String "2-3 sentences describing what happens when this clock fills. Be specific about stakes and outcomes.")
                      ])
                  ])
              , ("required", Array $ V.fromList
                  [String "ciName", String "ciSegments", String "ciFilled", String "ciFromCard", String "ciType", String "ciConsequence"])
              ])
          ])
      , ("siSceneNarration", Object $ KM.fromList
          [ ("type", String "string")
          , ("description", String "Opening scene narration - tight noir prose, ~100-150 words, establishes character in a moment")
          ])
      , ("siStartingStress", Object $ KM.fromList
          [ ("type", String "integer")
          , ("description", String "Starting stress 0-4")
          ])
      , ("siStartingCoin", Object $ KM.fromList
          [ ("type", String "integer")
          , ("description", String "Starting coin 0-4")
          ])
      , ("siStartingHeat", Object $ KM.fromList
          [ ("type", String "integer")
          , ("description", String "Starting heat 0-2")
          ])
      , ("siStartingWanted", Object $ KM.fromList
          [ ("type", String "integer")
          , ("description", String "Starting wanted 0-1")
          ])
      , ("siStartingTrauma", Object $ KM.fromList
          [ ("type", String "string")
          , ("description", String "Optional starting trauma if past strongly suggests it")
          ])
      , ("siSceneLocation", Object $ KM.fromList
          [ ("type", String "string")
          , ("description", String "Where the opening scene takes place")
          ])
      , ("siSceneStakes", Object $ KM.fromList
          [ ("type", String "string")
          , ("description", String "What's at stake in the opening")
          ])
      , ("siOpeningHook", Object $ KM.fromList
          [ ("type", String "string")
          , ("description", String "The immediate situation demanding response")
          ])
      , ("siSuggestedActions", Object $ KM.fromList
          [ ("type", String "array")
          , ("items", Object $ KM.fromList [("type", String "string")])
          , ("description", String "3-4 suggested actions the player could take in response to the opening hook")
          ])
      , ("siLocations", Object $ KM.fromList
          [ ("type", String "array")
          , ("description", String "2-3 locations relevant to the opening situation")
          , ("items", Object $ KM.fromList
              [ ("type", String "object")
              , ("additionalProperties", Bool False)
              , ("properties", Object $ KM.fromList
                  [ ("liId", Object $ KM.fromList
                      [ ("type", String "string")
                      , ("description", String "snake_case identifier")
                      ])
                  , ("liName", Object $ KM.fromList [("type", String "string")])
                  , ("liDescription", Object $ KM.fromList [("type", String "string")])
                  , ("liControlledBy", Object $ KM.fromList
                      [ ("type", String "string")
                      , ("description", String "Faction ID if controlled, or null")
                      ])
                  , ("liFeatures", Object $ KM.fromList
                      [ ("type", String "array")
                      , ("items", Object $ KM.fromList [("type", String "string")])
                      ])
                  ])
              , ("required", Array $ V.fromList
                  [String "liId", String "liName", String "liDescription", String "liFeatures"])
              ])
          ])
      , ("siFactions", Object $ KM.fromList
          [ ("type", String "array")
          , ("description", String "2-3 factions relevant to the character's situation")
          , ("items", Object $ KM.fromList
              [ ("type", String "object")
              , ("additionalProperties", Bool False)
              , ("properties", Object $ KM.fromList
                  [ ("fiId", Object $ KM.fromList
                      [ ("type", String "string")
                      , ("description", String "snake_case identifier")
                      ])
                  , ("fiName", Object $ KM.fromList [("type", String "string")])
                  , ("fiAttitude", Object $ KM.fromList
                      [ ("type", String "string")
                      , ("enum", Array $ V.fromList [String "Hostile", String "Wary", String "Neutral", String "Favorable", String "Allied"])
                      ])
                  , ("fiGoalDescription", Object $ KM.fromList [("type", String "string")])
                  , ("fiResources", Object $ KM.fromList [("type", String "string")])
                  ])
              , ("required", Array $ V.fromList
                  [String "fiId", String "fiName", String "fiAttitude", String "fiGoalDescription", String "fiResources"])
              ])
          ])
      , ("siNpcs", Object $ KM.fromList
          [ ("type", String "array")
          , ("description", String "2-4 NPCs the character might encounter")
          , ("items", Object $ KM.fromList
              [ ("type", String "object")
              , ("additionalProperties", Bool False)
              , ("properties", Object $ KM.fromList
                  [ ("niId", Object $ KM.fromList
                      [ ("type", String "string")
                      , ("description", String "snake_case identifier")
                      ])
                  , ("niName", Object $ KM.fromList [("type", String "string")])
                  , ("niFaction", Object $ KM.fromList
                      [ ("type", String "string")
                      , ("description", String "Faction ID if affiliated, or null")
                      ])
                  , ("niDisposition", Object $ KM.fromList
                      [ ("type", String "string")
                      , ("enum", Array $ V.fromList [String "DispHostile", String "Suspicious", String "DispNeutral", String "Friendly", String "Loyal"])
                      ])
                  , ("niWant", Object $ KM.fromList [("type", String "string")])
                  , ("niVoiceNotes", Object $ KM.fromList [("type", String "string")])
                  , ("niLocation", Object $ KM.fromList
                      [ ("type", String "string")
                      , ("description", String "Location ID where they usually are, or null")
                      ])
                  ])
              , ("required", Array $ V.fromList
                  [String "niId", String "niName", String "niDisposition", String "niWant", String "niVoiceNotes"])
              ])
          ])
      , ("siScenePresentNpcs", Object $ KM.fromList
          [ ("type", String "array")
          , ("items", Object $ KM.fromList [("type", String "string")])
          , ("description", String "NPC IDs present in the opening scene")
          ])
      ])
  , ("required", Array $ V.fromList
      [ String "siFateNarration"
      , String "siStartingClocks"
      , String "siLocations"
      , String "siFactions"
      , String "siNpcs"
      , String "siSceneNarration"
      , String "siStartingStress"
      , String "siStartingCoin"
      , String "siStartingHeat"
      , String "siStartingWanted"
      , String "siSceneLocation"
      , String "siSceneStakes"
      , String "siOpeningHook"
      , String "siSuggestedActions"
      , String "siScenePresentNpcs"
      ])
  ]

-- ══════════════════════════════════════════════════════════════
-- JSON EXTRACTION HELPERS (for fallback when parsing fails)
-- ══════════════════════════════════════════════════════════════

-- | Extract narration field from raw JSON (fallback when TurnOutput parsing fails)
-- This ensures we never show raw JSON to the user
extractNarrationFromJson :: Value -> Maybe Text
extractNarrationFromJson (Object obj) =
  case KM.lookup (Key.fromText "narration") obj of
    Just (String txt) | not (T.null $ T.strip txt) -> Just txt
    _ -> Nothing
extractNarrationFromJson _ = Nothing

-- | Extract suggestedActions from raw JSON (fallback when parsing fails)
extractSuggestionsFromJson :: Value -> [Text]
extractSuggestionsFromJson (Object obj) =
  case KM.lookup (Key.fromText "suggestedActions") obj of
    Just (Array arr) -> [txt | String txt <- V.toList arr]
    _ -> ["Try something else", "Look around", "Wait"]
extractSuggestionsFromJson _ = ["Try something else", "Look around", "Wait"]
