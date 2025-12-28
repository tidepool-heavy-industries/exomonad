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
import DM.Context (buildDMContext, buildCompressionContext, DMContext(..))
import DM.Output (TurnOutput(..), CompressionOutput(..), applyTurnOutput, applyCompression)
import DM.Templates (renderForMood, renderCompression, turnOutputSchema, compressionOutputSchema)
import DM.Tools (DMEvent(..), dmTools, makeDMDispatcher)
import Tidepool.Effect hiding (ToolResult)
import Tidepool.Template (Schema(..))

import Effectful
import Control.Concurrent.MVar (takeMVar)
import Control.Concurrent.STM (atomically, writeTVar, readTVar)
import Control.Monad (when, replicateM)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import System.IO (hFlush, stdout)
import System.Environment (lookupEnv)
import Database.SQLite.Simple (Connection)
import qualified Tidepool.Storage as Storage

import DM.CharacterCreation (CharacterChoices(..), scenarioInitPrompt, ScenarioInit(..))
import qualified DM.CharacterCreation as CC

import Tidepool.GUI.Core (GUIBridge(..), PendingRequest(..), RequestResponse(..),
                          updateState, addNarrative, setLLMActive, setSuggestedActions)
import qualified Tidepool.GUI.Core as GUI (logInfo)
import Tidepool.GUI.Handler (makeGUIHandler)
import Tidepool.Anthropic.Http (Message(..), ContentBlock(..), Role(..))
import Data.Aeson (Value(..))
import qualified Data.Aeson.KeyMap as KM
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
  startingMood <- gets @WorldState (.mood)

  -- 1. Record player action as scene beat (only on first call, not restarts)
  handlePlayerAction input

  -- 2. Run the turn (may restart on mood transition)
  response <- runMoodAwareTurn input.piActionText

  -- 3. If we STARTED in MoodTrauma, the turn just processed it - return to scene
  -- This ensures trauma gets one full turn to be narrated before transitioning
  case startingMood of
    MoodTrauma _ -> do
      logInfo "Trauma turn completed, returning to scene"
      modify @WorldState $ \s -> s
        { mood = MoodScene (Encounter "aftermath of breakdown" UrgencyLow True) }
    _ -> return ()

  return response

  where
    runMoodAwareTurn userAction = do
      -- Check for pending clock interrupts first
      -- If one forces action, this will restart with action mood
      interrupted <- checkPendingInterrupts
      if interrupted
        then runMoodAwareTurn userAction  -- Restart with forced action mood
        else do
          -- Get current mood and build context
          state <- get
          let mood = state.mood
          let context = buildDMContext state
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

          -- Call LLM with system prompt and user action
          let Schema{schemaJSON = outputSchema} = turnOutputSchema
          outcome <- runTurn @TurnOutput systemPrompt userAction outputSchema dmTools

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
                logWarn $ "Tools invoked: " <> T.pack (show $ map (\ti -> ti.tiName) tpfToolsInvoked)
                logWarn $ "Narrative was: " <> (if T.null tpfNarrative then "(empty)" else T.take 100 tpfNarrative)

                -- Use fallback response with whatever narrative we got
                let fallbackNarration = if T.null tpfNarrative
                      then "*The world waits for your next move.*"
                      else tpfNarrative
                return Response
                  { responseText = fallbackNarration
                  , responseStressDelta = 0
                  , responseCoinDelta = 0
                  , responseHeatDelta = 0
                  , responseSuggestedActions = ["Try something else", "Look around", "Wait"]
                  }

              -- Parsed successfully - apply output and continue
              TurnParsed result -> do
                -- Log LLM result
                logInfo $ "LLM turn complete - tools: " <> T.pack (show $ length result.trToolsInvoked)
                logDebug $ "Narrative: " <> T.take 150 result.trNarrative <> "..."

                -- Capture state BEFORE applying output (for accurate delta display)
                stateBefore <- get @WorldState

                -- Apply structured output to world state
                modify (applyTurnOutput result.trOutput)

                -- Record DM response as scene beat (for history)
                let narration = result.trOutput.narration
                modify $ \s -> case s.scene of
                  Nothing -> s
                  Just activeScene ->
                    let beat = DMNarration narration
                        newBeats = activeScene.sceneBeats Seq.|> beat
                    in s { scene = Just activeScene { sceneBeats = newBeats } }

                -- Check clock consequences
                checkClockConsequences

                -- Check if stress hit max (trauma trigger)
                checkTraumaTrigger stateBefore.player.stress

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

    -- | Check for pending clock interrupts and handle them
    -- Returns True if an interrupt forced a state transition
    checkPendingInterrupts = do
      currState <- get @WorldState
      case currState.pendingInterrupt of
        Just interrupt | interrupt.ciForcesAction -> do
          -- Force transition to Action with the interrupt as context
          logInfo $ "Clock interrupt forces action: " <> interrupt.ciDescription
          modify @WorldState $ \s -> s
            { mood = MoodAction (AvRisky interrupt.ciEventType "react or suffer") Nothing
            , pendingInterrupt = Nothing
            }
          return True  -- Signal that we need to restart
        Just _interrupt -> do
          -- Non-forcing interrupt - was surfaced in context, now clear it
          modify @WorldState $ \s -> s { pendingInterrupt = Nothing }
          return False
        Nothing -> return False

-- NOTE: Dice selection is now handled by the SpendDie tool during LLM turn
-- The tool modifies state directly and returns the outcome to the LLM

handlePlayerAction
  :: State WorldState :> es
  => PlayerInput
  -> Eff es ()
handlePlayerAction input = modify $ \state ->
  case state.scene of
    Nothing -> state  -- No active scene, nothing to do
    Just activeScene ->
      let beat = PlayerAction input.piActionText input.piActionTags
          newBeats = activeScene.sceneBeats Seq.|> beat
      in state { scene = Just activeScene { sceneBeats = newBeats } }

checkClockConsequences
  :: ( State WorldState :> es
     , Emit DMEvent :> es
     )
  => Eff es ()
checkClockConsequences = do
  state <- get
  let allClocks = HM.toList state.clocks
      (completed, remaining) = foldr partitionClock ([], HM.empty) allClocks

  -- Emit events for each completed clock
  mapM_ emitClockComplete completed

  -- Remove completed clocks from state
  put state { clocks = remaining }
  where
    partitionClock (clockId, clock) (done, keep)
      | clock.clockFilled >= clock.clockSegments = ((clockId, clock) : done, keep)
      | otherwise = (done, HM.insert clockId clock keep)

    emitClockComplete (ClockId clockId, clock) =
      emit $ ClockCompleted clockId clock.clockName clock.clockConsequence

-- | Check if stress just hit max (trauma trigger)
-- If player crossed from <9 to 9, transition to trauma mood
checkTraumaTrigger
  :: ( State WorldState :> es
     , Log :> es
     , Emit DMEvent :> es
     )
  => Int  -- Stress BEFORE this turn
  -> Eff es ()
checkTraumaTrigger stressBefore = do
  state <- get @WorldState
  let stressNow = state.player.stress
  when (stressNow >= 9 && stressBefore < 9) $ do
    logWarn "TRAUMA TRIGGERED: Stress hit maximum"
    -- Emit trauma event for GUI display
    emit $ TraumaTriggered
      { ttTrauma = Trauma "pending"  -- LLM will determine actual trauma
      , ttTrigger = "The pressure finally became too much."
      , ttBreakingPoint = "Something breaks inside you..."
      }
    -- Transition to trauma mood - next turn will use trauma template
    -- The LLM will narrate the breaking point and assign a trauma
    let traumaVariant = Breaking
          { tvWhatBroke = "stress overflow"  -- LLM will elaborate
          , tvTraumaType = Trauma "pending"  -- LLM will determine actual trauma
          , tvTrigger = "accumulated pressure"
          , tvAdrenaline = False  -- Could be True if in combat
          }
    modify @WorldState $ \s -> s { mood = MoodTrauma traumaVariant }

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
  case (before.mood, after.mood) of
    (_, MoodBargain bv) | not (isBargainMood before.mood) ->
      emit $ BargainOffered
        { boContext = bv.bvWhatDrained
        , boCanRetreat = bv.bvCanRetreat
        }
    _ -> pure ()
  where
    isBargainMood (MoodBargain _) = True
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
  case state.scene of
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
              -- Continue without compressing

            TurnParsed result -> do
              -- Apply compression output to world state
              modify (applyCompression result.trOutput)

              -- Clear scene beats (keep scene active with empty beats)
              modify $ \s -> case s.scene of
                Nothing -> s
                Just scene -> s { scene = Just scene { sceneBeats = Seq.empty } }

              -- Emit compression event
              emit $ SceneCompressed result.trOutput.summary
  where
    compressionThreshold = 20


-- ══════════════════════════════════════════════════════════════
-- SCENE MANAGEMENT
-- ══════════════════════════════════════════════════════════════

-- | Start a new scene at a location
startScene
  :: State WorldState :> es
  => LocationId
  -> [NpcId]
  -> Stakes
  -> Eff es ()
startScene locationId npcsPresent stakes = modify $ \state ->
  state { scene = Just ActiveScene
    { sceneLocation = locationId
    , scenePresent = npcsPresent
    , sceneStakes = stakes
    , sceneBeats = Seq.empty
    }
  }

-- | End the current scene, clearing it from state
-- Note: Call compressIfNeeded before this to preserve scene summary
endCurrentScene
  :: State WorldState :> es
  => Eff es ()
endCurrentScene = modify $ \state -> state { scene = Nothing }

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
            , llmThinkingBudget = Just 1024  -- Enable extended thinking
            }

      -- Set up terminal input handler
      let inputHandler = InputHandler
            { ihChoice = terminalChoice
            , ihText = terminalText
            , ihDice = terminalDice
            }

      -- Start without a scene - first turn establishes character and situation
      let stateWithScene = initialState
            { scene = Just ActiveScene
                { sceneLocation = LocationId "intro"
                , scenePresent = []
                , sceneStakes = Stakes "Establish who you are in Doskvol"
                , sceneBeats = Seq.empty
                }
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
        . runLLMWithTools @_ @DMEvent llmConfig makeDMDispatcher
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
            , llmThinkingBudget = Just 1024
            }

      let inputHandler = InputHandler
            { ihChoice = terminalChoice
            , ihText = terminalText
            , ihDice = terminalDice
            }

      -- Preserve loaded scene, or create intro scene if none exists
      let stateWithScene = case initialState.scene of
            Just _ -> initialState  -- Keep the loaded scene
            Nothing -> initialState
              { scene = Just ActiveScene
                  { sceneLocation = LocationId "intro"
                  , scenePresent = []
                  , sceneStakes = Stakes "Establish who you are in Doskvol"
                  , sceneBeats = Seq.empty
                  }
              }

      TIO.putStrLn "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
      case initialState.scene of
        Just _ -> TIO.putStrLn "Resuming your story in Doskvol..."
        Nothing -> do
          TIO.putStrLn "DOSKVOL. Industrial sprawl. Eternal night."
          TIO.putStrLn "The ghosts press against the lightning barriers."
          TIO.putStrLn "The gangs carve up the districts."
          TIO.putStrLn "You're about to step into the streets."
      TIO.putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
      TIO.putStrLn "\n[Type 'quit' to exit]\n"
      case initialState.scene of
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
        . runLLMWithTools @_ @DMEvent llmConfig makeDMDispatcher
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
      case state.scene of
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
              moodLabel = case currentState.mood of
                MoodScene _     -> "SCENE"
                MoodAction _ _  -> "ACTION"
                MoodAftermath _ -> "AFTERMATH"
                MoodDowntime _  -> "DOWNTIME"
                MoodTrauma _    -> "TRAUMA"
                MoodBargain _   -> "BARGAIN"
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
      case state.scene of
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
              moodLabel = case currentState.mood of
                MoodScene _     -> "SCENE"
                MoodAction _ _  -> "ACTION"
                MoodAftermath _ -> "AFTERMATH"
                MoodDowntime _  -> "DOWNTIME"
                MoodTrauma _    -> "TRAUMA"
                MoodBargain _   -> "BARGAIN"
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
terminalDice :: Text -> [(Int, Int)] -> IO Int
terminalDice prompt diceWithIndices = do
  TIO.putStrLn prompt
  mapM_ printDie (zip [1..] diceWithIndices)
  TIO.putStr "Enter choice (number): "
  hFlush stdout
  input <- TIO.getLine
  case reads (T.unpack input) of
    [(n, "")] | n >= 1 && n <= length diceWithIndices ->
      return $ snd (diceWithIndices !! (n - 1))
    _ -> do
      TIO.putStrLn "Invalid choice, try again."
      terminalDice prompt diceWithIndices
  where
    printDie (n :: Int, (dieValue, _idx)) =
      TIO.putStrLn $ "  " <> T.pack (show n) <> ". Die showing " <> T.pack (show dieValue)

-- ══════════════════════════════════════════════════════════════
-- GUI INTEGRATION
-- ══════════════════════════════════════════════════════════════

-- | Wait for player input via GUI
--
-- Posts a text request to the bridge and blocks until the user submits.
-- Returns the input as a PlayerInput record.
waitForPlayerInput :: GUIBridge WorldState -> IO PlayerInput
waitForPlayerInput bridge = do
  -- Post a text request
  atomically $ writeTVar bridge.gbPendingRequest
    (Just $ PendingText "What do you do?")

  -- Block until response
  response <- takeMVar bridge.gbRequestResponse

  -- Clear request
  atomically $ writeTVar bridge.gbPendingRequest Nothing

  case response of
    TextResponse txt -> pure $ PlayerInput txt []
    ChoiceResponse _ -> waitForPlayerInput bridge  -- Retry on wrong type

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
            , llmThinkingBudget = Just 1024
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

      -- Run the game loop (logs go to GUI debug panel)
      ((), _finalState) <- runEff
        . runRandom
        . runEmit handleEvent
        . runState initialState
        . runChatHistory
        . runLogWithBridge bridge Debug
        . runRequestInput inputHandler
        . runLLMWithTools @_ @DMEvent llmConfig makeDMDispatcher
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

      case state.scene of
        Nothing -> do
          -- No scene - create initial scene
          let introScene = ActiveScene
                { sceneLocation = LocationId "crows_foot"
                , scenePresent = []
                , sceneStakes = Stakes "Establish who you are in Doskvol"
                , sceneBeats = Seq.empty
                }
          modify $ \s -> s { scene = Just introScene }
          liftIO $ addNarrative bridge "Who are you? What brings you to Crow's Foot tonight?"
          loop

        Just _ -> do
          -- Wait for player input via GUI (spinner is OFF during input)
          playerInput <- liftIO $ waitForPlayerInput bridge

          -- Check for quit
          if T.toLower (T.strip playerInput.piActionText) `elem` ["quit", "exit", "q"]
            then return ()
            else do
              -- Show loading indicator WHILE LLM is thinking
              liftIO $ setLLMActive bridge True

              -- Run a turn
              response <- dmTurn playerInput

              -- Hide loading indicator
              liftIO $ setLLMActive bridge False

              -- Push narrative to bridge
              liftIO $ addNarrative bridge response.responseText

              -- Sync updated state to bridge
              updatedState <- get @WorldState
              liftIO $ updateState bridge (const updatedState)

              -- Continue loop
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
            , llmThinkingBudget = Just 1024
            }

      -- Set up GUI input handler
      let inputHandler = makeGUIHandler bridge

      -- Get initial state from bridge
      initialState <- atomically $ readTVar bridge.gbState
      GUI.logInfo bridge $ "[Startup] Initial state loaded, scene: " <> T.pack (show $ fmap (const "...") initialState.scene)

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
      stateAfterCreation <- case initialState.characterChoices of
        Just _ -> do
          -- Character already created, use existing state
          GUI.logInfo bridge "[Startup] Character already exists, skipping creation"
          return initialState
        Nothing -> do
          -- New game - trigger character creation
          GUI.logInfo bridge "[Startup] Fresh game - starting character creation..."
          runCharacterCreation bridge initialState

      -- Update bridge with potentially modified state
      atomically $ writeTVar bridge.gbState stateAfterCreation

      GUI.logInfo bridge "[Startup] Starting game loop..."

      -- Run the game loop with DB persistence
      ((), finalState) <- runEff
        . runRandom
        . runEmit handleEvent
        . runState stateAfterCreation
        . runChatHistoryWithDB conn gameId mCursor
        . runLogWithBridge bridge Debug
        . runRequestInput inputHandler
        . runLLMWithTools @_ @DMEvent llmConfig makeDMDispatcher
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
      -- Update state with character choices
      let newState = initialState { characterChoices = Just choices }

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
      | not (T.null $ T.strip txt) = [txt]
      | otherwise = []
    extractBlock (JsonBlock val) = maybeToList $ extractResponseText val
    extractBlock _ = []  -- Ignore tool use, thinking, etc.

    -- Parse structured output JSON to get responseText field
    extractResponseText :: Value -> Maybe Text
    extractResponseText (Object obj) =
      case KM.lookup "responseText" obj of
        Just (String t) | not (T.null $ T.strip t) -> Just t
        _ -> Nothing
    extractResponseText _ = Nothing

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

      case state.scene of
        Nothing -> do
          -- No scene - check if this is a fresh game or scene just ended
          let isFreshGame = Seq.null state.sessionHistory

          -- Initialize dice pool once for all fresh games
          when isFreshGame $ do
            startingDice <- rollStartingDice
            modify $ \s -> s { dicePool = DicePool startingDice }
            liftIO $ GUI.logInfo bridge $ "[Loop] Fresh game - initialized dice pool: " <> T.pack (show startingDice)

          case (isFreshGame, state.characterChoices) of
            (True, Just choices) -> do
              -- Fresh game with character - generate opening scenario
              liftIO $ GUI.logInfo bridge "[Loop] Generating opening scenario from character choices..."
              liftIO $ setLLMActive bridge True

              let prompt = scenarioInitPrompt choices
              liftIO $ GUI.logInfo bridge $ "[Loop] Scenario prompt length: " <> T.pack (show $ T.length prompt)

              -- Call LLM for scenario init (simple text response, no tools)
              result <- runTurn @ScenarioInit prompt "Generate the opening scenario." scenarioInitSchemaJSON []

              liftIO $ setLLMActive bridge False

              case result of
                TurnCompleted (TurnParsed tr) -> do
                  let scenario = tr.trOutput
                  liftIO $ GUI.logInfo bridge "[Loop] Scenario generated successfully"

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

                      introScene = ActiveScene
                        { sceneLocation = LocationId (T.toLower $ T.replace " " "_" scenario.siSceneLocation)
                        , scenePresent = []
                        , sceneStakes = Stakes scenario.siSceneStakes
                        , sceneBeats = Seq.singleton $ DMNarration scenario.siSceneNarration
                        }

                  modify $ \s -> s
                    { phase = PhasePlaying
                    , player = newPlayer
                    , scene = Just introScene
                    , clocks = clocksFromInit
                    , DM.State.suggestedActions = scenario.siSuggestedActions
                    }

                  -- Add Fate's narration first, then scene narration
                  liftIO $ addNarrative bridge scenario.siFateNarration
                  liftIO $ addNarrative bridge scenario.siSceneNarration
                  liftIO $ addNarrative bridge $ ">> " <> scenario.siOpeningHook

                  -- Set suggested actions in bridge for UI
                  liftIO $ setSuggestedActions bridge scenario.siSuggestedActions

                TurnCompleted (TurnParseFailed{..}) ->
                  error $ "Failed to generate opening scenario: " <> tpfError

                TurnBroken reason ->
                  error $ "LLM failed during scenario generation: " <> T.unpack reason

              loop

            (True, Nothing) ->
              -- Fresh game without character choices is invalid state
              error "Cannot start game without character choices - character creation was cancelled or failed"

            (False, _) -> do
              -- Scene ended (not fresh game) - create continuation scene
              liftIO $ GUI.logInfo bridge "[Loop] Scene ended, creating continuation..."
              createContinuationScene bridge state
              loop

        Just _ -> do
          -- Wait for player input via GUI (spinner is OFF during input)
          liftIO $ GUI.logInfo bridge "[Loop] Waiting for player input..."
          playerInput <- liftIO $ waitForPlayerInput bridge
          liftIO $ GUI.logInfo bridge $ "[Loop] Got input: " <> T.take 50 playerInput.piActionText

          -- Check for quit
          if T.toLower (T.strip playerInput.piActionText) `elem` ["quit", "exit", "q"]
            then return ()
            else do
              -- Show loading indicator WHILE LLM is thinking
              liftIO $ setLLMActive bridge True
              liftIO $ GUI.logInfo bridge "[Loop] Starting dmTurn..."

              -- Run a turn
              response <- dmTurn playerInput

              liftIO $ GUI.logInfo bridge $ "[Loop] dmTurn completed: " <> T.take 100 response.responseText

              -- Hide loading indicator
              liftIO $ setLLMActive bridge False

              -- Push narrative to bridge
              liftIO $ addNarrative bridge response.responseText

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

    -- Helper to create a continuation scene when previous scene ends
    createContinuationScene :: GUIBridge WorldState -> WorldState -> Eff (GameEffects WorldState DMEvent) ()
    createContinuationScene br _st = do
      let contScene = ActiveScene
            { sceneLocation = LocationId "doskvol"
            , scenePresent = []
            , sceneStakes = Stakes "What happens next?"
            , sceneBeats = Seq.empty
            }
      modify $ \s -> s { scene = Just contScene }
      liftIO $ addNarrative br "---"
      liftIO $ addNarrative br "The scene fades. What do you do next?"

-- | Roll starting dice pool (5 dice, values 1-6)
rollStartingDice :: Random :> es => Eff es [Int]
rollStartingDice = replicateM 5 (randomInt 1 6)

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
                  , ("ciConsequenceDesc", Object $ KM.fromList [("type", String "string")])
                  ])
              , ("required", Array $ V.fromList
                  [String "ciName", String "ciSegments", String "ciFilled", String "ciFromCard", String "ciType", String "ciConsequenceDesc"])
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
      ])
  , ("required", Array $ V.fromList
      [ String "siFateNarration"
      , String "siStartingClocks"
      , String "siSceneNarration"
      , String "siStartingStress"
      , String "siStartingCoin"
      , String "siStartingHeat"
      , String "siStartingWanted"
      , String "siSceneLocation"
      , String "siSceneStakes"
      , String "siOpeningHook"
      , String "siSuggestedActions"
      ])
  ]
