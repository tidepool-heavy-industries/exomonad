{-# LANGUAGE TypeApplications #-}
-- | DM Game Loop
module DM.Loop
  ( -- * Main Loop
    dmTurn
  , runDMGame

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
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import System.IO (hFlush, stdout)
import System.Environment (lookupEnv)

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
  -- 1. Record player action as scene beat (only on first call, not restarts)
  handlePlayerAction input

  -- 2. Run the turn (may restart on mood transition)
  -- Pass the player action to the inner loop
  runMoodAwareTurn input.piActionText

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

          -- Call LLM with system prompt and user action
          outcome <- runTurn systemPrompt userAction turnOutputSchema.schemaJSON dmTools

          case outcome of
            -- Tool triggered state transition - restart with new mood
            TurnBroken reason -> do
              logInfo $ "Mood transition: " <> reason
              runMoodAwareTurn userAction  -- Recursive call with new mood

            -- Turn completed normally - apply output and continue
            TurnCompleted result -> do
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

              -- Compress if scene is getting long
              compressIfNeeded

              -- Return response with narrative and mechanical changes
              let output = result.trOutput
              return Response
                { responseText = narration
                , responseStressDelta = output.stressDelta
                , responseCoinDelta = output.coinDelta
                , responseHeatDelta = output.heatDelta
                , responseSuggestedActions = output.suggestedActions
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
        outcome <- runTurn systemPrompt userAction compressionOutputSchema.schemaJSON []

        -- Compression shouldn't break, but handle it gracefully
        case outcome of
          TurnBroken reason -> do
            logWarn $ "Compression unexpectedly broke: " <> reason
            -- Continue without compressing

          TurnCompleted result -> do
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
            , llmSystemPrompt = "You are the Dungeon Master for a Blades in the Dark style game. Respond to player actions with narrative prose, using your tools as appropriate."
            }

      -- Set up terminal input handler
      let inputHandler = InputHandler
            { ihChoice = terminalChoice
            , ihText = terminalText
            }

      -- Start with a scene at the Leaky Bucket (neutral territory)
      let stateWithScene = initialState
            { scene = Just ActiveScene
                { sceneLocation = LocationId "leaky-bucket"
                , scenePresent = [NpcId "telda"]  -- Bartender is present
                , sceneStakes = Stakes "Find out what's happening in Crow's Foot"
                , sceneBeats = Seq.empty
                }
            }

      TIO.putStrLn "\n[Scene: The Leaky Bucket - neutral ground in Crow's Foot]"
      TIO.putStrLn "[Telda, the bartender, is behind the counter.]"
      TIO.putStrLn "[Type 'quit' to exit]\n"

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
