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
import DM.Output (TurnOutput(..), CompressionOutput(..), SceneOutcome(..), applyTurnOutput, applyCompression)
import DM.Templates (renderForMood, renderCompression, turnOutputSchema, compressionOutputSchema)
import DM.Tools (DMEvent(..), dmTools)
import Tidepool.Effect hiding (ToolResult)
import Tidepool.Effect (TurnOutcome(..))
import Tidepool.Template (Schema(..))

import Effectful
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

newtype Response = Response { responseText :: Text }
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
  runMoodAwareTurn

  where
    runMoodAwareTurn = do
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

      -- Call LLM with mood-specific template and tools
      outcome <- runTurn (renderForMood mood) turnOutputSchema.schemaJSON dmTools context

      case outcome of
        -- Tool triggered state transition - restart with new mood
        TurnBroken reason -> do
          logInfo $ "Mood transition: " <> reason
          runMoodAwareTurn  -- Recursive call with new mood

        -- Turn completed normally - apply output and continue
        TurnCompleted result -> do
          -- Apply structured output to world state
          modify (applyTurnOutput result.trOutput)

          -- Record DM response as scene beat (for history)
          let narration = result.trOutput.narration
          modify $ \state -> case state.scene of
            Nothing -> state
            Just activeScene ->
              let beat = DMNarration narration
                  newBeats = activeScene.sceneBeats Seq.|> beat
              in state { scene = Just activeScene { sceneBeats = newBeats } }

          -- Check clock consequences
          checkClockConsequences

          -- Compress if scene is getting long
          compressIfNeeded

          -- Return narrative response
          return (Response narration)

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

        -- Call LLM to compress the scene
        outcome <- runTurn renderCompression compressionOutputSchema.schemaJSON [] ctx

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
            emit $ SceneCompressed (result.trOutput.sceneOutcome.outcomeSummary)
  where
    compressionThreshold = 20

-- Helper for conditional execution
when :: Applicative f => Bool -> f () -> f ()
when True action = action
when False _     = pure ()

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
runDMGame
  :: WorldState
  -> (DMEvent -> IO ())
  -> IO ()
runDMGame initialState handleEvent = do
  -- Get API key from environment
  maybeKey <- lookupEnv "ANTHROPIC_API_KEY"
  case maybeKey of
    Nothing -> do
      TIO.putStrLn "Error: ANTHROPIC_API_KEY not set"
      TIO.putStrLn "Run: export ANTHROPIC_API_KEY=your-key-here"
      return ()
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

      -- Run the game loop (Debug level shows all log messages)
      ((), _finalState) <- runGame stateWithScene llmConfig handleEvent inputHandler Debug (gameLoop @(GameEffects WorldState DMEvent))

      TIO.putStrLn "Game ended."

-- Main game loop - separate function to help with type inference
gameLoop
  :: forall es.
     ( State WorldState :> es
     , LLM :> es
     , Emit DMEvent :> es
     , RequestInput :> es
     , Log :> es
     , Random :> es
     , IOE :> es
     )
  => Eff es ()
gameLoop = do
  -- Check if we have an active scene
  state <- get @WorldState
  case state.scene of
    Nothing -> do
      -- No scene - prompt to start one or quit
      liftIO $ TIO.putStrLn "\n[No active scene. Use startScene to begin.]"
      return ()  -- Exit for now - real game would prompt for scene setup

    Just _ -> do
      -- Get player input
      liftIO $ TIO.putStr "\n> What do you do? "
      liftIO $ hFlush stdout
      inputText <- liftIO TIO.getLine

      -- Check for quit command
      if T.toLower (T.strip inputText) `elem` ["quit", "exit", "q"]
        then return ()  -- Exit loop
        else do
          -- Parse input and run turn
          let playerInput = PlayerInput
                { piActionText = inputText
                , piActionTags = []  -- Could parse tags from input
                }

          -- Run a turn
          response <- dmTurn playerInput

          -- Display response
          liftIO $ TIO.putStrLn ""
          liftIO $ TIO.putStrLn response.responseText

          -- Continue loop
          gameLoop

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
