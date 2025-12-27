-- | DM Game Loop
module DM.Loop
  ( -- * Main Loop
    dmTurn
  , runDMGame

    -- * Turn Operations
  , handlePlayerAction
  , handleDiceSelection
  , handleDiceRequest
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
import DM.Context (buildDMContext, buildCompressionContext)
import DM.Output (TurnOutput(..), CompressionOutput(..), SceneOutcome(..), applyTurnOutput, applyCompression, RequestOutcomes(..))
import DM.Templates (renderDMTurn, renderCompression, turnOutputSchema, compressionOutputSchema)
import DM.Tools (DMEvent(..), dmTools)
import Tidepool.Effect
import Tidepool.Template (Schema(..))

import Effectful
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import System.IO (hFlush, stdout)

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
dmTurn
  :: ( State WorldState :> es
     , Random :> es
     , LLM :> es
     , Emit DMEvent :> es
     , RequestInput :> es
     )
  => PlayerInput
  -> Eff es Response
dmTurn input = do
  -- 1. Record player action as scene beat
  handlePlayerAction input

  -- 2. Build context from world state
  context <- gets buildDMContext

  -- 3. Call LLM with template and tools
  result <- runTurn renderDMTurn turnOutputSchema.schemaJSON dmTools context

  -- 4. Apply structured output to world state
  modify (applyTurnOutput result.trOutput)

  -- 5. Handle dice mechanics if LLM requested outcomes
  handleDiceRequest result.trOutput

  -- 6. Check clock consequences
  checkClockConsequences

  -- 7. Compress if scene is getting long
  compressIfNeeded

  -- 8. Return narrative response
  return (Response result.trNarrative)

-- | Handle dice selection when LLM requests outcomes
-- Sets up PendingOutcome so player can select a die on next turn
handleDiceRequest
  :: ( State WorldState :> es
     , RequestInput :> es
     )
  => TurnOutput
  -> Eff es ()
handleDiceRequest output = case output.requestOutcomes of
  Nothing -> return ()  -- No dice request this turn
  Just req -> do
    -- Set up pending outcome for player to select die
    modify $ \state -> state
      { pendingOutcome = Just PendingOutcome
          { outcomeContext = req.requestContext
          , outcomePosition = req.requestPosition
          , outcomeEffect = req.requestEffect
          , outcomeStakes = req.requestStakes
          , chosenDie = Nothing
          , chosenTier = Nothing
          }
      }
    -- Immediately prompt for die selection
    _ <- handleDiceSelection
    return ()

-- | Handle dice selection from player
handleDiceSelection
  :: ( State WorldState :> es
     , RequestInput :> es
     )
  => Eff es (Maybe Int)
handleDiceSelection = do
  state <- get
  case state.pendingOutcome of
    Nothing -> return Nothing
    Just pending -> do
      let pool = state.dicePool.poolDice
          choices = [(describeDie d pending.outcomePosition, d) | d <- pool]
      selected <- requestChoice "Select a die from your pool:" choices
      -- Update pending outcome with selection
      let outcome = calculateOutcome pending.outcomePosition selected
      put state { pendingOutcome = Just pending { chosenDie = Just selected, chosenTier = Just outcome } }
      return (Just selected)

-- | Format a die value with its outcome for display
-- Shows: "⚄ (4) → Success" or "⚀ (1) → Disaster"
describeDie :: Int -> Position -> Text
describeDie die pos =
  let dieChar = case die of
        1 -> "⚀"
        2 -> "⚁"
        3 -> "⚂"
        4 -> "⚃"
        5 -> "⚄"
        6 -> "⚅"
        _ -> "?"
      outcome = calculateOutcome pos die
      tierText = case outcome of
        Critical -> "Critical!"
        Success  -> "Success"
        Partial  -> "Partial"
        Bad      -> "Bad"
        Disaster -> "Disaster"
  in dieChar <> " (" <> T.pack (show die) <> ") → " <> tierText

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
        result <- runTurn renderCompression compressionOutputSchema.schemaJSON [] ctx

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
  -- Set up the LLM config (stub - will be configured properly later)
  let llmConfig = LLMConfig
        { llmApiKey = ""
        , llmModel = "claude-sonnet-4-20250514"
        , llmMaxTokens = 4096
        }

  -- Set up terminal input handler
  let inputHandler = InputHandler
        { ihChoice = terminalChoice
        , ihText = terminalText
        }

  -- Run the game loop
  ((), _finalState) <- runGame initialState llmConfig handleEvent inputHandler (gameLoop @(GameEffects WorldState DMEvent))

  TIO.putStrLn "Game ended."

-- Main game loop - separate function to help with type inference
gameLoop
  :: forall es.
     ( State WorldState :> es
     , LLM :> es
     , Emit DMEvent :> es
     , RequestInput :> es
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
