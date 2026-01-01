{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | DM Graph Runner
--
-- Entry points for running the DM graph.
--
-- The graph runner provides:
-- - 'runDMGraph': Execute graph from entry with player input
-- - 'runDMGraphFromResume': Execute from resume router (saved mood)
-- - 'interpretDMEffects': Wire up all effect handlers
--
-- Usage with terminal IO:
-- @
-- (response, finalState) <- interpretDMEffectsTerminal llmConfig initialState $ \\runEffects ->
--   runEffects $ runDMGraph (PlayerInput "I look around" Nothing)
-- @
--
-- Usage with GUI:
-- @
-- interpretDMEffectsGUI llmConfig initialState bridge eventHandler $ \\runEffects ->
--   runEffects $ runDMGraph playerInput
-- @
module DM.Graph.Run
  ( -- * Graph Execution
    runDMGraph
  , runDMGraphFromResume

    -- * Effect Interpretation
  , interpretDMEffectsTerminal
  , interpretDMEffectsGUI
  , RunGraphEffects
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Effectful
import Effectful.State.Static.Local (State, runState)
import System.IO (hFlush, stdout)

import Tidepool.Effect
  ( LLM, RequestInput, Log, Random, Emit
  , LLMConfig(..), InputHandler(..), LogLevel(..)
  , runRandom, runEmit, runLog, runRequestInput, runChatHistory, runTime
  )
import Tidepool.Effect.Runners (runLLMWithTools)
import Tidepool.Graph.Execute (runGraph, runGraphFrom)

import DM.State (WorldState)
import DM.Tools (DMEvent, makeDMDispatcher)
import DM.Graph (DMGraph)
import DM.Graph.Types (PlayerInput(..), Response)
import DM.Graph.Handlers (DMEffects, dmHandlers)

-- ══════════════════════════════════════════════════════════════
-- GRAPH EXECUTION
-- ══════════════════════════════════════════════════════════════

-- | Run the DM graph from entry to exit.
--
-- Takes player input and returns the final response after executing
-- all graph transitions until Exit is reached.
--
-- @
-- response <- runDMGraph (PlayerInput "I look around" Nothing)
-- @
runDMGraph
  :: PlayerInput
  -> Eff DMEffects Response
runDMGraph = runGraph dmHandlers

-- | Run the DM graph from the resume router.
--
-- This is the main entry point - it uses resumeRouter to determine
-- which node to start from based on the saved mood.
--
-- For testing specific paths, use runGraphFrom directly:
-- @
-- response <- runGraphFrom @"action" dmHandlers actionSetup
-- @
runDMGraphFromResume
  :: PlayerInput
  -> Eff DMEffects Response
runDMGraphFromResume = runGraphFrom @"resumeRouter" dmHandlers

-- ══════════════════════════════════════════════════════════════
-- EFFECT INTERPRETATION
-- ══════════════════════════════════════════════════════════════

-- | Type alias for the effect runner function
type RunGraphEffects a = Eff DMEffects a -> IO (a, WorldState)

-- | Interpret DM effects with terminal IO.
--
-- This wires up all effect handlers for terminal-based gameplay:
-- - State: WorldState
-- - LLM: Anthropic API with tools
-- - RequestInput: Terminal prompts
-- - Emit: Print events to stdout
-- - Random: System random
-- - Log: Terminal debug logging
--
-- Usage:
-- @
-- (response, finalState) <- interpretDMEffectsTerminal llmConfig initialState eventHandler $ \\runEffects ->
--   runEffects $ runDMGraph playerInput
-- @
interpretDMEffectsTerminal
  :: LLMConfig                     -- ^ LLM configuration
  -> WorldState                    -- ^ Initial game state
  -> (DMEvent -> IO ())            -- ^ Event handler
  -> (RunGraphEffects a -> IO b)   -- ^ Action to run with interpreter
  -> IO b
interpretDMEffectsTerminal llmConfig initialState eventHandler action = do
  let inputHandler = InputHandler
        { ihChoice = terminalChoice
        , ihText = terminalText
        , ihDice = terminalDice
        , ihCustom = \tag _ -> error $ "Custom request '" <> T.unpack tag <> "' not supported"
        }

      runEffects :: Eff DMEffects a -> IO (a, WorldState)
      runEffects computation = runEff
        . runTime
        . runRandom
        . runEmit eventHandler
        . runState initialState
        . runChatHistory
        . runLog Debug
        . runRequestInput inputHandler
        . runLLMWithTools @_ @DMEvent llmConfig makeDMDispatcher
        $ computation

  action runEffects

-- | Interpret DM effects with GUI bridge.
--
-- Similar to 'interpretDMEffectsTerminal' but uses GUI handlers:
-- - RequestInput: GUI bridge (TVar/MVar based)
-- - Log: GUI debug panel
-- - Emit: GUI event display
interpretDMEffectsGUI
  :: LLMConfig
  -> WorldState
  -> InputHandler                  -- ^ GUI-based input handler
  -> (DMEvent -> IO ())
  -> (RunGraphEffects a -> IO b)
  -> IO b
interpretDMEffectsGUI llmConfig initialState inputHandler eventHandler action = do
  let runEffects :: Eff DMEffects a -> IO (a, WorldState)
      runEffects computation = runEff
        . runTime
        . runRandom
        . runEmit eventHandler
        . runState initialState
        . runChatHistory
        . runLog Debug
        . runRequestInput inputHandler
        . runLLMWithTools @_ @DMEvent llmConfig makeDMDispatcher
        $ computation

  action runEffects

-- ══════════════════════════════════════════════════════════════
-- TERMINAL INPUT HANDLERS
-- ══════════════════════════════════════════════════════════════

-- | Terminal choice handler
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
