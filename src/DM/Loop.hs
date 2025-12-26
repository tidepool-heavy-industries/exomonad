-- | DM Game Loop
module DM.Loop
  ( -- * Main Loop
    dmTurn
  , runDMGame

    -- * Turn Operations
  , handlePlayerAction
  , handleDiceSelection
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
import DM.Context (buildDMContext)
import DM.Output (TurnOutput, applyTurnOutput)
import DM.Templates (dmTurnTemplate)
import DM.Tools (DMEvent(..), dmTools)
import Tidepool.Effect
import Tidepool.Template (render)
import Tidepool.Tool (toolListToJSON)

import Effectful
import Data.Text (Text)
import Data.Aeson (Value)

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

-- | Schema for TurnOutput (placeholder - should use deriveSchema in real impl)
turnOutputSchema :: Value
turnOutputSchema = error "TODO: turnOutputSchema - generate JSON Schema for TurnOutput"

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
  result <- runTurn (render dmTurnTemplate) turnOutputSchema dmTools context

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
handleDiceRequest
  :: ( State WorldState :> es
     , RequestInput :> es
     )
  => TurnOutput
  -> Eff es ()
handleDiceRequest _output = error "TODO: handleDiceRequest - check requestOutcomes, use requestChoice for dice selection"

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
  where
    describeDie :: Int -> Position -> Text
    describeDie die pos = error "TODO: describeDie - show die value and outcome tier"

handlePlayerAction
  :: State WorldState :> es
  => PlayerInput
  -> Eff es ()
handlePlayerAction _input = error "TODO: handlePlayerAction - add PlayerAction beat to current scene"

checkClockConsequences
  :: ( State WorldState :> es
     , Emit DMEvent :> es
     )
  => Eff es ()
checkClockConsequences = error "TODO: checkClockConsequences - find completed clocks, execute consequences, remove"

compressIfNeeded
  :: ( State WorldState :> es
     , LLM :> es
     )
  => Eff es ()
compressIfNeeded = error "TODO: compressIfNeeded - check beat count, run compression if threshold exceeded"

-- ══════════════════════════════════════════════════════════════
-- SCENE MANAGEMENT
-- ══════════════════════════════════════════════════════════════

startScene
  :: State WorldState :> es
  => LocationId
  -> [NpcId]
  -> Stakes
  -> Eff es ()
startScene locationId npcs stakes = error "TODO: startScene - create ActiveScene, set in WorldState"

endCurrentScene 
  :: State WorldState :> es 
  => Eff es ()
endCurrentScene = error "TODO: endCurrentScene - clear scene from WorldState"

-- ══════════════════════════════════════════════════════════════
-- RUNNING THE GAME
-- ══════════════════════════════════════════════════════════════

runDMGame
  :: WorldState
  -> (DMEvent -> IO ())
  -> IO ()
runDMGame initialWorld handleEvent = error "TODO: runDMGame - set up effect stack, run main loop with player I/O"
