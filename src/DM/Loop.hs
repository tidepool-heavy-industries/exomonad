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
import DM.Output
import DM.Context
import DM.Templates
import DM.Tools
import Tidepool.Effect

import Effectful
import Data.Text (Text)

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

dmTurn 
  :: ( State WorldState :> es
     , Random :> es
     , LLM :> es
     , Emit DMEvent :> es
     )
  => PlayerInput
  -> Eff es Response
dmTurn input = error "TODO: dmTurn - record beat, build context, call LLM, apply output, check clocks, maybe compress"

handlePlayerAction 
  :: State WorldState :> es 
  => PlayerInput 
  -> Eff es ()
handlePlayerAction input = error "TODO: handlePlayerAction - add PlayerAction beat to current scene"

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
