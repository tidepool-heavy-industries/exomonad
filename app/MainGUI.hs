-- | Main entry point for the Tidepool DM GUI
--
-- This runs the GUI in standalone demo mode with sample data.
-- Integration with the actual game loop is done separately.
module Main where

import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq

import DM.State
import DM.GUI.App (dmGUISetup, defaultDMGUIConfig)
import Tidepool.GUI.Core (newGUIBridge, addNarrative)
import Tidepool.GUI.Server (startServer, defaultServerConfig, ServerConfig(..))

main :: IO ()
main = do
  -- Create a demo world state with some sample data
  let demoWorld = createDemoWorld

  -- Create the GUI bridge
  bridge <- newGUIBridge demoWorld

  -- Add some demo narrative
  addNarrative bridge "The rain drums on the cobblestones as you make your way through the Docks."
  addNarrative bridge "A figure emerges from the shadows of an alley - it's Bazso Baz, leader of the Lampblacks."

  -- Configure and start the server
  let config = defaultServerConfig
        { scTitle = "Tidepool DM - Blades in the Dark"
        }

  startServer config (dmGUISetup defaultDMGUIConfig bridge)

-- | Create a demo world state with sample data for testing
createDemoWorld :: WorldState
createDemoWorld = initialWorld
  { player = demoPlayer
  , mood = demoMood
  , clocks = demoClocks
  , dicePool = DicePool [1, 3, 4, 6]  -- Sample dice pool
  }

-- | Demo player with some stats
demoPlayer :: PlayerState
demoPlayer = PlayerState
  { stress = 4
  , coin = 7
  , heat = 2
  , wanted = 1
  , trauma = [Trauma "Cold"]
  }

-- | Demo mood - a scene with a job offer
demoMood :: DMMood
demoMood = MoodScene $ Opportunity
  { svOfferedBy = Just (NpcId "bazso_baz")
  , svNature = "theft job"
  , svCatch = "The Red Sashes will know it was you"
  }

-- | Demo clocks
demoClocks :: HM.HashMap ClockId Clock
demoClocks = HM.fromList
  [ (ClockId "bluecoat_investigation", Clock
      { clockName = "Bluecoat Investigation"
      , clockSegments = 6
      , clockFilled = 2
      , clockVisible = True
      , clockConsequence = Escalate (Escalation
          { escalationDescription = "The Bluecoats close in"
          , escalationSeverity = Moderate
          })
      , clockTriggers = []
      })
  , (ClockId "red_sash_revenge", Clock
      { clockName = "Red Sash Revenge"
      , clockSegments = 4
      , clockFilled = 1
      , clockVisible = True
      , clockConsequence = FactionMoves (FactionId "red_sashes") (Attack (Target "player"))
      , clockTriggers = []
      })
  ]
