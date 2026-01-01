{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | DM Graph Definition
--
-- This module defines the DM agent as a type-safe graph using the v2 Graph DSL.
-- The DM's mood state machine maps to graph nodes:
--
-- @
--   Entry(PlayerInput)
--     → resumeRouter (determines starting node from saved mood)
--     → sceneEncounter | sceneOpportunity | sceneDiscovery
--       ├─→ action (via engage tool)
--       │     ├─→ aftermath (via resolve tool)
--       │     │     ├─→ sceneRouter (via accept tool)
--       │     │     └─→ trauma (if stress >= 9)
--       │     └─→ bargain (if dice pool empty)
--       │           ├─→ sceneRouter (accept deal)
--       │           ├─→ downtime (retreat)
--       │           └─→ trauma (pass out)
--       └─→ downtime (scene ends)
--             └─→ sceneRouter | Exit
-- @
module DM.Graph
  ( -- * Graph Definition
    DMGraph(..)

    -- * Effect Types
  , DMSceneEffects
  , DMActionEffects
  , DMAftermathEffects
  , DMBargainEffects
  , DMTraumaEffects
  , DMDowntimeEffects
  ) where

import Data.Proxy (Proxy)
import GHC.Generics (Generic)

import Tidepool.Graph.Types (type (:@), Needs, UsesEffects, Exit)
import Tidepool.Graph.Generic (GraphMode(..), type (:-))
import qualified Tidepool.Graph.Generic as G (Entry, Exit, LogicNode)
import Tidepool.Graph.Goto (Goto)

import DM.Graph.Types
  ( PlayerInput
  , SceneSetup
  , ActionSetup
  , AftermathSetup
  , BargainSetup
  , TraumaSetup
  , DowntimeSetup
  , Response
  )

-- ══════════════════════════════════════════════════════════════
-- EFFECT TYPE ALIASES
-- ══════════════════════════════════════════════════════════════

-- | Scene nodes can:
-- - Continue in scene (Self loop)
-- - Engage in action (→ action)
-- - End scene (→ downtime)
-- - End session (→ Exit)
type DMSceneEffects =
  '[ Goto "action" ActionSetup        -- engage tool triggers action
   , Goto "downtime" DowntimeSetup    -- scene ends naturally
   , Goto "sceneRouter" SceneSetup    -- continue scene with new variant
   , Goto Exit Response               -- session ends
   ]

-- | Action nodes can:
-- - Resolve action (→ aftermath)
-- - Run out of dice (→ bargain)
-- - Continue action (Self loop)
type DMActionEffects =
  '[ Goto "aftermath" AftermathSetup  -- resolve tool triggers aftermath
   , Goto "bargain" BargainSetup      -- empty dice pool
   , Goto "action" ActionSetup        -- continue in action (retry)
   ]

-- | Aftermath nodes can:
-- - Accept outcome (→ sceneRouter)
-- - Trigger trauma (→ trauma) if stress >= 9
-- - Continue aftermath
type DMAftermathEffects =
  '[ Goto "sceneRouter" SceneSetup    -- accept tool returns to scene
   , Goto "trauma" TraumaSetup        -- stress >= 9
   , Goto "downtime" DowntimeSetup    -- scene ends after aftermath
   , Goto "aftermath" AftermathSetup  -- continue in aftermath
   ]

-- | Bargain nodes can:
-- - Accept deal (→ sceneRouter) with new dice
-- - Retreat (→ downtime) if allowed
-- - Pass out (→ trauma)
type DMBargainEffects =
  '[ Goto "sceneRouter" SceneSetup    -- accept_bargain returns to scene
   , Goto "downtime" DowntimeSetup    -- retreat
   , Goto "trauma" TraumaSetup        -- pass_out
   , Goto "bargain" BargainSetup      -- continue bargaining
   ]

-- | Trauma nodes can:
-- - Return to scene after processing trauma
-- - End session
type DMTraumaEffects =
  '[ Goto "sceneRouter" SceneSetup    -- return to scene after trauma
   , Goto Exit Response               -- session ends
   ]

-- | Downtime nodes can:
-- - Start new scene
-- - End session
type DMDowntimeEffects =
  '[ Goto "sceneRouter" SceneSetup    -- start new scene
   , Goto Exit Response               -- end session
   ]

-- ══════════════════════════════════════════════════════════════
-- GRAPH DEFINITION
-- ══════════════════════════════════════════════════════════════

-- | The DM Graph - a type-safe state machine for the Dungeon Master agent.
--
-- Each mood becomes a node, and mood transitions become Goto effects.
-- The graph is parameterized by a mode which determines the interpretation
-- of each field (node definition vs. handler implementation).
data DMGraph mode = DMGraph
  { -- Entry point: receives player input
    entry :: mode :- G.Entry PlayerInput

    -- Resume router: determines which node to start from based on saved mood
  , resumeRouter :: mode :- G.LogicNode
                        :@ Needs '[PlayerInput]
                        :@ UsesEffects '[Goto "sceneRouter" SceneSetup
                                        , Goto "action" ActionSetup
                                        , Goto "aftermath" AftermathSetup
                                        , Goto "bargain" BargainSetup
                                        , Goto "trauma" TraumaSetup
                                        , Goto "downtime" DowntimeSetup
                                        ]

    -- Scene router: picks scene variant based on world state
  , sceneRouter :: mode :- G.LogicNode
                       :@ Needs '[SceneSetup]
                       :@ UsesEffects '[Goto "sceneEncounter" SceneSetup
                                       , Goto "sceneOpportunity" SceneSetup
                                       , Goto "sceneDiscovery" SceneSetup
                                       ]

    -- Scene: Encounter (someone demands attention)
  , sceneEncounter :: mode :- G.LogicNode
                          :@ Needs '[SceneSetup]
                          :@ UsesEffects DMSceneEffects

    -- Scene: Opportunity (something offered)
  , sceneOpportunity :: mode :- G.LogicNode
                            :@ Needs '[SceneSetup]
                            :@ UsesEffects DMSceneEffects

    -- Scene: Discovery (found something)
  , sceneDiscovery :: mode :- G.LogicNode
                          :@ Needs '[SceneSetup]
                          :@ UsesEffects DMSceneEffects

    -- Action: Dice resolution phase
  , action :: mode :- G.LogicNode
                  :@ Needs '[ActionSetup]
                  :@ UsesEffects DMActionEffects

    -- Aftermath: Consequences manifest
  , aftermath :: mode :- G.LogicNode
                     :@ Needs '[AftermathSetup]
                     :@ UsesEffects DMAftermathEffects

    -- Bargain: Out of dice, must make a deal
  , bargain :: mode :- G.LogicNode
                   :@ Needs '[BargainSetup]
                   :@ UsesEffects DMBargainEffects

    -- Trauma: Breaking point (stress >= 9)
  , trauma :: mode :- G.LogicNode
                  :@ Needs '[TraumaSetup]
                  :@ UsesEffects DMTraumaEffects

    -- Downtime: Between scenes
  , downtime :: mode :- G.LogicNode
                    :@ Needs '[DowntimeSetup]
                    :@ UsesEffects DMDowntimeEffects

    -- Exit: Final response to player
  , exit :: mode :- G.Exit Response
  }
  deriving Generic
