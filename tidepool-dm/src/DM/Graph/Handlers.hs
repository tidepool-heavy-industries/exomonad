{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | DM Graph Handlers
--
-- Handler implementations for each node in the DMGraph.
-- Each handler takes its Needs payload and returns a GotoChoice.
module DM.Graph.Handlers
  ( -- * Effect Stack
    DMEffects

    -- * Handler Record
  , dmHandlers

    -- * Individual Handlers
  , resumeRouterHandler
  , sceneRouterHandler
  , sceneEncounterHandler
  , sceneOpportunityHandler
  , sceneDiscoveryHandler
  , actionHandler
  , aftermathHandler
  , bargainHandler
  , traumaHandler
  , downtimeHandler
  ) where

import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import Effectful

import Tidepool.Effect (LLM, RequestInput, Log, Random, Emit, logInfo)
import Effectful.State.Static.Local (State, get, modify)
import Tidepool.Graph.Goto (GotoChoice, To, gotoChoice, gotoExit)
import Tidepool.Graph.Types (Exit)
import Tidepool.Graph.Generic (AsHandler)

import DM.State
import DM.Tools (DMEvent(..))
import DM.Graph (DMGraph(..))
import DM.Graph.Types

-- ══════════════════════════════════════════════════════════════
-- EFFECT STACK
-- ══════════════════════════════════════════════════════════════

-- | Effect stack available to DM handlers
--
-- Handlers are IO-blind - all IO happens in the runner via effect interpreters.
type DMEffects =
  '[ State WorldState      -- Game state
   , LLM                   -- Language model calls
   , RequestInput          -- Player choices (dice, text, options)
   , Emit DMEvent          -- Event log for GUI
   , Random                -- Dice rolls
   , Log                   -- Debug logging
   ]

-- ══════════════════════════════════════════════════════════════
-- HANDLER RECORD
-- ══════════════════════════════════════════════════════════════

-- | Complete handler record for the DM graph
dmHandlers :: DMGraph (AsHandler DMEffects)
dmHandlers = DMGraph
  { entry = Proxy @PlayerInput

  , resumeRouter = resumeRouterHandler
  , sceneRouter = sceneRouterHandler

  , sceneEncounter = sceneEncounterHandler
  , sceneOpportunity = sceneOpportunityHandler
  , sceneDiscovery = sceneDiscoveryHandler

  , action = actionHandler
  , aftermath = aftermathHandler
  , bargain = bargainHandler
  , trauma = traumaHandler
  , downtime = downtimeHandler

  , exit = Proxy @Response
  }

-- ══════════════════════════════════════════════════════════════
-- RESUME ROUTER
-- ══════════════════════════════════════════════════════════════

-- | Route to the appropriate node based on saved mood or fresh start
resumeRouterHandler
  :: PlayerInput
  -> Eff DMEffects (GotoChoice
      '[ To "sceneRouter" SceneSetup
       , To "action" ActionSetup
       , To "aftermath" AftermathSetup
       , To "bargain" BargainSetup
       , To "trauma" TraumaSetup
       , To "downtime" DowntimeSetup
       ])
resumeRouterHandler input = do
  state <- get @WorldState
  logInfo $ "[ResumeRouter] Current mood: " <> T.pack (show state.mood)

  case input.piResumeMood of
    -- Fresh start or explicit scene request
    Nothing -> do
      let setup = SceneSetup
            { ssVariant = getSceneVariant state.mood
            , ssPlayerAction = input.piActionText
            , ssPreviousNarration = Nothing
            }
      pure $ gotoChoice @"sceneRouter" setup

    -- Resume from saved mood
    Just mood -> routeToMood mood input.piActionText

  where
    getSceneVariant (MoodScene sv) = sv
    getSceneVariant _ = Encounter "continuing" UrgencyLow True

    routeToMood mood action = case mood of
      MoodScene sv -> do
        let setup = SceneSetup
              { ssVariant = sv
              , ssPlayerAction = action
              , ssPreviousNarration = Nothing
              }
        pure $ gotoChoice @"sceneRouter" setup

      MoodAction av domain -> do
        let setup = ActionSetup
              { asVariant = av
              , asDomain = domain
              , asPlayerAction = action
              , asContext = "resuming action"
              }
        pure $ gotoChoice @"action" setup

      MoodAftermath av -> do
        let setup = AftermathSetup
              { afVariant = av
              , afOutcomeTier = Success  -- Default, should be saved
              , afActionContext = "resuming aftermath"
              }
        pure $ gotoChoice @"aftermath" setup

      MoodBargain bv -> do
        let setup = BargainSetup
              { bsVariant = bv
              , bsPlayerAction = action
              }
        pure $ gotoChoice @"bargain" setup

      MoodTrauma tv -> do
        let setup = TraumaSetup
              { tsVariant = tv
              , tsTrigger = "resuming trauma"
              }
        pure $ gotoChoice @"trauma" setup

      MoodDowntime dv -> do
        let setup = DowntimeSetup
              { dsVariant = dv
              , dsPreviousSceneSummary = "resuming"
              }
        pure $ gotoChoice @"downtime" setup

-- ══════════════════════════════════════════════════════════════
-- SCENE ROUTER
-- ══════════════════════════════════════════════════════════════

-- | Route to the appropriate scene variant
sceneRouterHandler
  :: SceneSetup
  -> Eff DMEffects (GotoChoice
      '[ To "sceneEncounter" SceneSetup
       , To "sceneOpportunity" SceneSetup
       , To "sceneDiscovery" SceneSetup
       ])
sceneRouterHandler setup = do
  logInfo $ "[SceneRouter] Routing to variant: " <> variantName setup.ssVariant

  case setup.ssVariant of
    Encounter{} -> pure $ gotoChoice @"sceneEncounter" setup
    Opportunity{} -> pure $ gotoChoice @"sceneOpportunity" setup
    Discovery{} -> pure $ gotoChoice @"sceneDiscovery" setup

  where
    variantName Encounter{} = "encounter"
    variantName Opportunity{} = "opportunity"
    variantName Discovery{} = "discovery"

-- ══════════════════════════════════════════════════════════════
-- SCENE HANDLERS
-- ══════════════════════════════════════════════════════════════

-- | Handle an Encounter scene
sceneEncounterHandler
  :: SceneSetup
  -> Eff DMEffects (GotoChoice
      '[ To "action" ActionSetup
       , To "downtime" DowntimeSetup
       , To "sceneRouter" SceneSetup
       , To Exit Response
       ])
sceneEncounterHandler setup = do
  logInfo $ "[SceneEncounter] Processing: " <> setup.ssPlayerAction

  -- TODO: Build context, call LLM, process output
  -- For now, stub that continues the scene
  state <- get @WorldState

  -- Check if player action triggers action mode
  -- In the full implementation, the LLM decides this via the engage tool
  let isRiskyAction = containsRiskyKeywords setup.ssPlayerAction

  if isRiskyAction
    then do
      logInfo "[SceneEncounter] Risky action detected, transitioning to action"
      let actionSetup = ActionSetup
            { asVariant = AvRisky "unknown threat" "potential gain"
            , asDomain = Nothing
            , asPlayerAction = setup.ssPlayerAction
            , asContext = "scene action"
            }
      pure $ gotoChoice @"action" actionSetup
    else do
      -- Continue in scene
      let newSetup = setup { ssPreviousNarration = Just "The scene continues..." }
      pure $ gotoChoice @"sceneRouter" newSetup

  where
    containsRiskyKeywords txt =
      any (`T.isInfixOf` T.toLower txt)
        ["attack", "fight", "steal", "sneak", "break", "force", "threaten"]

-- | Handle an Opportunity scene
sceneOpportunityHandler
  :: SceneSetup
  -> Eff DMEffects (GotoChoice
      '[ To "action" ActionSetup
       , To "downtime" DowntimeSetup
       , To "sceneRouter" SceneSetup
       , To Exit Response
       ])
sceneOpportunityHandler setup = do
  logInfo $ "[SceneOpportunity] Processing: " <> setup.ssPlayerAction

  -- TODO: Implement full LLM-based handling
  -- Stub: continue scene
  let newSetup = setup { ssPreviousNarration = Just "The opportunity unfolds..." }
  pure $ gotoChoice @"sceneRouter" newSetup

-- | Handle a Discovery scene
sceneDiscoveryHandler
  :: SceneSetup
  -> Eff DMEffects (GotoChoice
      '[ To "action" ActionSetup
       , To "downtime" DowntimeSetup
       , To "sceneRouter" SceneSetup
       , To Exit Response
       ])
sceneDiscoveryHandler setup = do
  logInfo $ "[SceneDiscovery] Processing: " <> setup.ssPlayerAction

  -- TODO: Implement full LLM-based handling
  -- Stub: continue scene
  let newSetup = setup { ssPreviousNarration = Just "The discovery reveals more..." }
  pure $ gotoChoice @"sceneRouter" newSetup

-- ══════════════════════════════════════════════════════════════
-- ACTION HANDLER
-- ══════════════════════════════════════════════════════════════

-- | Handle the action phase (dice resolution)
actionHandler
  :: ActionSetup
  -> Eff DMEffects (GotoChoice
      '[ To "aftermath" AftermathSetup
       , To "bargain" BargainSetup
       , To "action" ActionSetup
       ])
actionHandler setup = do
  logInfo $ "[Action] Processing: " <> setup.asPlayerAction

  state <- get @WorldState

  -- Check if dice pool is empty
  if null state.dicePool.poolDice
    then do
      logInfo "[Action] Dice pool empty, transitioning to bargain"
      let bargainSetup = BargainSetup
            { bsVariant = Bargaining
                { bvWhatDrained = setup.asContext
                , bvCanRetreat = True
                , bvRetreatDesc = "slip away"
                , bvPassOutDesc = "collapse"
                , bvPreviousMood = MoodAction setup.asVariant setup.asDomain
                }
            , bsPlayerAction = setup.asPlayerAction
            }
      pure $ gotoChoice @"bargain" bargainSetup
    else do
      -- TODO: Request dice selection, resolve outcome
      -- Stub: transition to aftermath with success
      let aftermathSetup = AftermathSetup
            { afVariant = AmClean { amWhatAchieved = "the action succeeded" }
            , afOutcomeTier = Success
            , afActionContext = setup.asPlayerAction
            }
      pure $ gotoChoice @"aftermath" aftermathSetup

-- ══════════════════════════════════════════════════════════════
-- AFTERMATH HANDLER
-- ══════════════════════════════════════════════════════════════

-- | Handle the aftermath phase (consequences)
aftermathHandler
  :: AftermathSetup
  -> Eff DMEffects (GotoChoice
      '[ To "sceneRouter" SceneSetup
       , To "trauma" TraumaSetup
       , To "downtime" DowntimeSetup
       , To "aftermath" AftermathSetup
       ])
aftermathHandler setup = do
  logInfo $ "[Aftermath] Processing: " <> T.pack (show setup.afVariant)

  state <- get @WorldState

  -- Check for trauma trigger (stress >= 9)
  if state.player.stress >= 9
    then do
      logInfo "[Aftermath] Stress at max, triggering trauma"
      let traumaSetup = TraumaSetup
            { tsVariant = Breaking
                { tvWhatBroke = "the pressure"
                , tvTraumaType = Trauma "pending"
                , tvTrigger = setup.afActionContext
                , tvAdrenaline = False
                }
            , tsTrigger = setup.afActionContext
            }
      pure $ gotoChoice @"trauma" traumaSetup
    else do
      -- Return to scene
      let sceneSetup = SceneSetup
            { ssVariant = Encounter "aftermath" UrgencyLow True
            , ssPlayerAction = ""
            , ssPreviousNarration = Just "The dust settles..."
            }
      pure $ gotoChoice @"sceneRouter" sceneSetup

-- ══════════════════════════════════════════════════════════════
-- BARGAIN HANDLER
-- ══════════════════════════════════════════════════════════════

-- | Handle the bargain phase (out of dice)
bargainHandler
  :: BargainSetup
  -> Eff DMEffects (GotoChoice
      '[ To "sceneRouter" SceneSetup
       , To "downtime" DowntimeSetup
       , To "trauma" TraumaSetup
       , To "bargain" BargainSetup
       ])
bargainHandler setup = do
  logInfo $ "[Bargain] Out of dice: " <> setup.bsVariant.bvWhatDrained

  -- TODO: Present bargain options via RequestInput
  -- Stub: assume player accepts a deal
  let sceneSetup = SceneSetup
        { ssVariant = Encounter "after bargain" UrgencyMedium True
        , ssPlayerAction = ""
        , ssPreviousNarration = Just "You make a deal..."
        }
  pure $ gotoChoice @"sceneRouter" sceneSetup

-- ══════════════════════════════════════════════════════════════
-- TRAUMA HANDLER
-- ══════════════════════════════════════════════════════════════

-- | Handle the trauma phase (breaking point)
traumaHandler
  :: TraumaSetup
  -> Eff DMEffects (GotoChoice
      '[ To "sceneRouter" SceneSetup
       , To Exit Response
       ])
traumaHandler setup = do
  logInfo $ "[Trauma] Breaking point: " <> setup.tsVariant.tvWhatBroke

  -- TODO: LLM narrates trauma, assigns permanent trauma type
  -- Reset stress and return to scene
  modify @WorldState $ \s -> s
    { player = s.player { stress = 0 }
    , mood = MoodScene (Encounter "after trauma" UrgencyLow True)
    }

  let sceneSetup = SceneSetup
        { ssVariant = Encounter "trauma aftermath" UrgencyLow True
        , ssPlayerAction = ""
        , ssPreviousNarration = Just "Something broke inside you..."
        }
  pure $ gotoChoice @"sceneRouter" sceneSetup

-- ══════════════════════════════════════════════════════════════
-- DOWNTIME HANDLER
-- ══════════════════════════════════════════════════════════════

-- | Handle the downtime phase (between scenes)
downtimeHandler
  :: DowntimeSetup
  -> Eff DMEffects (GotoChoice
      '[ To "sceneRouter" SceneSetup
       , To Exit Response
       ])
downtimeHandler setup = do
  logInfo $ "[Downtime] Between scenes: " <> T.pack (show setup.dsVariant)

  -- TODO: Present downtime options (lay low, recover, work goal, new scene, quit)
  -- Stub: start new scene
  let sceneSetup = SceneSetup
        { ssVariant = Encounter "new scene" UrgencyLow True
        , ssPlayerAction = ""
        , ssPreviousNarration = Just "Time passes in Doskvol..."
        }
  pure $ gotoChoice @"sceneRouter" sceneSetup
