{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | DM Graph Handlers
--
-- Handler implementations for each node in the DMGraph.
-- Each handler takes its Needs payload and returns a GotoChoice.
--
-- Architecture:
-- - Each handler builds DMContext from WorldState + payload
-- - Calls LLM with mood-specific template via runTurn
-- - Processes TurnOutput and applies to state
-- - Returns GotoChoice based on tool results (engage/resolve/accept)
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

import Tidepool.Effect (LLM, RequestInput, Log, Random, Emit, logInfo, logDebug, logWarn, runTurn, TurnOutcome(..), TurnResult(..))
import Tidepool.Template (Schema(..))
import Effectful.State.Static.Local (State, get, modify, put)
import Tidepool.Graph.Goto (GotoChoice, To, gotoChoice, gotoExit)
import Tidepool.Graph.Types (Exit)
import Tidepool.Graph.Generic (AsHandler)

import DM.State
import DM.Context (buildDMContext, DMContext(..))
import DM.Output (TurnOutput(..), applyTurnOutput)
import DM.Templates (renderForMood, turnOutputSchema)
import DM.Tools (DMEvent(..), dmTools)
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
--
-- This is the main scene handler. It:
-- 1. Sets mood to scene with encounter variant
-- 2. Builds DMContext from WorldState
-- 3. Calls LLM with scene template
-- 4. Processes output and checks for transitions (engage → action)
-- 5. Returns appropriate GotoChoice
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

  -- 1. Set mood to encounter variant
  modify @WorldState $ \s -> s { mood = MoodScene setup.ssVariant }

  -- 2. Build context from world state
  state <- get @WorldState
  let ctx = buildDMContext state
  logDebug $ "[SceneEncounter] Location: " <> ctx.ctxLocation.locationName
  logDebug $ "[SceneEncounter] Stakes: " <> ctx.ctxStakes

  -- 3. Render mood-specific template
  let systemPrompt = renderForMood state.mood ctx
  logDebug $ "[SceneEncounter] Prompt length: " <> T.pack (show $ T.length systemPrompt)

  -- 4. Call LLM with template, user action, schema, and tools
  let Schema{schemaJSON = outputSchema} = turnOutputSchema
  outcome <- runTurn @TurnOutput systemPrompt setup.ssPlayerAction outputSchema dmTools

  -- 5. Process outcome and return appropriate GotoChoice
  case outcome of
    -- Tool triggered a transition (engage → action)
    TurnBroken reason -> do
      logInfo $ "[SceneEncounter] Transition triggered: " <> reason
      handleSceneTransition setup reason

    -- Turn completed - process output
    TurnCompleted parseResult -> case parseResult of
      TurnParseFailed{..} -> do
        logWarn $ "[SceneEncounter] Parse failed: " <> T.pack tpfError
        -- Continue scene with fallback narration
        let response = Response
              { rNarration = if T.null tpfNarrative
                  then "*The world waits for your next move.*"
                  else tpfNarrative
              , rStressDelta = 0
              , rCoinDelta = 0
              , rSuggestedActions = ["Look around", "Wait", "Try something else"]
              }
        pure $ gotoExit response

      TurnParsed result -> do
        -- Apply output to state
        stateBefore <- get @WorldState
        modify @WorldState (applyTurnOutput result.trOutput)
        stateAfter <- get @WorldState

        -- Emit state change events
        emitStateChanges stateBefore stateAfter result.trOutput.narration

        -- Check if scene should continue or end
        if result.trOutput.continueScene
          then do
            let newSetup = setup { ssPreviousNarration = Just result.trOutput.narration }
            pure $ gotoChoice @"sceneRouter" newSetup
          else do
            -- Scene ends - transition to downtime
            let downtimeSetup = DowntimeSetup
                  { dsVariant = Recovery ["rest", "reflect"] "a moment"
                  , dsPreviousSceneSummary = T.take 100 result.trOutput.narration
                  }
            pure $ gotoChoice @"downtime" downtimeSetup

-- | Handle a transition triggered by a tool (engage/resolve/accept)
handleSceneTransition
  :: SceneSetup
  -> Text  -- Reason/tool name
  -> Eff DMEffects (GotoChoice
      '[ To "action" ActionSetup
       , To "downtime" DowntimeSetup
       , To "sceneRouter" SceneSetup
       , To Exit Response
       ])
handleSceneTransition setup reason = do
  -- Check what mood we're now in (tool modified state)
  state <- get @WorldState
  case state.mood of
    MoodAction av domain -> do
      let actionSetup = ActionSetup
            { asVariant = av
            , asDomain = domain
            , asPlayerAction = setup.ssPlayerAction
            , asContext = reason
            }
      pure $ gotoChoice @"action" actionSetup

    MoodDowntime dv -> do
      let downtimeSetup = DowntimeSetup
            { dsVariant = dv
            , dsPreviousSceneSummary = setup.ssPlayerAction
            }
      pure $ gotoChoice @"downtime" downtimeSetup

    -- Still in scene - continue
    _ -> do
      let newSetup = setup { ssPreviousNarration = Just reason }
      pure $ gotoChoice @"sceneRouter" newSetup

-- | Emit events for state changes (for GUI display)
emitStateChanges
  :: (Emit DMEvent :> es, Log :> es)
  => WorldState
  -> WorldState
  -> Text  -- Context/reason
  -> Eff es ()
emitStateChanges before after reason = do
  let pb = before.player
      pa = after.player

  when (pa.stress /= pb.stress) $
    emit $ StressChanged pb.stress pa.stress reason

  when (pa.heat /= pb.heat) $
    emit $ HeatChanged pb.heat pa.heat reason

  when (pa.wanted /= pb.wanted) $
    emit $ WantedChanged pb.wanted pa.wanted reason

  when (pa.coin /= pb.coin) $
    emit $ CoinChanged pb.coin pa.coin reason
  where
    when cond action = if cond then action else pure ()

-- | Handle an Opportunity scene
-- Uses same flow as Encounter but with Opportunity variant
sceneOpportunityHandler
  :: SceneSetup
  -> Eff DMEffects (GotoChoice
      '[ To "action" ActionSetup
       , To "downtime" DowntimeSetup
       , To "sceneRouter" SceneSetup
       , To Exit Response
       ])
sceneOpportunityHandler = runSceneHandler "Opportunity"

-- | Handle a Discovery scene
-- Uses same flow as Encounter but with Discovery variant
sceneDiscoveryHandler
  :: SceneSetup
  -> Eff DMEffects (GotoChoice
      '[ To "action" ActionSetup
       , To "downtime" DowntimeSetup
       , To "sceneRouter" SceneSetup
       , To Exit Response
       ])
sceneDiscoveryHandler = runSceneHandler "Discovery"

-- | Common scene handler logic
-- All scene variants (Encounter, Opportunity, Discovery) share the same flow
runSceneHandler
  :: Text  -- Variant name for logging
  -> SceneSetup
  -> Eff DMEffects (GotoChoice
      '[ To "action" ActionSetup
       , To "downtime" DowntimeSetup
       , To "sceneRouter" SceneSetup
       , To Exit Response
       ])
runSceneHandler variantName setup = do
  logInfo $ "[Scene" <> variantName <> "] Processing: " <> setup.ssPlayerAction

  -- Set mood to scene variant
  modify @WorldState $ \s -> s { mood = MoodScene setup.ssVariant }

  -- Build context and render template
  state <- get @WorldState
  let ctx = buildDMContext state
      systemPrompt = renderForMood state.mood ctx
      Schema{schemaJSON = outputSchema} = turnOutputSchema

  -- Call LLM
  outcome <- runTurn @TurnOutput systemPrompt setup.ssPlayerAction outputSchema dmTools

  -- Process outcome
  case outcome of
    TurnBroken reason -> do
      logInfo $ "[Scene" <> variantName <> "] Transition: " <> reason
      handleSceneTransition setup reason

    TurnCompleted parseResult -> case parseResult of
      TurnParseFailed{..} -> do
        logWarn $ "[Scene" <> variantName <> "] Parse failed: " <> T.pack tpfError
        let response = Response
              { rNarration = if T.null tpfNarrative
                  then "*The moment passes...*"
                  else tpfNarrative
              , rStressDelta = 0
              , rCoinDelta = 0
              , rSuggestedActions = ["Continue", "Look around"]
              }
        pure $ gotoExit response

      TurnParsed result -> do
        stateBefore <- get @WorldState
        modify @WorldState (applyTurnOutput result.trOutput)
        stateAfter <- get @WorldState
        emitStateChanges stateBefore stateAfter result.trOutput.narration

        if result.trOutput.continueScene
          then do
            let newSetup = setup { ssPreviousNarration = Just result.trOutput.narration }
            pure $ gotoChoice @"sceneRouter" newSetup
          else do
            let downtimeSetup = DowntimeSetup
                  { dsVariant = Recovery ["rest"] "a moment"
                  , dsPreviousSceneSummary = T.take 100 result.trOutput.narration
                  }
            pure $ gotoChoice @"downtime" downtimeSetup

-- ══════════════════════════════════════════════════════════════
-- ACTION HANDLER
-- ══════════════════════════════════════════════════════════════

-- | Handle the action phase (dice resolution)
--
-- The action phase is where risky actions are resolved via dice:
-- 1. LLM precommits outcomes for each die via spend_die tool
-- 2. Player chooses which die to use (sees hints, not full outcomes)
-- 3. Die is removed from pool, outcome revealed
-- 4. On TurnBroken (resolve), transition to aftermath
-- 5. If pool empties, SpendDie transitions to bargain
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

  -- Check if dice pool is empty BEFORE calling LLM
  if null state.dicePool.poolDice
    then do
      logInfo "[Action] Dice pool empty, transitioning to bargain"
      let isDesperateAction = case setup.asVariant of
            AvDesperate{} -> True
            _ -> False
          bargainSetup = BargainSetup
            { bsVariant = Bargaining
                { bvWhatDrained = setup.asContext
                , bvCanRetreat = not isDesperateAction
                , bvRetreatDesc = "slip away and regroup"
                , bvPassOutDesc = "collapse from exhaustion"
                , bvPreviousMood = MoodAction setup.asVariant setup.asDomain
                }
            , bsPlayerAction = setup.asPlayerAction
            }
      pure $ gotoChoice @"bargain" bargainSetup
    else do
      -- Set mood to action variant
      modify @WorldState $ \s -> s { mood = MoodAction setup.asVariant setup.asDomain }

      -- Build context and render template
      updatedState <- get @WorldState
      let ctx = buildDMContext updatedState
          systemPrompt = renderForMood updatedState.mood ctx
          Schema{schemaJSON = outputSchema} = turnOutputSchema

      logDebug $ "[Action] Pool size: " <> T.pack (show $ length updatedState.dicePool.poolDice)
      logDebug $ "[Action] Position: " <> T.pack (show setup.asVariant)

      -- Call LLM - will invoke spend_die for dice selection
      outcome <- runTurn @TurnOutput systemPrompt setup.asPlayerAction outputSchema dmTools

      -- Process outcome
      case outcome of
        -- Transition triggered (resolve → aftermath, or spend_die emptied pool → bargain)
        TurnBroken reason -> do
          logInfo $ "[Action] Transition: " <> reason
          handleActionTransition setup reason

        TurnCompleted parseResult -> case parseResult of
          TurnParseFailed{..} -> do
            logWarn $ "[Action] Parse failed: " <> T.pack tpfError
            -- On failure, retry action or go to aftermath with bad outcome
            let aftermathSetup = AftermathSetup
                  { afVariant = AmSetback
                      { amWhatWentWrong = "something went wrong"
                      , amImmediateDanger = False
                      , amEscapeRoute = "regroup"
                      }
                  , afOutcomeTier = Bad
                  , afActionContext = setup.asPlayerAction
                  }
            pure $ gotoChoice @"aftermath" aftermathSetup

          TurnParsed result -> do
            -- Apply output
            stateBefore <- get @WorldState
            modify @WorldState (applyTurnOutput result.trOutput)
            stateAfter <- get @WorldState
            emitStateChanges stateBefore stateAfter result.trOutput.narration

            -- Check post-turn state - if we're in bargain mood (SpendDie emptied pool)
            case stateAfter.mood of
              MoodBargain bv -> do
                let bargainSetup = BargainSetup
                      { bsVariant = bv
                      , bsPlayerAction = setup.asPlayerAction
                      }
                pure $ gotoChoice @"bargain" bargainSetup

              MoodAftermath av -> do
                let aftermathSetup = AftermathSetup
                      { afVariant = av
                      , afOutcomeTier = Success  -- Will be refined by resolve tool
                      , afActionContext = setup.asPlayerAction
                      }
                pure $ gotoChoice @"aftermath" aftermathSetup

              -- Still in action - continue (maybe more dice to spend)
              _ -> do
                let newSetup = setup { asContext = result.trOutput.narration }
                pure $ gotoChoice @"action" newSetup

-- | Handle transition from action phase
handleActionTransition
  :: ActionSetup
  -> Text  -- Reason
  -> Eff DMEffects (GotoChoice
      '[ To "aftermath" AftermathSetup
       , To "bargain" BargainSetup
       , To "action" ActionSetup
       ])
handleActionTransition setup reason = do
  state <- get @WorldState
  case state.mood of
    MoodAftermath av -> do
      let aftermathSetup = AftermathSetup
            { afVariant = av
            , afOutcomeTier = Success  -- Refined by resolve tool data
            , afActionContext = reason
            }
      pure $ gotoChoice @"aftermath" aftermathSetup

    MoodBargain bv -> do
      let bargainSetup = BargainSetup
            { bsVariant = bv
            , bsPlayerAction = setup.asPlayerAction
            }
      pure $ gotoChoice @"bargain" bargainSetup

    -- Still in action
    _ -> do
      let newSetup = setup { asContext = reason }
      pure $ gotoChoice @"action" newSetup

-- ══════════════════════════════════════════════════════════════
-- AFTERMATH HANDLER
-- ══════════════════════════════════════════════════════════════

-- | Handle the aftermath phase (consequences)
--
-- Aftermath is where the LLM narrates the outcome's consequences:
-- - Clean: smooth success
-- - Costly: achieved goal but paid a price
-- - Setback: partial or failed, complications arise
-- - Disaster: catastrophic failure, may trigger trauma
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

  -- Capture stress before turn for trauma detection
  stateBefore <- get @WorldState
  let stressBefore = stateBefore.player.stress

  -- Set mood to aftermath
  modify @WorldState $ \s -> s { mood = MoodAftermath setup.afVariant }

  -- Build context and call LLM
  state <- get @WorldState
  let ctx = buildDMContext state
      systemPrompt = renderForMood state.mood ctx
      Schema{schemaJSON = outputSchema} = turnOutputSchema

  outcome <- runTurn @TurnOutput systemPrompt setup.afActionContext outputSchema dmTools

  case outcome of
    TurnBroken reason -> do
      logInfo $ "[Aftermath] Transition: " <> reason
      handleAftermathTransition setup stressBefore reason

    TurnCompleted parseResult -> case parseResult of
      TurnParseFailed{..} -> do
        logWarn $ "[Aftermath] Parse failed: " <> T.pack tpfError
        -- Fallback: return to scene
        let sceneSetup = SceneSetup
              { ssVariant = Encounter "aftermath" UrgencyLow True
              , ssPlayerAction = ""
              , ssPreviousNarration = Just $ if T.null tpfNarrative
                  then "The dust settles..."
                  else tpfNarrative
              }
        pure $ gotoChoice @"sceneRouter" sceneSetup

      TurnParsed result -> do
        -- Apply output
        modify @WorldState (applyTurnOutput result.trOutput)
        stateAfter <- get @WorldState
        emitStateChanges stateBefore stateAfter result.trOutput.narration

        -- Check for trauma trigger (crossed from <9 to 9)
        if stateAfter.player.stress >= 9 && stressBefore < 9
          then do
            logInfo "[Aftermath] Stress hit max, triggering trauma"
            emit $ TraumaTriggered
              { ttTrauma = Trauma "pending"
              , ttTrigger = setup.afActionContext
              , ttBreakingPoint = "Something breaks inside you..."
              }
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
            -- Return to scene or end scene based on continueScene
            if result.trOutput.continueScene
              then do
                let sceneSetup = SceneSetup
                      { ssVariant = Encounter "aftermath" UrgencyLow True
                      , ssPlayerAction = ""
                      , ssPreviousNarration = Just result.trOutput.narration
                      }
                pure $ gotoChoice @"sceneRouter" sceneSetup
              else do
                let downtimeSetup = DowntimeSetup
                      { dsVariant = Recovery ["rest", "reflect"] "a moment"
                      , dsPreviousSceneSummary = T.take 100 result.trOutput.narration
                      }
                pure $ gotoChoice @"downtime" downtimeSetup

-- | Handle transition from aftermath (accept tool)
handleAftermathTransition
  :: AftermathSetup
  -> Int  -- Stress before
  -> Text
  -> Eff DMEffects (GotoChoice
      '[ To "sceneRouter" SceneSetup
       , To "trauma" TraumaSetup
       , To "downtime" DowntimeSetup
       , To "aftermath" AftermathSetup
       ])
handleAftermathTransition setup stressBefore reason = do
  state <- get @WorldState

  -- Check for trauma first
  if state.player.stress >= 9 && stressBefore < 9
    then do
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
      -- Return to scene (accept tool transitions back)
      let sceneSetup = SceneSetup
            { ssVariant = Encounter "continuing" UrgencyLow True
            , ssPlayerAction = ""
            , ssPreviousNarration = Just reason
            }
      pure $ gotoChoice @"sceneRouter" sceneSetup

-- ══════════════════════════════════════════════════════════════
-- BARGAIN HANDLER
-- ══════════════════════════════════════════════════════════════

-- | Handle the bargain phase (out of dice)
--
-- When dice pool is empty, player must choose:
-- - Accept a bargain (devil's deal for more dice)
-- - Retreat (if allowed, ends scene)
-- - Pass out (if retreat not allowed, wake up somewhere bad)
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

  -- Set mood to bargain
  modify @WorldState $ \s -> s { mood = MoodBargain setup.bsVariant }

  -- Build context and call LLM to generate bargain narration and options
  state <- get @WorldState
  let ctx = buildDMContext state
      systemPrompt = renderForMood state.mood ctx
      Schema{schemaJSON = outputSchema} = turnOutputSchema

  -- Emit that we're in bargain mode
  emit $ BargainOffered
    { boContext = setup.bsVariant.bvWhatDrained
    , boCanRetreat = setup.bsVariant.bvCanRetreat
    }

  -- Call LLM - will invoke accept_bargain, retreat, or pass_out
  outcome <- runTurn @TurnOutput systemPrompt setup.bsPlayerAction outputSchema dmTools

  case outcome of
    TurnBroken reason -> do
      logInfo $ "[Bargain] Transition: " <> reason
      handleBargainTransition setup reason

    TurnCompleted parseResult -> case parseResult of
      TurnParseFailed{..} -> do
        logWarn $ "[Bargain] Parse failed: " <> T.pack tpfError
        -- Fallback: force retreat if possible, else pass out
        if setup.bsVariant.bvCanRetreat
          then do
            let downtimeSetup = DowntimeSetup
                  { dsVariant = Recovery ["rest", "escape"] "a moment"
                  , dsPreviousSceneSummary = "slipped away"
                  }
            pure $ gotoChoice @"downtime" downtimeSetup
          else do
            let traumaSetup = TraumaSetup
                  { tsVariant = Breaking
                      { tvWhatBroke = "exhaustion"
                      , tvTraumaType = Trauma "pending"
                      , tvTrigger = setup.bsVariant.bvWhatDrained
                      , tvAdrenaline = False
                      }
                  , tsTrigger = "collapsed from exhaustion"
                  }
            pure $ gotoChoice @"trauma" traumaSetup

      TurnParsed result -> do
        -- Apply output
        stateBefore <- get @WorldState
        modify @WorldState (applyTurnOutput result.trOutput)
        stateAfter <- get @WorldState
        emitStateChanges stateBefore stateAfter result.trOutput.narration

        -- Check new mood (tools change mood)
        case stateAfter.mood of
          MoodScene sv -> do
            let sceneSetup = SceneSetup
                  { ssVariant = sv
                  , ssPlayerAction = ""
                  , ssPreviousNarration = Just result.trOutput.narration
                  }
            pure $ gotoChoice @"sceneRouter" sceneSetup

          MoodDowntime dv -> do
            let downtimeSetup = DowntimeSetup
                  { dsVariant = dv
                  , dsPreviousSceneSummary = result.trOutput.narration
                  }
            pure $ gotoChoice @"downtime" downtimeSetup

          -- Still in bargain - continue negotiating
          _ -> do
            let newSetup = setup
            pure $ gotoChoice @"bargain" newSetup

-- | Handle transitions from bargain
handleBargainTransition
  :: BargainSetup
  -> Text
  -> Eff DMEffects (GotoChoice
      '[ To "sceneRouter" SceneSetup
       , To "downtime" DowntimeSetup
       , To "trauma" TraumaSetup
       , To "bargain" BargainSetup
       ])
handleBargainTransition setup reason = do
  state <- get @WorldState
  case state.mood of
    MoodScene sv -> do
      let sceneSetup = SceneSetup
            { ssVariant = sv
            , ssPlayerAction = ""
            , ssPreviousNarration = Just reason
            }
      pure $ gotoChoice @"sceneRouter" sceneSetup

    MoodTrauma tv -> do
      let traumaSetup = TraumaSetup
            { tsVariant = tv
            , tsTrigger = reason
            }
      pure $ gotoChoice @"trauma" traumaSetup

    -- Retreat leads to downtime/between scenes
    MoodDowntime dv -> do
      let downtimeSetup = DowntimeSetup
            { dsVariant = dv
            , dsPreviousSceneSummary = reason
            }
      pure $ gotoChoice @"downtime" downtimeSetup

    -- Still in bargain
    _ -> pure $ gotoChoice @"bargain" setup

-- ══════════════════════════════════════════════════════════════
-- TRAUMA HANDLER
-- ══════════════════════════════════════════════════════════════

-- | Handle the trauma phase (breaking point)
--
-- Trauma happens when stress hits 9. The LLM narrates:
-- - The breaking point moment
-- - What trauma the character acquires
-- - How they might push through (adrenaline) or collapse
-- After trauma, stress resets to 0 and scene continues
traumaHandler
  :: TraumaSetup
  -> Eff DMEffects (GotoChoice
      '[ To "sceneRouter" SceneSetup
       , To Exit Response
       ])
traumaHandler setup = do
  logInfo $ "[Trauma] Breaking point: " <> setup.tsVariant.tvWhatBroke

  -- Set mood to trauma
  modify @WorldState $ \s -> s { mood = MoodTrauma setup.tsVariant }

  -- Build context and call LLM to narrate the breaking point
  state <- get @WorldState
  let ctx = buildDMContext state
      systemPrompt = renderForMood state.mood ctx
      Schema{schemaJSON = outputSchema} = turnOutputSchema

  outcome <- runTurn @TurnOutput systemPrompt setup.tsTrigger outputSchema dmTools

  case outcome of
    TurnBroken reason -> do
      logInfo $ "[Trauma] Transition: " <> reason
      -- Trauma always resets stress and returns to scene
      finishTrauma setup.tsVariant reason

    TurnCompleted parseResult -> case parseResult of
      TurnParseFailed{..} -> do
        logWarn $ "[Trauma] Parse failed: " <> T.pack tpfError
        finishTrauma setup.tsVariant (if T.null tpfNarrative
          then "Something breaks inside you..."
          else tpfNarrative)

      TurnParsed result -> do
        -- Apply output
        stateBefore <- get @WorldState
        modify @WorldState (applyTurnOutput result.trOutput)
        stateAfter <- get @WorldState
        emitStateChanges stateBefore stateAfter result.trOutput.narration

        finishTrauma setup.tsVariant result.trOutput.narration

-- | Finish trauma processing - reset stress and return to scene
finishTrauma
  :: TraumaVariant
  -> Text
  -> Eff DMEffects (GotoChoice
      '[ To "sceneRouter" SceneSetup
       , To Exit Response
       ])
finishTrauma variant narration = do
  -- Reset stress to 0 and add trauma to character
  modify @WorldState $ \s -> s
    { player = s.player
        { stress = 0
        , trauma = variant.tvTraumaType : s.player.trauma
        }
    , mood = MoodScene (Encounter "after trauma" UrgencyLow True)
    }

  -- Check adrenaline - can push through for one more action
  if variant.tvAdrenaline
    then do
      logInfo "[Trauma] Adrenaline surge - can push through"
      let sceneSetup = SceneSetup
            { ssVariant = Encounter "adrenaline surge" UrgencyHigh True
            , ssPlayerAction = ""
            , ssPreviousNarration = Just $ narration <> " But something keeps you moving..."
            }
      pure $ gotoChoice @"sceneRouter" sceneSetup
    else do
      -- Normal trauma - return to scene quietly
      let sceneSetup = SceneSetup
            { ssVariant = Encounter "after trauma" UrgencyLow True
            , ssPlayerAction = ""
            , ssPreviousNarration = Just narration
            }
      pure $ gotoChoice @"sceneRouter" sceneSetup

-- ══════════════════════════════════════════════════════════════
-- DOWNTIME HANDLER
-- ══════════════════════════════════════════════════════════════

-- | Handle the downtime phase (between scenes)
--
-- Downtime is the pause between scenes where:
-- - Threat clocks tick (time passes)
-- - Player can lay low (reduce heat)
-- - Player can recover (spend coin for stress)
-- - Player can work on goals (tick goal clocks)
-- - Player can start a new scene
-- - Player can end the session
downtimeHandler
  :: DowntimeSetup
  -> Eff DMEffects (GotoChoice
      '[ To "sceneRouter" SceneSetup
       , To Exit Response
       ])
downtimeHandler setup = do
  logInfo $ "[Downtime] Between scenes: " <> T.pack (show setup.dsVariant)

  -- Set mood to downtime
  modify @WorldState $ \s -> s { mood = MoodDowntime setup.dsVariant }

  -- Build context and call LLM for transition narration
  state <- get @WorldState
  let ctx = buildDMContext state
      systemPrompt = renderForMood state.mood ctx
      Schema{schemaJSON = outputSchema} = turnOutputSchema

  -- LLM generates transition narration and suggested actions
  outcome <- runTurn @TurnOutput systemPrompt setup.dsPreviousSceneSummary outputSchema dmTools

  case outcome of
    TurnBroken _reason -> do
      -- Downtime shouldn't break, but handle gracefully
      startNewScene "Time passes..."

    TurnCompleted parseResult -> case parseResult of
      TurnParseFailed{..} -> do
        logWarn $ "[Downtime] Parse failed: " <> T.pack tpfError
        startNewScene (if T.null tpfNarrative
          then "Time passes in Doskvol..."
          else tpfNarrative)

      TurnParsed result -> do
        -- Apply output
        stateBefore <- get @WorldState
        modify @WorldState (applyTurnOutput result.trOutput)
        stateAfter <- get @WorldState
        emitStateChanges stateBefore stateAfter result.trOutput.narration

        -- Check if session should end
        if result.trOutput.continueScene
          then startNewScene result.trOutput.narration
          else do
            -- Session ends
            let response = Response
                  { rNarration = result.trOutput.narration <> "\n\n*The eternal night of Doskvol awaits your return...*"
                  , rStressDelta = stateAfter.player.stress - stateBefore.player.stress
                  , rCoinDelta = stateAfter.player.coin - stateBefore.player.coin
                  , rSuggestedActions = []
                  }
            pure $ gotoExit response

-- | Start a new scene after downtime
startNewScene
  :: Text
  -> Eff DMEffects (GotoChoice
      '[ To "sceneRouter" SceneSetup
       , To Exit Response
       ])
startNewScene narration = do
  -- Refresh dice pool if needed
  state <- get @WorldState
  when (null state.dicePool.poolDice) $ do
    modify @WorldState $ \s -> s { dicePool = DicePool [4, 4, 4] }

  let sceneSetup = SceneSetup
        { ssVariant = Encounter "new scene" UrgencyLow True
        , ssPlayerAction = ""
        , ssPreviousNarration = Just narration
        }
  pure $ gotoChoice @"sceneRouter" sceneSetup
  where
    when cond action = if cond then action else pure ()
