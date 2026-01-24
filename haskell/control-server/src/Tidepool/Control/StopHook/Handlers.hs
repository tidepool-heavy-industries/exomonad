{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Tidepool.Control.StopHook.Handlers
  ( stopHookHandlers
  ) where

import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.State (State, get, modify)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Tidepool.Graph.Generic (AsHandler, (:-))
import Tidepool.Graph.Goto (GotoChoice, To, gotoChoice, gotoExit)
import Tidepool.Graph.Types (Exit)
import Tidepool.Control.StopHook.Types
import Tidepool.Control.StopHook.Graph
import Tidepool.Control.StopHook.ErrorParser (parseGHCOutput)
import Tidepool.Effects.Cabal (Cabal, CabalResult(..), cabalBuild, RawCompileError(..))

stopHookHandlers
  :: ( Member (State WorkflowState) es
     , Member Cabal es
     )
  => StopHookGraph (AsHandler es)
stopHookHandlers = StopHookGraph
  { entry = ()
  , globalLoopCheck = handleGlobalLoopCheck
  , globalMaxReached = handleGlobalMaxReached
  , checkBuild = handleCheckBuild
  , routeBuild = handleRouteBuild
  , buildLoopCheck = handleBuildLoopCheck
  , buildMaxReached = handleBuildMaxReached
  , buildContext = handleBuildContext
  , stubNextStage = handleStubNextStage
  , exit = ()
  }

-- | Global loop check (circuit breaker integration)
handleGlobalLoopCheck
  :: (Member (State WorkflowState) es)
  => AgentState
  -> Eff es (GotoChoice '[To "globalMaxReached" (), To "checkBuild" AgentState])
handleGlobalLoopCheck state = do
  ws <- get
  if wsGlobalStops ws >= 15
    then pure $ gotoChoice @"globalMaxReached" ()
    else do
      modify $ \s -> s { wsGlobalStops = wsGlobalStops s + 1 }
      pure $ gotoChoice @"checkBuild" state

-- | Global max reached: exit with max-loops template
handleGlobalMaxReached
  :: (Member (State WorkflowState) es)
  => ()
  -> Eff es (GotoChoice '[To Exit (TemplateName, StopHookContext)])
handleGlobalMaxReached () = do
  ws <- get
  let context = buildTemplateContext ws "max-loops"
  pure $ gotoExit ("max-loops" :: Text, context)

-- | Check build status
handleCheckBuild
  :: (Member Cabal es, Member (State WorkflowState) es)
  => AgentState
  -> Eff es (GotoChoice '[To "routeBuild" (AgentState, BuildResult)])
handleCheckBuild state = do
  cabalRes <- cabalBuild (asCwd state)
  let buildRes = translateCabalResult cabalRes
  modify $ \s -> s
    { wsLastBuildResult = Just buildRes
    , wsCurrentStage = StageBuild
    }
  pure $ gotoChoice @"routeBuild" (state, buildRes)

translateCabalResult :: CabalResult -> BuildResult
translateCabalResult CabalSuccess = BuildSuccess
translateCabalResult (CabalBuildFailure _code stderr _stdout parsed) = 
  let errors = map translateRawError parsed
      allErrors = parseGHCOutput stderr
      finalErrors = if null allErrors then errors else allErrors
      (errs, warns) = span (\e -> geSeverity e == ErrorSeverity) finalErrors
  in BuildFailure $ BuildFailureInfo
    { bfiRawOutput = stderr
    , bfiErrors = errs
    , bfiWarnings = warns
    , bfiErrorCount = length errs
    , bfiWarningCount = length warns
    }
translateCabalResult (CabalTestFailure _ _raw) = BuildSuccess
translateCabalResult (CabalTestSuccess _) = BuildSuccess

translateRawError :: RawCompileError -> GHCError
translateRawError rce = GHCError
  { geFile = rce.rceFile
  , geLine = rce.rceLine
  , geColumn = 0
  , geMessage = rce.rceMessage
  , geErrorType = OtherError (rce.rceMessage)
  , geSeverity = ErrorSeverity
  }

-- | Route based on build result
handleRouteBuild
  :: (Member (State WorkflowState) es)
  => (AgentState, BuildResult)
  -> Eff es (GotoChoice '[To "buildContext" TemplateName, To "buildLoopCheck" AgentState])
handleRouteBuild (state, result) = case result of
  BuildSuccess -> do
    pure $ gotoChoice @"buildLoopCheck" state
  BuildFailure _info -> do
    pure $ gotoChoice @"buildContext" ("fix-build-errors" :: Text)

-- | Check build-specific loop count
handleBuildLoopCheck
  :: (Member (State WorkflowState) es)
  => AgentState
  -> Eff es (GotoChoice '[To "buildMaxReached" (), To "stubNextStage" AgentState])
handleBuildLoopCheck state = do
  ws <- get
  let buildRetries = fromMaybe 0 $ Map.lookup StageBuild (wsStageRetries ws)
  if buildRetries >= 5
    then pure $ gotoChoice @"buildMaxReached" ()
    else do
      modify $ \s -> s
        { wsStageRetries = Map.insertWith (+) StageBuild 1 (wsStageRetries s)
        }
      pure $ gotoChoice @"stubNextStage" state

-- | Build max reached: go to buildContext with build-stuck template
handleBuildMaxReached
  :: (Member (State WorkflowState) es)
  => ()
  -> Eff es (GotoChoice '[To "buildContext" TemplateName])
handleBuildMaxReached () = do
  pure $ gotoChoice @"buildContext" ("build-stuck" :: Text)

-- | Build context for template rendering
handleBuildContext
  :: (Member (State WorkflowState) es)
  => TemplateName
  -> Eff es (GotoChoice '[To Exit (TemplateName, StopHookContext)])
handleBuildContext templateName = do
  ws <- get
  let context = buildTemplateContext ws templateName
  pure $ gotoExit (templateName, context)

-- | Stub for next stage
handleStubNextStage
  :: (Member (State WorkflowState) es)
  => AgentState
  -> Eff es (GotoChoice '[To Exit (TemplateName, StopHookContext)])
handleStubNextStage _state = do
  ws <- get
  let context = buildTemplateContext ws "next-stage-stub"
  pure $ gotoExit ("next-stage-stub" :: Text, context)

buildTemplateContext :: WorkflowState -> TemplateName -> StopHookContext
buildTemplateContext ws templateName = 
  let mRes = wsLastBuildResult ws
      (errs, warns, count, raw, failed) = case mRes of
        Just (BuildFailure info) -> 
          (bfiErrors info, bfiWarnings info, bfiErrorCount info, bfiRawOutput info, True)
        Just BuildSuccess -> 
          ([], [], 0, "", False)
        Nothing -> 
          ([], [], 0, "", False)
  in StopHookContext
    { template = templateName
    , stage = T.pack $ show (wsCurrentStage ws)
    , global_stops = wsGlobalStops ws
    , stage_retries = fromMaybe 0 (Map.lookup (wsCurrentStage ws) (wsStageRetries ws))
    , build_failed = failed
    , errors = errs
    , warnings = warns
    , error_count = count
    , raw_output = raw
    }
