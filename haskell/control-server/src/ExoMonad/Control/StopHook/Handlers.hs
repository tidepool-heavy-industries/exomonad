{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module ExoMonad.Control.StopHook.Handlers
  ( stopHookHandlers,
  )
where

import Control.Lens (at, non, (&), (+~), (.~), (^.))
import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.State (State, get, modify)
import Data.Aeson (eitherDecodeStrict)
import Data.ByteString.Char8 qualified as BS8
import Data.Generics.Labels ()
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Control.StopHook.Graph
import ExoMonad.Control.StopHook.Types
import ExoMonad.Effects.Cabal (Cabal, CabalResult (..), cabalBuild, cabalTest)
import ExoMonad.Effects.Effector (Effector, GhPrStatusResult (..), GitStatusResult (..), effectorGitLsFiles, effectorGitStatus, runEffector)
import ExoMonad.Graph.Generic (AsHandler, (:-))
import ExoMonad.Graph.Goto (GotoChoice, To, gotoChoice, gotoExit)
import ExoMonad.Graph.Types (Exit)
import GHC.Generics (Generic)

stopHookHandlers ::
  ( Member (State WorkflowState) es,
    Member Cabal es,
    Member Effector es
  ) =>
  StopHookGraph (AsHandler es)
stopHookHandlers =
  StopHookGraph
    { entry = (),
      globalLoopCheck = handleGlobalLoopCheck,
      globalMaxReached = handleGlobalMaxReached,
      checkBuild = handleCheckBuild,
      routeBuild = handleRouteBuild,
      buildLoopCheck = handleBuildLoopCheck,
      buildMaxReached = handleBuildMaxReached,
      checkTest = handleCheckTest,
      routeTest = handleRouteTest,
      testLoopCheck = handleTestLoopCheck,
      testMaxReached = handleTestMaxReached,
      checkDocs = handleCheckDocs,
      checkPR = handleCheckPR,
      routePR = handleRoutePR,
      prLoopCheck = handlePrLoopCheck,
      prMaxReached = handlePrMaxReached,
      buildContext = handleBuildContext,
      exit = ()
    }

-- | Global loop check (circuit breaker integration)
handleGlobalLoopCheck ::
  (Member (State WorkflowState) es) =>
  AgentState ->
  Eff es (GotoChoice '[To "globalMaxReached" AgentState, To "checkBuild" AgentState])
handleGlobalLoopCheck state = do
  (ws :: WorkflowState) <- get
  if ws.globalStops >= 15
    then pure $ gotoChoice @"globalMaxReached" state
    else do
      modify @WorkflowState $ \s -> s & #globalStops +~ 1
      pure $ gotoChoice @"checkBuild" state

-- | Global max reached: exit with max-loops template
handleGlobalMaxReached ::
  (Member (State WorkflowState) es) =>
  AgentState ->
  Eff es (GotoChoice '[To Exit (TemplateName, StopHookContext)])
handleGlobalMaxReached state = do
  (ws :: WorkflowState) <- get
  let context = buildTemplateContext state ws "max-loops"
  pure $ gotoExit ("max-loops" :: Text, context)

-- | Check build status
handleCheckBuild ::
  (Member Cabal es, Member (State WorkflowState) es) =>
  AgentState ->
  Eff es (GotoChoice '[To "routeBuild" (AgentState, BuildResult)])
handleCheckBuild state = do
  cabalRes <- cabalBuild (state.cwd)
  let buildRes = translateCabalResult cabalRes
  modify @WorkflowState $ \s ->
    s
      & #lastBuildResult .~ Just buildRes
      & #currentStage .~ StageBuild
  pure $ gotoChoice @"routeBuild" (state, buildRes)

translateCabalResult :: CabalResult -> BuildResult
translateCabalResult CabalSuccess = BuildSuccess
translateCabalResult (CabalBuildFailure _code stderr _stdout) =
  BuildFailure $
    BuildFailureInfo
      { rawOutput = stderr
      }
translateCabalResult (CabalInfraError err) =
  BuildFailure $
    BuildFailureInfo
      { rawOutput = "Cabal infrastructure error: " <> err
      }
translateCabalResult (CabalTestFailure _raw) = BuildSuccess
translateCabalResult (CabalTestSuccess _) = BuildSuccess

-- | Route based on build result
handleRouteBuild ::
  (Member (State WorkflowState) es) =>
  (AgentState, BuildResult) ->
  Eff es (GotoChoice '[To "buildContext" (AgentState, TemplateName), To "buildLoopCheck" AgentState])
handleRouteBuild (state, result) = case result of
  BuildSuccess -> do
    pure $ gotoChoice @"buildLoopCheck" state
  BuildFailure _info -> do
    pure $ gotoChoice @"buildContext" (state, "fix-build-errors" :: Text)

-- | Check build-specific loop count
handleBuildLoopCheck ::
  (Member (State WorkflowState) es) =>
  AgentState ->
  Eff es (GotoChoice '[To "buildMaxReached" AgentState, To "checkTest" AgentState])
handleBuildLoopCheck state = do
  (ws :: WorkflowState) <- get
  let buildRetries = ws.stageRetries ^. at StageBuild . non 0
  if buildRetries >= 5
    then pure $ gotoChoice @"buildMaxReached" state
    else do
      modify @WorkflowState $ \s -> s & #stageRetries . at StageBuild . non 0 +~ 1
      pure $ gotoChoice @"checkTest" state

-- | Build max reached: go to buildContext with build-stuck template
handleBuildMaxReached ::
  (Member (State WorkflowState) es) =>
  AgentState ->
  Eff es (GotoChoice '[To "buildContext" (AgentState, TemplateName)])
handleBuildMaxReached state = do
  pure $ gotoChoice @"buildContext" (state, "build-stuck" :: Text)

-- | Check test status
handleCheckTest ::
  (Member Cabal es, Member (State WorkflowState) es) =>
  AgentState ->
  Eff es (GotoChoice '[To "routeTest" (AgentState, TestResult)])
handleCheckTest state = do
  cabalRes <- cabalTest (state.cwd)
  let testRes = translateTestResult cabalRes
  modify @WorkflowState $ \s ->
    s
      & #lastTestResult .~ Just testRes
      & #currentStage .~ StageTest
  pure $ gotoChoice @"routeTest" (state, testRes)

translateTestResult :: CabalResult -> TestResult
translateTestResult (CabalTestSuccess output) =
  TestResult 0 0 output
translateTestResult (CabalTestFailure raw) =
  TestResult 0 1 raw -- Capture raw output for template
translateTestResult (CabalInfraError err) =
  TestResult 0 1 ("Cabal infrastructure error: " <> err)
-- Defensive case: should be unreachable if checkBuild handled build failures correctly.
translateTestResult (CabalBuildFailure _ _ _) =
  error "translateTestResult: unexpected CabalBuildFailure (test called after build failure)"
translateTestResult CabalSuccess = TestResult 0 0 ""

-- | Route based on test result
handleRouteTest ::
  (Member (State WorkflowState) es) =>
  (AgentState, TestResult) ->
  Eff es (GotoChoice '[To "buildContext" (AgentState, TemplateName), To "testLoopCheck" AgentState])
handleRouteTest (state, result) = case result.failed of
  0 -> do
    pure $ gotoChoice @"testLoopCheck" state
  _ -> do
    pure $ gotoChoice @"buildContext" (state, "fix-test-failures" :: Text)

-- | Check test-specific loop count
handleTestLoopCheck ::
  (Member (State WorkflowState) es) =>
  AgentState ->
  Eff es (GotoChoice '[To "testMaxReached" AgentState, To "checkDocs" AgentState])
handleTestLoopCheck state = do
  (ws :: WorkflowState) <- get
  let testRetries = ws.stageRetries ^. at StageTest . non 0
  if testRetries >= 3
    then pure $ gotoChoice @"testMaxReached" state
    else do
      modify @WorkflowState $ \s -> s & #stageRetries . at StageTest . non 0 +~ 1
      pure $ gotoChoice @"checkDocs" state

-- | Test max reached
handleTestMaxReached ::
  (Member (State WorkflowState) es) =>
  AgentState ->
  Eff es (GotoChoice '[To "buildContext" (AgentState, TemplateName)])
handleTestMaxReached state = do
  pure $ gotoChoice @"buildContext" (state, "test-stuck" :: Text)

-- | Check documentation freshness
handleCheckDocs ::
  (Member Effector es, Member (State WorkflowState) es) =>
  AgentState ->
  Eff es (GotoChoice '[To Exit (TemplateName, StopHookContext), To "checkPR" AgentState])
handleCheckDocs state = do
  modify @WorkflowState $ \s -> s & #currentStage .~ StageDocs

  -- Get git status
  status <- effectorGitStatus (state.cwd)

  -- Get all CLAUDE.md files
  claudeFiles <- effectorGitLsFiles (state.cwd) ["**/CLAUDE.md"]

  let dirtyCode = filter (\f -> ".hs" `T.isSuffixOf` T.pack f || ".rs" `T.isSuffixOf` T.pack f) status.gsrDirty
      dirtyDocs = filter (\f -> "CLAUDE.md" `T.isSuffixOf` T.pack f) (status.gsrDirty ++ status.gsrStaged)

      docsStale = not (null dirtyCode) && null dirtyDocs

  if docsStale
    then do
      (ws :: WorkflowState) <- get
      let ctx =
            (buildTemplateContext state ws "update-docs")
              { git_dirty_files = status.gsrDirty,
                stale_docs = claudeFiles
              }
      pure $ gotoExit ("update-docs" :: Text, ctx)
    else do
      pure $ gotoChoice @"checkPR" state

-- | Check PR status
handleCheckPR ::
  (Member Effector es, Member (State WorkflowState) es) =>
  AgentState ->
  Eff es (GotoChoice '[To "routePR" (AgentState, GhPrStatusResult)])
handleCheckPR state = do
  let branchArgs = maybe [] (\b -> [b]) (state.branch)
  raw <- runEffector "gh" ("pr-status" : branchArgs)
  let mResult = eitherDecodeStrict (BS8.pack $ T.unpack raw)
  let result = case mResult of
        Left _err -> GhPrStatusResult False Nothing Nothing Nothing Nothing []
        Right res -> res
  modify @WorkflowState $ \s ->
    s
      & #lastPRStatus .~ Just result
      & #currentStage .~ (if result.exists then StageReview else StagePR)
  pure $ gotoChoice @"routePR" (state, result)

-- | Route based on PR status
handleRoutePR ::
  (Member (State WorkflowState) es) =>
  (AgentState, GhPrStatusResult) ->
  Eff es (GotoChoice '[To "buildContext" (AgentState, TemplateName), To "prLoopCheck" AgentState])
handleRoutePR (state, result) = do
  if not (result.exists)
    then pure $ gotoChoice @"buildContext" (state, "file-pr" :: Text)
    else
      if not (null (result.comments))
        then pure $ gotoChoice @"buildContext" (state, "address-review" :: Text)
        else pure $ gotoChoice @"prLoopCheck" state

-- | Check PR-specific loop count
handlePrLoopCheck ::
  (Member (State WorkflowState) es) =>
  AgentState ->
  Eff es (GotoChoice '[To "prMaxReached" AgentState, To Exit (TemplateName, StopHookContext)])
handlePrLoopCheck state = do
  (ws :: WorkflowState) <- get
  let prRetries = ws.stageRetries ^. at (ws.currentStage) . non 0
  if prRetries >= 3
    then pure $ gotoChoice @"prMaxReached" state
    else do
      modify @WorkflowState $ \s -> s & #stageRetries . at (s.currentStage) . non 0 +~ 1
      let context = buildTemplateContext state ws "complete"
      pure $ gotoExit ("complete" :: Text, context)

-- | PR max reached
handlePrMaxReached ::
  (Member (State WorkflowState) es) =>
  AgentState ->
  Eff es (GotoChoice '[To "buildContext" (AgentState, TemplateName)])
handlePrMaxReached state = do
  pure $ gotoChoice @"buildContext" (state, "pr-stuck" :: Text)

-- | Build context for template rendering
handleBuildContext ::
  (Member (State WorkflowState) es) =>
  (AgentState, TemplateName) ->
  Eff es (GotoChoice '[To Exit (TemplateName, StopHookContext)])
handleBuildContext (state, templateName) = do
  (ws :: WorkflowState) <- get
  let context = buildTemplateContext state ws templateName
  pure $ gotoExit (templateName, context)

buildTemplateContext :: AgentState -> WorkflowState -> TemplateName -> StopHookContext
buildTemplateContext as ws templateName =
  let mRes = ws.lastBuildResult
      (buildRaw, buildFailed) = case mRes of
        Just (BuildFailure info) ->
          (info.rawOutput, True)
        _ ->
          ("", False)
      mTestRes = ws.lastTestResult
      (tFailed, tPCount, tFCount, testRaw) = case mTestRes of
        Just tr -> (tr.failed > 0, tr.passed, tr.failed, tr.rawOutput)
        Nothing -> (False, 0, 0, "")
      -- Use test raw output if tests failed, otherwise build raw output
      raw = if tFailed then testRaw else buildRaw
      failed = buildFailed
      mPR = ws.lastPRStatus
      (prExists, prUrl, prNum, prStatus, prComments) = case mPR of
        Just pr ->
          (pr.exists, pr.url, pr.number, pr.review_status, pr.comments)
        Nothing ->
          (False, Nothing, Nothing, Nothing, [])
   in StopHookContext
        { template = templateName,
          stage = T.pack $ show (ws.currentStage),
          issue_number = as.issueNum,
          branch = fromMaybe "" as.branch,
          global_stops = ws.globalStops,
          stage_retries = ws.stageRetries ^. at (ws.currentStage) . non 0,
          build_failed = failed,
          raw_output = raw,
          tests_failed = tFailed,
          test_passed_count = tPCount,
          test_failed_count = tFCount,
          pr_exists = prExists,
          pr_url = prUrl,
          pr_number = prNum,
          pr_review_status = prStatus,
          pr_comments = prComments,
          stale_docs = [],
          git_dirty_files = []
        }
