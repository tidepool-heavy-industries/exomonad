{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module ExoMonad.Control.Interpreters.Traced
  ( traceCabal
  , traceGit
  , traceLSP
  ) where

import Control.Monad.Freer (Eff, Member, LastMember, interpose, send, sendM)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import OpenTelemetry.Trace
import OpenTelemetry.Context.ThreadLocal (getContext)

import ExoMonad.Effects.Cabal (Cabal(..), CabalResult(..))
import ExoMonad.Effects.Git (Git(..), WorktreeInfo(..))
import ExoMonad.Effect.LSP (LSP(..), Position(..), TextDocumentIdentifier(..))

-- | Helper to wrap an action in a span.
-- Note: We don't propagate context thread-locally here because 'interpose' 
-- runs the action via 'send' which might switch threads in the interpreter.
-- This is sufficient for measuring the latency of the effect itself.
withSpan 
  :: (LastMember IO effs) 
  => Tracer 
  -> Text 
  -> (Span -> Eff effs a) 
  -> Eff effs a
withSpan tracer name action = do
  -- Get current context (from thread local) to link as parent
  ctx <- sendM getContext
  traceSpan <- sendM $ createSpan tracer ctx name defaultSpanArguments
  result <- action traceSpan
  sendM $ endSpan traceSpan Nothing
  pure result

-- | Trace Cabal effects
traceCabal 
  :: (Member Cabal effs, LastMember IO effs) 
  => Tracer 
  -> Eff effs a 
  -> Eff effs a
traceCabal tracer = interpose $ \case
  CabalBuild path -> withSpan tracer "cabal.build" $ \traceSpan -> do
    sendM $ addAttribute traceSpan "cabal.path" (T.pack path)
    res <- send (CabalBuild path)
    case res of
      CabalSuccess
        -> sendM $ addAttribute traceSpan "cabal.status" ("success" :: Text)
      CabalBuildFailure code stderr _
        -> do sendM
                $ addAttribute traceSpan "cabal.status" ("build_failure" :: Text)
              sendM
                $ addAttribute traceSpan "cabal.exit_code" (fromIntegral code :: Int)
              sendM $ addAttribute traceSpan "error.message" (T.take 1000 stderr)
      CabalTestFailure raw
        -> do sendM
                $ addAttribute traceSpan "cabal.status" ("test_failure" :: Text)
              sendM $ addAttribute traceSpan "error.message" (T.take 1000 raw)
      CabalTestSuccess _
        -> sendM $ addAttribute traceSpan "cabal.status" ("success" :: Text)
      CabalInfraError err -> do
        sendM $ addAttribute traceSpan "cabal.status" ("infra_error" :: Text)
        sendM $ addAttribute traceSpan "error.message" (T.take 1000 err)
    pure res

  CabalTest path -> withSpan tracer "cabal.test" $ \traceSpan -> do
    sendM $ addAttribute traceSpan "cabal.path" (T.pack path)
    res <- send (CabalTest path)
    case res of
      CabalSuccess -> 
        sendM $ addAttribute traceSpan "cabal.status" ("success" :: Text)
      CabalTestSuccess _ -> 
        sendM $ addAttribute traceSpan "cabal.status" ("success" :: Text)
      CabalBuildFailure code stderr _ -> do
        sendM $ addAttribute traceSpan "cabal.status" ("build_failure" :: Text)
        sendM $ addAttribute traceSpan "cabal.exit_code" (fromIntegral code :: Int)
        sendM $ addAttribute traceSpan "error.message" (T.take 1000 stderr)
      CabalTestFailure raw -> do
        sendM $ addAttribute traceSpan "cabal.status" ("test_failure" :: Text)
        sendM $ addAttribute traceSpan "error.message" (T.take 1000 raw)
      CabalInfraError err -> do
        sendM $ addAttribute traceSpan "cabal.status" ("infra_error" :: Text)
        sendM $ addAttribute traceSpan "error.message" (T.take 1000 err)
    pure res

  CabalClean path -> withSpan tracer "cabal.clean" $ \traceSpan -> do
    sendM $ addAttribute traceSpan "cabal.path" (T.pack path)
    res <- send (CabalClean path)
    pure res

-- | Trace Git effects
traceGit 
  :: (Member Git effs, LastMember IO effs) 
  => Tracer 
  -> Eff effs a 
  -> Eff effs a
traceGit tracer = interpose $ \case
  GetWorktreeInfo -> withSpan tracer "git.worktree_info" $ \traceSpan -> do
    res <- send GetWorktreeInfo
    sendM $ addAttribute traceSpan "git.found" (isJust res)
    case res of
      Just info -> do
        sendM $ addAttribute traceSpan "git.branch" info.wiBranch
        sendM $ addAttribute traceSpan "git.is_worktree" info.wiIsWorktree
      Nothing -> pure ()
    pure res

  GetDirtyFiles -> withSpan tracer "git.dirty_files" $ \traceSpan -> do
    res <- send GetDirtyFiles
    sendM $ addAttribute traceSpan "git.dirty_count" (length res)
    pure res

  GetRecentCommits n -> withSpan tracer "git.recent_commits" $ \traceSpan -> do
    sendM $ addAttribute traceSpan "git.limit" (fromIntegral n :: Int)
    res <- send (GetRecentCommits n)
    sendM $ addAttribute traceSpan "git.count" (length res)
    pure res

  GetCurrentBranch -> withSpan tracer "git.current_branch" $ \traceSpan -> do
    res <- send GetCurrentBranch
    sendM $ addAttribute traceSpan "git.branch" res
    pure res

  GetCommitsAhead ref -> withSpan tracer "git.commits_ahead" $ \traceSpan -> do
    sendM $ addAttribute traceSpan "git.ref" ref
    res <- send (GetCommitsAhead ref)
    sendM $ addAttribute traceSpan "git.ahead_count" (fromIntegral res :: Int)
    pure res

-- | Trace LSP effects
traceLSP 
  :: (Member LSP effs, LastMember IO effs) 
  => Tracer 
  -> Eff effs a 
  -> Eff effs a
traceLSP tracer = interpose $ \case
  Hover doc pos -> withSpan tracer "lsp.hover" $ \traceSpan -> do
    sendM $ addAttribute traceSpan "lsp.file" doc.tdiUri
    sendM $ addAttribute traceSpan "lsp.line" (fromIntegral $ pos.posLine :: Int)
    res <- send (Hover doc pos)
    sendM $ addAttribute traceSpan "lsp.found" (isJust res)
    pure res

  References doc pos -> withSpan tracer "lsp.references" $ \traceSpan -> do
    sendM $ addAttribute traceSpan "lsp.file" doc.tdiUri
    res <- send (References doc pos)
    sendM $ addAttribute traceSpan "lsp.count" (length res)
    pure res

  Diagnostics doc -> withSpan tracer "lsp.diagnostics" $ \traceSpan -> do
    sendM $ addAttribute traceSpan "lsp.file" doc.tdiUri
    res <- send (Diagnostics doc)
    sendM $ addAttribute traceSpan "lsp.count" (length res)
    pure res

  -- Fallback
  op -> withSpan tracer "lsp.op" $ \_ -> send op
