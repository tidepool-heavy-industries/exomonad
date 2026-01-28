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

import Control.Monad (when)
import Control.Monad.Freer (Eff, Member, LastMember, interpose, send, sendM)
import Data.Maybe (isJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import OpenTelemetry.Trace
import OpenTelemetry.Context (Context)
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
  span <- sendM $ createSpan tracer ctx name defaultSpanArguments
  result <- action span
  sendM $ endSpan span Nothing
  pure result

-- | Trace Cabal effects
traceCabal 
  :: (Member Cabal effs, LastMember IO effs) 
  => Tracer 
  -> Eff effs a 
  -> Eff effs a
traceCabal tracer = interpose $ \case
  CabalBuild path -> withSpan tracer "cabal.build" $ \span -> do
    sendM $ addAttribute span "cabal.path" (T.pack path)
    res <- send (CabalBuild path)
    case res of
      CabalSuccess
        -> sendM $ addAttribute span "cabal.status" ("success" :: Text)
      CabalBuildFailure code stderr _
        -> do sendM
                $ addAttribute span "cabal.status" ("build_failure" :: Text)
              sendM
                $ addAttribute span "cabal.exit_code" (fromIntegral code :: Int)
              sendM $ addAttribute span "error.message" (T.take 1000 stderr)
      CabalTestFailure raw
        -> do sendM
                $ addAttribute span "cabal.status" ("test_failure" :: Text)
              sendM $ addAttribute span "error.message" (T.take 1000 raw)
      CabalTestSuccess _
        -> sendM $ addAttribute span "cabal.status" ("success" :: Text)
    pure res

  CabalTest path -> withSpan tracer "cabal.test" $ \span -> do
    sendM $ addAttribute span "cabal.path" (T.pack path)
    res <- send (CabalTest path)
    case res of
      CabalSuccess -> 
        sendM $ addAttribute span "cabal.status" ("success" :: Text)
      CabalTestSuccess _ -> 
        sendM $ addAttribute span "cabal.status" ("success" :: Text)
      CabalBuildFailure code stderr _ -> do
        sendM $ addAttribute span "cabal.status" ("build_failure" :: Text)
        sendM $ addAttribute span "cabal.exit_code" (fromIntegral code :: Int)
        sendM $ addAttribute span "error.message" (T.take 1000 stderr)
      CabalTestFailure raw -> do
        sendM $ addAttribute span "cabal.status" ("test_failure" :: Text)
        sendM $ addAttribute span "error.message" (T.take 1000 raw)
    pure res

  CabalClean path -> withSpan tracer "cabal.clean" $ \span -> do
    sendM $ addAttribute span "cabal.path" (T.pack path)
    res <- send (CabalClean path)
    pure res

-- | Trace Git effects
traceGit 
  :: (Member Git effs, LastMember IO effs) 
  => Tracer 
  -> Eff effs a 
  -> Eff effs a
traceGit tracer = interpose $ \case
  GetWorktreeInfo -> withSpan tracer "git.worktree_info" $ \span -> do
    res <- send GetWorktreeInfo
    sendM $ addAttribute span "git.found" (isJust res)
    case res of
      Just info -> do
        sendM $ addAttribute span "git.branch" info.wiBranch
        sendM $ addAttribute span "git.is_worktree" info.wiIsWorktree
      Nothing -> pure ()
    pure res

  GetDirtyFiles -> withSpan tracer "git.dirty_files" $ \span -> do
    res <- send GetDirtyFiles
    sendM $ addAttribute span "git.dirty_count" (length res)
    pure res

  GetRecentCommits n -> withSpan tracer "git.recent_commits" $ \span -> do
    sendM $ addAttribute span "git.limit" (fromIntegral n :: Int)
    res <- send (GetRecentCommits n)
    sendM $ addAttribute span "git.count" (length res)
    pure res

  GetCurrentBranch -> withSpan tracer "git.current_branch" $ \span -> do
    res <- send GetCurrentBranch
    sendM $ addAttribute span "git.branch" res
    pure res

  GetCommitsAhead ref -> withSpan tracer "git.commits_ahead" $ \span -> do
    sendM $ addAttribute span "git.ref" ref
    res <- send (GetCommitsAhead ref)
    sendM $ addAttribute span "git.ahead_count" (fromIntegral res :: Int)
    pure res

-- | Trace LSP effects
traceLSP 
  :: (Member LSP effs, LastMember IO effs) 
  => Tracer 
  -> Eff effs a 
  -> Eff effs a
traceLSP tracer = interpose $ \case
  Hover doc pos -> withSpan tracer "lsp.hover" $ \span -> do
    sendM $ addAttribute span "lsp.file" doc.tdiUri
    sendM $ addAttribute span "lsp.line" (fromIntegral $ pos.posLine :: Int)
    res <- send (Hover doc pos)
    sendM $ addAttribute span "lsp.found" (isJust res)
    pure res

  References doc pos -> withSpan tracer "lsp.references" $ \span -> do
    sendM $ addAttribute span "lsp.file" doc.tdiUri
    res <- send (References doc pos)
    sendM $ addAttribute span "lsp.count" (length res)
    pure res

  Diagnostics doc -> withSpan tracer "lsp.diagnostics" $ \span -> do
    sendM $ addAttribute span "lsp.file" doc.tdiUri
    res <- send (Diagnostics doc)
    sendM $ addAttribute span "lsp.count" (length res)
    pure res

  -- Fallback
  op -> withSpan tracer "lsp.op" $ \_ -> send op
