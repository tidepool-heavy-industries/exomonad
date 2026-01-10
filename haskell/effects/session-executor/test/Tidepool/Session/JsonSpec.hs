-- | JSON parsing tests for Session types.
--
-- Verifies that SessionOutput, SessionMetadata, and InterruptSignal
-- correctly parse JSON from mantle CLI output.
module Tidepool.Session.JsonSpec (spec) where

import Data.Aeson (eitherDecodeStrict, encode)
import Data.ByteString.Lazy qualified as LBS
import Test.Hspec

import Tidepool.Effect.Session
  ( SessionId(..)
  , SessionOutput(..)
  , SessionMetadata(..)
  , InterruptSignal(..)
  )


spec :: Spec
spec = do
  describe "SessionOutput JSON" $ do
    it "parses a successful session result" $ do
      let json = "{\"session_id\":\"abc123\",\"branch\":\"feat-x\",\"worktree\":\"/path/to/wt\",\"exit_code\":0,\"is_error\":false,\"total_cost_usd\":0.05,\"num_turns\":3,\"interrupts\":[],\"duration_secs\":45.2}"
      case eitherDecodeStrict json of
        Left err -> expectationFailure $ "Parse failed: " <> err
        Right (output :: SessionOutput) -> do
          output.soSessionId.unSessionId `shouldBe` "abc123"
          output.soBranch `shouldBe` "feat-x"
          output.soWorktree `shouldBe` "/path/to/wt"
          output.soExitCode `shouldBe` 0
          output.soIsError `shouldBe` False
          output.soTotalCostUsd `shouldBe` 0.05
          output.soNumTurns `shouldBe` 3
          output.soInterrupts `shouldBe` []
          output.soDurationSecs `shouldBe` 45.2

    it "parses a result with error" $ do
      let json = "{\"session_id\":\"\",\"branch\":\"\",\"worktree\":\"\",\"exit_code\":1,\"is_error\":true,\"total_cost_usd\":0,\"num_turns\":0,\"interrupts\":[],\"duration_secs\":0,\"error\":\"Container failed to start\"}"
      case eitherDecodeStrict json of
        Left err -> expectationFailure $ "Parse failed: " <> err
        Right (output :: SessionOutput) -> do
          output.soIsError `shouldBe` True
          output.soError `shouldBe` Just "Container failed to start"

    it "parses a result with fork interrupt" $ do
      let json = "{\"session_id\":\"parent123\",\"branch\":\"main\",\"worktree\":\"/wt\",\"exit_code\":0,\"is_error\":false,\"total_cost_usd\":0.1,\"num_turns\":5,\"interrupts\":[{\"signal_type\":\"fork\",\"state\":\"feat-child\",\"reason\":\"Implement child feature\"}],\"duration_secs\":120}"
      case eitherDecodeStrict json of
        Left err -> expectationFailure $ "Parse failed: " <> err
        Right (output :: SessionOutput) -> do
          length output.soInterrupts `shouldBe` 1
          let [sig] = output.soInterrupts
          sig.isSignalType `shouldBe` "fork"
          sig.isState `shouldBe` Just "feat-child"
          sig.isReason `shouldBe` Just "Implement child feature"

    it "round-trips through JSON" $ do
      let output = SessionOutput
            { soSessionId = SessionId "test-id"
            , soBranch = "test-branch"
            , soWorktree = "/test/path"
            , soExitCode = 0
            , soIsError = False
            , soResultText = Just "Success!"
            , soTotalCostUsd = 0.25
            , soNumTurns = 10
            , soInterrupts = []
            , soDurationSecs = 60.5
            , soError = Nothing
            }
      let json = LBS.toStrict $ encode output
      case eitherDecodeStrict json of
        Left err -> expectationFailure $ "Round-trip failed: " <> err
        Right (parsed :: SessionOutput) -> do
          parsed.soSessionId `shouldBe` output.soSessionId
          parsed.soBranch `shouldBe` output.soBranch
          parsed.soWorktree `shouldBe` output.soWorktree

  describe "InterruptSignal JSON" $ do
    it "parses fork signal" $ do
      let json = "{\"signal_type\":\"fork\",\"state\":\"child-branch\",\"reason\":\"Do the thing\"}"
      case eitherDecodeStrict json of
        Left err -> expectationFailure $ "Parse failed: " <> err
        Right (sig :: InterruptSignal) -> do
          sig.isSignalType `shouldBe` "fork"
          sig.isState `shouldBe` Just "child-branch"
          sig.isReason `shouldBe` Just "Do the thing"

    it "parses escalate signal without state" $ do
      let json = "{\"signal_type\":\"escalate\",\"reason\":\"Need human input\"}"
      case eitherDecodeStrict json of
        Left err -> expectationFailure $ "Parse failed: " <> err
        Right (sig :: InterruptSignal) -> do
          sig.isSignalType `shouldBe` "escalate"
          sig.isState `shouldBe` Nothing
          sig.isReason `shouldBe` Just "Need human input"

    it "parses transition signal with state" $ do
      let json = "{\"signal_type\":\"transition\",\"state\":\"review\"}"
      case eitherDecodeStrict json of
        Left err -> expectationFailure $ "Parse failed: " <> err
        Right (sig :: InterruptSignal) -> do
          sig.isSignalType `shouldBe` "transition"
          sig.isState `shouldBe` Just "review"
          sig.isReason `shouldBe` Nothing

  describe "SessionMetadata JSON" $ do
    it "parses session info" $ do
      let json = "{\"session_id\":\"sess123\",\"branch\":\"feat-y\",\"worktree\":\"/wt/path\",\"status\":\"idle\",\"created_at\":\"2024-01-15T10:30:00Z\",\"updated_at\":\"2024-01-15T11:00:00Z\",\"last_exit_code\":0,\"total_cost_usd\":0.5,\"child_sessions\":[]}"
      case eitherDecodeStrict json of
        Left err -> expectationFailure $ "Parse failed: " <> err
        Right (meta :: SessionMetadata) -> do
          meta.smSessionId.unSessionId `shouldBe` "sess123"
          meta.smBranch `shouldBe` "feat-y"
          meta.smWorktree `shouldBe` "/wt/path"
          meta.smStatus `shouldBe` "idle"
          meta.smParentSession `shouldBe` Nothing
          meta.smChildSessions `shouldBe` []
          meta.smLastExitCode `shouldBe` 0
          meta.smTotalCostUsd `shouldBe` 0.5

    it "parses session with parent and children" $ do
      let json = "{\"session_id\":\"child1\",\"branch\":\"child\",\"worktree\":\"/wt\",\"parent_session\":\"parent123\",\"child_sessions\":[\"grandchild1\",\"grandchild2\"],\"status\":\"completed\",\"created_at\":\"2024-01-15T10:30:00Z\",\"updated_at\":\"2024-01-15T11:00:00Z\",\"last_exit_code\":0,\"total_cost_usd\":1.0}"
      case eitherDecodeStrict json of
        Left err -> expectationFailure $ "Parse failed: " <> err
        Right (meta :: SessionMetadata) -> do
          meta.smParentSession `shouldBe` Just (SessionId "parent123")
          length meta.smChildSessions `shouldBe` 2
