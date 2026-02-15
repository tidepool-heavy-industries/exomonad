{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Wire format compatibility tests: JSON (proto3-suite ↔ prost+serde) and
-- protobuf binary (proto3-suite ↔ prost).
--
-- The binary tests use hex byte literals produced by the Rust test suite
-- (rust/exomonad-proto/tests/wire_compat.rs :: cross_language_hex_reference).
-- If a test fails here, it means the Haskell decoder is incompatible with
-- bytes that Rust actually produces — which is exactly the spawn_agents bug.
module Main where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Vector qualified as V
import Data.Word (Word8)
import Effects.Agent
  ( AgentInfo (..),
    SpawnBatchResponse (..),
    SpawnResponse (..),
  )
import Effects.EffectError
  ( Custom (..),
    EffectError (..),
    EffectErrorKind (..),
    NotFound (..),
  )
import Effects.Envelope
  ( EffectEnvelope (..),
    EffectResponse (..),
    EffectResponseResult (..),
  )
import Effects.Git (GetBranchResponse (..))
import Effects.Log (LogResponse (..))
import Proto3.Suite.Class (fromByteString, toLazyByteString)
import System.Exit (exitFailure, exitSuccess)

-- ============================================================================
-- Test harness
-- ============================================================================

data TestResult = Pass String | Fail String String

runTests :: [IO TestResult] -> IO ()
runTests tests = do
  results <- sequence tests
  let failures = [msg | Fail name msg <- results]
  let passes = [name | Pass name <- results]
  putStrLn ""
  putStrLn $ "=== " ++ show (length passes) ++ " passed, " ++ show (length failures) ++ " failed ==="
  mapM_ (\name -> putStrLn $ "  ✓ " ++ name) passes
  mapM_ (\msg -> putStrLn $ "  ✗ " ++ msg) failures
  if null failures then exitSuccess else exitFailure

assert :: String -> Bool -> String -> IO TestResult
assert name True _ = pure (Pass name)
assert name False msg = pure (Fail name (name ++ ": " ++ msg))

-- | Pack a list of Word8 into a strict ByteString (our hex literals).
pack :: [Word8] -> ByteString
pack = BS.pack

-- ============================================================================
-- Entry point
-- ============================================================================

main :: IO ()
main = do
  putStrLn "=== Cross-Language Wire Format Tests ==="
  putStrLn ""
  runTests
    [ -- Haskell-only roundtrips (sanity)
      testEncodeDecodeEffectEnvelope,
      testEncodeDecodeEffectResponsePayload,
      testEncodeDecodeEffectResponseError,
      -- Cross-language: decode Rust-produced bytes (EffectResponse envelope)
      testDecodeRustEffectResponsePayload,
      testDecodeRustEffectResponseSpawn,
      testDecodeRustEffectResponseError,
      testDecodeRustEffectEnvelope,
      -- Cross-language: decode Rust-produced inner types
      testDecodeRustGetBranchResponse,
      testDecodeRustLogResponse,
      testDecodeRustSpawnResponse,
      testDecodeRustSpawnBatchResponse,
      -- Cross-language: decode Rust-produced wrapped responses
      testDecodeRustWrappedGetBranch,
      testDecodeRustWrappedLogResponse,
      testDecodeRustWrappedSpawnBatch,
      -- Cross-language: error variants
      testDecodeRustCustomError,
      -- Cross-language: varint boundary (128-byte payload)
      testDecodeRustVarintBoundary128,
      -- Cross-language: large payload (reproduces production decode failure)
      testDecodeRustLargeSpawnBatchError,
      -- Symmetry: Haskell-encoded bytes match Rust reference
      testHaskellEncodingMatchesRust,
      testHaskellGetBranchEncodingMatchesRust,
      testHaskellLogResponseEncodingMatchesRust,
      -- Symmetry: error encoding matches Rust
      testHaskellNotFoundErrorEncodingMatchesRust,
      testHaskellCustomErrorEncodingMatchesRust,
      -- Edge case: empty payload in oneof
      testDecodeRustEmptyPayload,
      testHaskellEmptyPayloadMatchesRust
    ]

-- ============================================================================
-- Haskell-only roundtrip tests
-- ============================================================================

testEncodeDecodeEffectEnvelope :: IO TestResult
testEncodeDecodeEffectEnvelope = do
  let envelope =
        EffectEnvelope
          { effectEnvelopeEffectType = "git.get_branch",
            effectEnvelopePayload = pack [10, 1, 46]
          }
  let bytes = BL.toStrict (toLazyByteString envelope)
  case fromByteString bytes of
    Left err -> pure $ Fail "encode_decode_envelope" (show err)
    Right (decoded :: EffectEnvelope) ->
      assert
        "encode_decode_envelope"
        ( effectEnvelopeEffectType decoded == "git.get_branch"
            && effectEnvelopePayload decoded == pack [10, 1, 46]
        )
        "Fields did not roundtrip"

testEncodeDecodeEffectResponsePayload :: IO TestResult
testEncodeDecodeEffectResponsePayload = do
  let resp =
        EffectResponse
          { effectResponseResult = Just (EffectResponseResultPayload "test-data")
          }
  let bytes = BL.toStrict (toLazyByteString resp)
  case fromByteString bytes of
    Left err -> pure $ Fail "encode_decode_response_payload" (show err)
    Right (decoded :: EffectResponse) ->
      case effectResponseResult decoded of
        Just (EffectResponseResultPayload p) ->
          assert "encode_decode_response_payload" (p == "test-data") "Payload mismatch"
        other ->
          pure $ Fail "encode_decode_response_payload" ("Expected Payload, got: " ++ show other)

testEncodeDecodeEffectResponseError :: IO TestResult
testEncodeDecodeEffectResponseError = do
  let resp =
        EffectResponse
          { effectResponseResult =
              Just
                ( EffectResponseResultError
                    EffectError
                      { effectErrorKind =
                          Just (EffectErrorKindNotFound (NotFound {notFoundResource = "file/x"}))
                      }
                )
          }
  let bytes = BL.toStrict (toLazyByteString resp)
  case fromByteString bytes of
    Left err -> pure $ Fail "encode_decode_response_error" (show err)
    Right (decoded :: EffectResponse) ->
      case effectResponseResult decoded of
        Just (EffectResponseResultError e) ->
          case effectErrorKind e of
            Just (EffectErrorKindNotFound nf) ->
              assert "encode_decode_response_error" (notFoundResource nf == "file/x") "Resource mismatch"
            other ->
              pure $ Fail "encode_decode_response_error" ("Expected NotFound, got: " ++ show other)
        other ->
          pure $ Fail "encode_decode_response_error" ("Expected Error, got: " ++ show other)

-- ============================================================================
-- Cross-language: decode bytes produced by Rust (prost)
-- ============================================================================

-- Rust: EffectResponse { result: Payload(b"hello") }
-- Hex: [0x0a, 0x05, 0x68, 0x65, 0x6c, 0x6c, 0x6f]
testDecodeRustEffectResponsePayload :: IO TestResult
testDecodeRustEffectResponsePayload = do
  let rustBytes = pack [0x0a, 0x05, 0x68, 0x65, 0x6c, 0x6c, 0x6f]
  case fromByteString rustBytes of
    Left err ->
      pure $ Fail "decode_rust_response_payload" ("Decode failed: " ++ show err)
    Right (decoded :: EffectResponse) ->
      case effectResponseResult decoded of
        Just (EffectResponseResultPayload p) ->
          assert "decode_rust_response_payload" (p == "hello") ("Payload: " ++ show p)
        other ->
          pure $ Fail "decode_rust_response_payload" ("Expected Payload, got: " ++ show other)

-- Rust: EffectResponse { result: Payload(SpawnResponse { agent: AgentInfo { id:"a1", ... } }) }
-- Hex: [0x0a, 0x19, 0x0a, 0x17, ...]
testDecodeRustEffectResponseSpawn :: IO TestResult
testDecodeRustEffectResponseSpawn = do
  let rustBytes =
        pack
          [ 0x0a,
            0x19,
            0x0a,
            0x17,
            0x0a,
            0x02,
            0x61,
            0x31,
            0x12,
            0x01,
            0x31,
            0x1a,
            0x02,
            0x2f,
            0x77,
            0x22,
            0x01,
            0x62,
            0x28,
            0x01,
            0x30,
            0x01,
            0x38,
            0x01,
            0x42,
            0x01,
            0x74
          ]
  case fromByteString rustBytes of
    Left err ->
      pure $ Fail "decode_rust_response_spawn" ("Outer decode failed: " ++ show err)
    Right (decoded :: EffectResponse) ->
      case effectResponseResult decoded of
        Just (EffectResponseResultPayload innerBytes) ->
          -- The inner bytes are a SpawnResponse; verify they're non-empty
          -- (Full SpawnResponse decode tested separately via Haskell roundtrip)
          assert
            "decode_rust_response_spawn"
            (BS.length innerBytes == 25) -- 0x19 = 25
            ("Inner payload length: " ++ show (BS.length innerBytes) ++ ", expected 25")
        other ->
          pure $ Fail "decode_rust_response_spawn" ("Expected Payload, got: " ++ show other)

-- Rust: EffectResponse { result: Error(EffectError { kind: NotFound { resource: "test" } }) }
-- Hex: [0x12, 0x08, 0x0a, 0x06, 0x0a, 0x04, 0x74, 0x65, 0x73, 0x74]
testDecodeRustEffectResponseError :: IO TestResult
testDecodeRustEffectResponseError = do
  let rustBytes = pack [0x12, 0x08, 0x0a, 0x06, 0x0a, 0x04, 0x74, 0x65, 0x73, 0x74]
  case fromByteString rustBytes of
    Left err ->
      pure $ Fail "decode_rust_response_error" ("Decode failed: " ++ show err)
    Right (decoded :: EffectResponse) ->
      case effectResponseResult decoded of
        Just (EffectResponseResultError e) ->
          case effectErrorKind e of
            Just (EffectErrorKindNotFound nf) ->
              assert "decode_rust_response_error" (notFoundResource nf == "test") ("Resource: " ++ show (notFoundResource nf))
            other ->
              pure $ Fail "decode_rust_response_error" ("Expected NotFound, got: " ++ show other)
        other ->
          pure $ Fail "decode_rust_response_error" ("Expected Error, got: " ++ show other)

-- Rust: EffectEnvelope { effect_type: "agent.spawn", payload: [10, 1, 49] }
-- Hex: [0x0a, 0x0b, 0x61, 0x67, 0x65, 0x6e, 0x74, 0x2e, 0x73, 0x70, 0x61, 0x77, 0x6e, 0x12, 0x03, 0x0a, 0x01, 0x31]
testDecodeRustEffectEnvelope :: IO TestResult
testDecodeRustEffectEnvelope = do
  let rustBytes =
        pack
          [ 0x0a,
            0x0b,
            0x61,
            0x67,
            0x65,
            0x6e,
            0x74,
            0x2e,
            0x73,
            0x70,
            0x61,
            0x77,
            0x6e,
            0x12,
            0x03,
            0x0a,
            0x01,
            0x31
          ]
  case fromByteString rustBytes of
    Left err ->
      pure $ Fail "decode_rust_envelope" ("Decode failed: " ++ show err)
    Right (decoded :: EffectEnvelope) ->
      assert
        "decode_rust_envelope"
        ( effectEnvelopeEffectType decoded == "agent.spawn"
            && effectEnvelopePayload decoded == pack [0x0a, 0x01, 0x31]
        )
        ("effect_type: " ++ show (effectEnvelopeEffectType decoded))

-- ============================================================================
-- Cross-language: decode Rust-produced inner types
-- ============================================================================

-- Rust: GetBranchResponse { branch: "main", detached: false }
-- Hex: [0x0a, 0x04, 0x6d, 0x61, 0x69, 0x6e]
-- Note: detached=false is default, so prost omits it from wire
testDecodeRustGetBranchResponse :: IO TestResult
testDecodeRustGetBranchResponse = do
  let rustBytes = pack [0x0a, 0x04, 0x6d, 0x61, 0x69, 0x6e]
  case fromByteString rustBytes of
    Left err ->
      pure $ Fail "decode_rust_get_branch" ("Decode failed: " ++ show err)
    Right (decoded :: GetBranchResponse) ->
      assert
        "decode_rust_get_branch"
        ( getBranchResponseBranch decoded == "main"
            && not (getBranchResponseDetached decoded)
        )
        ("branch=" ++ show (getBranchResponseBranch decoded))

-- Rust: LogResponse { success: true }
-- Hex: [0x08, 0x01]
testDecodeRustLogResponse :: IO TestResult
testDecodeRustLogResponse = do
  let rustBytes = pack [0x08, 0x01]
  case fromByteString rustBytes of
    Left err ->
      pure $ Fail "decode_rust_log_response" ("Decode failed: " ++ show err)
    Right (decoded :: LogResponse) ->
      assert
        "decode_rust_log_response"
        (logResponseSuccess decoded)
        "Expected success=true"

-- Rust: SpawnResponse { agent: AgentInfo { id:"a1", issue:"1", ... } }
-- Hex from cross_language_hex_reference
testDecodeRustSpawnResponse :: IO TestResult
testDecodeRustSpawnResponse = do
  let rustBytes =
        pack
          [ 0x0a,
            0x17,
            0x0a,
            0x02,
            0x61,
            0x31,
            0x12,
            0x01,
            0x31,
            0x1a,
            0x02,
            0x2f,
            0x77,
            0x22,
            0x01,
            0x62,
            0x28,
            0x01,
            0x30,
            0x01,
            0x38,
            0x01,
            0x42,
            0x01,
            0x74
          ]
  case fromByteString rustBytes of
    Left err ->
      pure $ Fail "decode_rust_spawn_response" ("Decode failed: " ++ show err)
    Right (decoded :: SpawnResponse) ->
      case spawnResponseAgent decoded of
        Nothing -> pure $ Fail "decode_rust_spawn_response" "Expected agent, got Nothing"
        Just agent ->
          assert
            "decode_rust_spawn_response"
            ( agentInfoId agent == "a1"
                && agentInfoIssue agent == "1"
                && agentInfoWorktreePath agent == "/w"
                && agentInfoBranchName agent == "b"
                && agentInfoZellijTab agent == "t"
            )
            ("id=" ++ show (agentInfoId agent))

-- Rust: SpawnBatchResponse { agents: [AgentInfo{...}], errors: ["issue 2: failed"] }
-- Hex from cross_language_hex_reference
testDecodeRustSpawnBatchResponse :: IO TestResult
testDecodeRustSpawnBatchResponse = do
  let rustBytes =
        pack
          [ 0x0a,
            0x29,
            0x0a,
            0x0b,
            0x67,
            0x68,
            0x2d,
            0x31,
            0x2d,
            0x63,
            0x6c,
            0x61,
            0x75,
            0x64,
            0x65,
            0x12,
            0x01,
            0x31,
            0x1a,
            0x04,
            0x2f,
            0x77,
            0x2f,
            0x31,
            0x22,
            0x06,
            0x67,
            0x68,
            0x2d,
            0x31,
            0x2f,
            0x61,
            0x28,
            0x01,
            0x30,
            0x01,
            0x38,
            0x01,
            0x42,
            0x03,
            0x31,
            0x2d,
            0x61,
            0x12,
            0x0f,
            0x69,
            0x73,
            0x73,
            0x75,
            0x65,
            0x20,
            0x32,
            0x3a,
            0x20,
            0x66,
            0x61,
            0x69,
            0x6c,
            0x65,
            0x64
          ]
  case fromByteString rustBytes of
    Left err ->
      pure $ Fail "decode_rust_spawn_batch" ("Decode failed: " ++ show err)
    Right (decoded :: SpawnBatchResponse) ->
      let agents = spawnBatchResponseAgents decoded
          errors = spawnBatchResponseErrors decoded
       in assert
            "decode_rust_spawn_batch"
            ( V.length agents == 1
                && agentInfoId (V.head agents) == "gh-1-claude"
                && V.length errors == 1
                && V.head errors == "issue 2: failed"
            )
            ( "agents="
                ++ show (V.length agents)
                ++ ", errors="
                ++ show (V.toList errors)
            )

-- ============================================================================
-- Cross-language: decode Rust-produced wrapped responses (EffectResponse envelope)
-- ============================================================================

-- Rust: EffectResponse { Payload(GetBranchResponse { branch: "main" }) }
-- Hex: [0x0a, 0x06, 0x0a, 0x04, 0x6d, 0x61, 0x69, 0x6e]
testDecodeRustWrappedGetBranch :: IO TestResult
testDecodeRustWrappedGetBranch = do
  let rustBytes = pack [0x0a, 0x06, 0x0a, 0x04, 0x6d, 0x61, 0x69, 0x6e]
  case fromByteString rustBytes of
    Left err ->
      pure $ Fail "decode_rust_wrapped_get_branch" ("Outer decode failed: " ++ show err)
    Right (decoded :: EffectResponse) ->
      case effectResponseResult decoded of
        Just (EffectResponseResultPayload innerBytes) ->
          case fromByteString innerBytes of
            Left err ->
              pure $ Fail "decode_rust_wrapped_get_branch" ("Inner decode failed: " ++ show err)
            Right (inner :: GetBranchResponse) ->
              assert
                "decode_rust_wrapped_get_branch"
                (getBranchResponseBranch inner == "main")
                ("branch=" ++ show (getBranchResponseBranch inner))
        other ->
          pure $ Fail "decode_rust_wrapped_get_branch" ("Expected Payload, got: " ++ show other)

-- Rust: EffectResponse { Payload(LogResponse { success: true }) }
-- Hex: [0x0a, 0x02, 0x08, 0x01]
testDecodeRustWrappedLogResponse :: IO TestResult
testDecodeRustWrappedLogResponse = do
  let rustBytes = pack [0x0a, 0x02, 0x08, 0x01]
  case fromByteString rustBytes of
    Left err ->
      pure $ Fail "decode_rust_wrapped_log" ("Outer decode failed: " ++ show err)
    Right (decoded :: EffectResponse) ->
      case effectResponseResult decoded of
        Just (EffectResponseResultPayload innerBytes) ->
          case fromByteString innerBytes of
            Left err ->
              pure $ Fail "decode_rust_wrapped_log" ("Inner decode failed: " ++ show err)
            Right (inner :: LogResponse) ->
              assert "decode_rust_wrapped_log" (logResponseSuccess inner) "Expected success=true"
        other ->
          pure $ Fail "decode_rust_wrapped_log" ("Expected Payload, got: " ++ show other)

-- Rust: EffectResponse { Payload(SpawnBatchResponse { 1 agent, 1 error }) }
-- This is THE critical test path — the exact pattern that fails in production.
-- Hex: [0x0a, 0x3c, ...]
testDecodeRustWrappedSpawnBatch :: IO TestResult
testDecodeRustWrappedSpawnBatch = do
  let rustBytes =
        pack
          [ 0x0a,
            0x3c,
            0x0a,
            0x29,
            0x0a,
            0x0b,
            0x67,
            0x68,
            0x2d,
            0x31,
            0x2d,
            0x63,
            0x6c,
            0x61,
            0x75,
            0x64,
            0x65,
            0x12,
            0x01,
            0x31,
            0x1a,
            0x04,
            0x2f,
            0x77,
            0x2f,
            0x31,
            0x22,
            0x06,
            0x67,
            0x68,
            0x2d,
            0x31,
            0x2f,
            0x61,
            0x28,
            0x01,
            0x30,
            0x01,
            0x38,
            0x01,
            0x42,
            0x03,
            0x31,
            0x2d,
            0x61,
            0x12,
            0x0f,
            0x69,
            0x73,
            0x73,
            0x75,
            0x65,
            0x20,
            0x32,
            0x3a,
            0x20,
            0x66,
            0x61,
            0x69,
            0x6c,
            0x65,
            0x64
          ]
  case fromByteString rustBytes of
    Left err ->
      pure $ Fail "decode_rust_wrapped_spawn_batch" ("Outer decode failed: " ++ show err)
    Right (decoded :: EffectResponse) ->
      case effectResponseResult decoded of
        Just (EffectResponseResultPayload innerBytes) ->
          case fromByteString innerBytes of
            Left err ->
              pure $
                Fail
                  "decode_rust_wrapped_spawn_batch"
                  ( "Inner decode failed (THIS IS THE PRODUCTION BUG PATH): "
                      ++ show err
                      ++ " bytes_len="
                      ++ show (BS.length innerBytes)
                  )
            Right (inner :: SpawnBatchResponse) ->
              let agents = spawnBatchResponseAgents inner
               in assert
                    "decode_rust_wrapped_spawn_batch"
                    (V.length agents == 1 && agentInfoId (V.head agents) == "gh-1-claude")
                    ("agents=" ++ show (V.length agents))
        other ->
          pure $ Fail "decode_rust_wrapped_spawn_batch" ("Expected Payload, got: " ++ show other)

-- ============================================================================
-- Cross-language: error variants
-- ============================================================================

-- Rust: EffectResponse { Error(Custom { code: "agent_error", message: "spawn timed out" }) }
-- Hex from cross_language_hex_reference
testDecodeRustCustomError :: IO TestResult
testDecodeRustCustomError = do
  let rustBytes =
        pack
          [ 0x12,
            0x20,
            0x32,
            0x1e,
            0x0a,
            0x0b,
            0x61,
            0x67,
            0x65,
            0x6e,
            0x74,
            0x5f,
            0x65,
            0x72,
            0x72,
            0x6f,
            0x72,
            0x12,
            0x0f,
            0x73,
            0x70,
            0x61,
            0x77,
            0x6e,
            0x20,
            0x74,
            0x69,
            0x6d,
            0x65,
            0x64,
            0x20,
            0x6f,
            0x75,
            0x74
          ]
  case fromByteString rustBytes of
    Left err ->
      pure $ Fail "decode_rust_custom_error" ("Decode failed: " ++ show err)
    Right (decoded :: EffectResponse) ->
      case effectResponseResult decoded of
        Just (EffectResponseResultError e) ->
          case effectErrorKind e of
            Just (EffectErrorKindCustom c) ->
              assert
                "decode_rust_custom_error"
                (customCode c == "agent_error" && customMessage c == "spawn timed out")
                ("code=" ++ show (customCode c) ++ ", msg=" ++ show (customMessage c))
            other ->
              pure $ Fail "decode_rust_custom_error" ("Expected Custom, got: " ++ show other)
        other ->
          pure $ Fail "decode_rust_custom_error" ("Expected Error, got: " ++ show other)

-- ============================================================================
-- Cross-language: varint boundary
-- ============================================================================

-- Rust: EffectResponse { Payload(128 bytes of 0x42) }
-- First 5 bytes: [0x0a, 0x80, 0x01, 0x42, 0x42], total=131
-- The 2-byte varint [0x80, 0x01] encodes length 128.
testDecodeRustVarintBoundary128 :: IO TestResult
testDecodeRustVarintBoundary128 = do
  -- Construct the full 131-byte message: tag + 2-byte varint + 128 payload bytes
  let header = pack [0x0a, 0x80, 0x01]
  let payload = BS.replicate 128 0x42
  let rustBytes = header <> payload
  case fromByteString rustBytes of
    Left err ->
      pure $ Fail "decode_rust_varint_128" ("Decode failed: " ++ show err)
    Right (decoded :: EffectResponse) ->
      case effectResponseResult decoded of
        Just (EffectResponseResultPayload p) ->
          assert
            "decode_rust_varint_128"
            (BS.length p == 128 && BS.all (== 0x42) p)
            ("payload_len=" ++ show (BS.length p))
        other ->
          pure $ Fail "decode_rust_varint_128" ("Expected Payload, got: " ++ show other)

-- ============================================================================
-- Cross-language: large payload (reproduces production decode failure)
-- ============================================================================

-- Rust: EffectResponse { Payload(SpawnBatchResponse { errors: [<377-byte string>] }) }
-- Outer message: 383 bytes total (tag + 2-byte varint + 380 payload)
-- Inner SpawnBatchResponse: 380 bytes (tag + 2-byte varint + 377 string)
testDecodeRustLargeSpawnBatchError :: IO TestResult
testDecodeRustLargeSpawnBatchError = do
  -- Construct the 383-byte message from known Rust output
  let header = pack [0x0a, 0xfc, 0x02] -- field 1, LEN, varint 380
  let innerHeader = pack [0x12, 0xf9, 0x02] -- field 2, LEN, varint 377
  let errorStr = "Issue 539: git worktree add failed: Preparing worktree (checking out 'gh-539/improve-stop-hook-error-messages-with-specific-com-gemini')\nfatal: '/Users/inannamalick/hangars/tidepool/repo/.exo/worktrees/gh-539-improve-stop-hook-error-messages-with-specific-com-gemini' is a missing but already registered worktree;\nuse 'git worktree prune' to remove stale worktree entries\n"
  let rustBytes = header <> innerHeader <> errorStr
  case fromByteString rustBytes of
    Left err ->
      pure $ Fail "decode_rust_large_spawn_batch_error" ("Outer decode failed: " ++ show err ++ " bytes_len=" ++ show (BS.length rustBytes))
    Right (decoded :: EffectResponse) ->
      case effectResponseResult decoded of
        Just (EffectResponseResultPayload innerBytes) ->
          case fromByteString innerBytes of
            Left err ->
              pure $ Fail "decode_rust_large_spawn_batch_error" ("Inner decode failed: " ++ show err)
            Right (inner :: SpawnBatchResponse) ->
              assert
                "decode_rust_large_spawn_batch_error"
                (V.length (spawnBatchResponseErrors inner) == 1)
                ("errors=" ++ show (V.length (spawnBatchResponseErrors inner)))
        other ->
          pure $ Fail "decode_rust_large_spawn_batch_error" ("Expected Payload, got: " ++ show other)

-- ============================================================================
-- Symmetry: verify Haskell-encoded bytes match Rust reference
-- ============================================================================

testHaskellEncodingMatchesRust :: IO TestResult
testHaskellEncodingMatchesRust = do
  -- Encode EffectResponse { payload: "hello" } in Haskell
  let resp =
        EffectResponse
          { effectResponseResult = Just (EffectResponseResultPayload "hello")
          }
  let haskellBytes = BL.toStrict (toLazyByteString resp)
  let rustBytes = pack [0x0a, 0x05, 0x68, 0x65, 0x6c, 0x6c, 0x6f]

  assert
    "haskell_encoding_matches_rust"
    (haskellBytes == rustBytes)
    ( "Haskell: "
        ++ show (BS.unpack haskellBytes)
        ++ " vs Rust: "
        ++ show (BS.unpack rustBytes)
    )

testHaskellGetBranchEncodingMatchesRust :: IO TestResult
testHaskellGetBranchEncodingMatchesRust = do
  let resp = GetBranchResponse {getBranchResponseBranch = "main", getBranchResponseDetached = False}
  let haskellBytes = BL.toStrict (toLazyByteString resp)
  let rustBytes = pack [0x0a, 0x04, 0x6d, 0x61, 0x69, 0x6e]
  assert
    "haskell_get_branch_encoding_matches_rust"
    (haskellBytes == rustBytes)
    ( "Haskell: "
        ++ show (BS.unpack haskellBytes)
        ++ " vs Rust: "
        ++ show (BS.unpack rustBytes)
    )

testHaskellLogResponseEncodingMatchesRust :: IO TestResult
testHaskellLogResponseEncodingMatchesRust = do
  let resp = LogResponse {logResponseSuccess = True}
  let haskellBytes = BL.toStrict (toLazyByteString resp)
  let rustBytes = pack [0x08, 0x01]
  assert
    "haskell_log_response_encoding_matches_rust"
    (haskellBytes == rustBytes)
    ( "Haskell: "
        ++ show (BS.unpack haskellBytes)
        ++ " vs Rust: "
        ++ show (BS.unpack rustBytes)
    )

-- ============================================================================
-- Symmetry: error encoding matches Rust
-- ============================================================================

-- Rust: EffectResponse { Error(NotFound { resource: "test" }) }
-- Hex: [0x12, 0x08, 0x0a, 0x06, 0x0a, 0x04, 0x74, 0x65, 0x73, 0x74]
testHaskellNotFoundErrorEncodingMatchesRust :: IO TestResult
testHaskellNotFoundErrorEncodingMatchesRust = do
  let resp =
        EffectResponse
          { effectResponseResult =
              Just
                ( EffectResponseResultError
                    EffectError
                      { effectErrorKind =
                          Just (EffectErrorKindNotFound (NotFound {notFoundResource = "test"}))
                      }
                )
          }
  let haskellBytes = BL.toStrict (toLazyByteString resp)
  let rustBytes = pack [0x12, 0x08, 0x0a, 0x06, 0x0a, 0x04, 0x74, 0x65, 0x73, 0x74]
  assert
    "haskell_not_found_error_encoding_matches_rust"
    (haskellBytes == rustBytes)
    ( "Haskell: "
        ++ show (BS.unpack haskellBytes)
        ++ " vs Rust: "
        ++ show (BS.unpack rustBytes)
    )

-- Rust: EffectResponse { Error(Custom { code: "agent_error", message: "spawn timed out" }) }
-- Hex from cross_language_hex_reference
testHaskellCustomErrorEncodingMatchesRust :: IO TestResult
testHaskellCustomErrorEncodingMatchesRust = do
  let resp =
        EffectResponse
          { effectResponseResult =
              Just
                ( EffectResponseResultError
                    EffectError
                      { effectErrorKind =
                          Just
                            ( EffectErrorKindCustom
                                Custom
                                  { customCode = "agent_error",
                                    customMessage = "spawn timed out",
                                    customData = ""
                                  }
                            )
                      }
                )
          }
  let haskellBytes = BL.toStrict (toLazyByteString resp)
  let rustBytes =
        pack
          [ 0x12,
            0x20,
            0x32,
            0x1e,
            0x0a,
            0x0b,
            0x61,
            0x67,
            0x65,
            0x6e,
            0x74,
            0x5f,
            0x65,
            0x72,
            0x72,
            0x6f,
            0x72,
            0x12,
            0x0f,
            0x73,
            0x70,
            0x61,
            0x77,
            0x6e,
            0x20,
            0x74,
            0x69,
            0x6d,
            0x65,
            0x64,
            0x20,
            0x6f,
            0x75,
            0x74
          ]
  assert
    "haskell_custom_error_encoding_matches_rust"
    (haskellBytes == rustBytes)
    ( "Haskell: "
        ++ show (BS.unpack haskellBytes)
        ++ " vs Rust: "
        ++ show (BS.unpack rustBytes)
    )

-- ============================================================================
-- Edge case: empty payload in oneof
-- ============================================================================

-- Rust: EffectResponse { Payload(vec![]) } → [0x0a, 0x00]
-- This is a critical edge case: empty bytes in a oneof must still be present
-- on the wire (tag + length=0), otherwise it looks like "no result set".
testDecodeRustEmptyPayload :: IO TestResult
testDecodeRustEmptyPayload = do
  let rustBytes = pack [0x0a, 0x00]
  case fromByteString rustBytes of
    Left err ->
      pure $ Fail "decode_rust_empty_payload" ("Decode failed: " ++ show err)
    Right (decoded :: EffectResponse) ->
      case effectResponseResult decoded of
        Just (EffectResponseResultPayload p) ->
          assert "decode_rust_empty_payload" (BS.null p) ("Expected empty, got " ++ show (BS.length p) ++ " bytes")
        other ->
          pure $ Fail "decode_rust_empty_payload" ("Expected Payload, got: " ++ show other)

testHaskellEmptyPayloadMatchesRust :: IO TestResult
testHaskellEmptyPayloadMatchesRust = do
  let resp =
        EffectResponse
          { effectResponseResult = Just (EffectResponseResultPayload "")
          }
  let haskellBytes = BL.toStrict (toLazyByteString resp)
  let rustBytes = pack [0x0a, 0x00]
  assert
    "haskell_empty_payload_matches_rust"
    (haskellBytes == rustBytes)
    ( "Haskell: "
        ++ show (BS.unpack haskellBytes)
        ++ " vs Rust: "
        ++ show (BS.unpack rustBytes)
    )
