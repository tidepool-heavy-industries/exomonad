module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Aeson (eitherDecode, encode, Value)
import qualified Data.ByteString.Lazy as BL
import ExoMonad.Control.Protocol
import System.FilePath ((</>))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Protocol Golden Tests"
  [ testGroup "ControlResponse"
      [ testCase "hook_response_allow.json" $ assertGoldenResponse "hook_response_allow.json"
      , testCase "hook_response_deny.json" $ assertGoldenResponse "hook_response_deny.json"
      , testCase "mcp_tool_response_success.json" $ assertGoldenResponse "mcp_tool_response_success.json"
      , testCase "mcp_tool_response_error.json" $ assertGoldenResponse "mcp_tool_response_error.json"
      , testCase "pong.json" $ assertGoldenResponse "pong.json"
      , testCase "tools_list_response.json" $ assertGoldenResponse "tools_list_response.json"
      ]
  , testGroup "ControlMessage"
      [ testCase "mcp_tool_call.json" $ assertGoldenMessage "mcp_tool_call.json"
      , testCase "ping.json" $ assertGoldenMessage "ping.json"
      , testCase "tools_list_request.json" $ assertGoldenMessage "tools_list_request.json"
      ]
  ]

fixtureDir :: FilePath
fixtureDir = "test/fixtures/protocol"

assertGoldenResponse :: FilePath -> Assertion
assertGoldenResponse name = do
  content <- BL.readFile (fixtureDir </> name)
  -- Test deserialization
  case eitherDecode @ControlResponse content of
    Left err -> assertFailure $ "Failed to decode " ++ name ++ ": " ++ err
    Right response -> do
      -- Test re-serialization (via Value to ignore formatting/ordering)
      let serialized = encode response
      case (eitherDecode @Value content, eitherDecode @Value serialized) of
        (Right expectedV, Right actualV) ->
          actualV @?= expectedV
        (Left err, _) -> assertFailure $ "Failed to decode fixture as Value: " ++ err
        (_, Left err) -> assertFailure $ "Failed to decode serialized as Value: " ++ err

assertGoldenMessage :: FilePath -> Assertion
assertGoldenMessage name = do
  content <- BL.readFile (fixtureDir </> name)
  -- Test deserialization
  case eitherDecode @ControlMessage content of
    Left err -> assertFailure $ "Failed to decode " ++ name ++ ": " ++ err
    Right message -> do
      -- Test re-serialization
      let serialized = encode message
      case (eitherDecode @Value content, eitherDecode @Value serialized) of
        (Right expectedV, Right actualV) ->
          actualV @?= expectedV
        (Left err, _) -> assertFailure $ "Failed to decode fixture as Value: " ++ err
        (_, Left err) -> assertFailure $ "Failed to decode serialized as Value: " ++ err
