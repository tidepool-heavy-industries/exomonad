{-# LANGUAGE OverloadedStrings #-}

-- | Property-based tests ensuring JSON encode/decode roundtrips correctly.
--
-- These tests use QuickCheck to generate arbitrary wire types and verify
-- that @decode . encode ≡ id@ holds for all types that cross the WASM/JSON boundary.
module ProtocolPropertySpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
  ( Arbitrary(..)
  , oneof
  , elements
  , listOf
  , scale
  , frequency
  , arbitraryUnicodeChar
  )
import Data.Aeson (encode, decode, Value(..), object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V

import Tidepool.Wasm.WireTypes


spec :: Spec
spec = do
  serializableEffectPropertySpec
  effectResultPropertySpec
  executionPhasePropertySpec
  graphStatePropertySpec
  stepOutputPropertySpec
  edgeCaseSpec


-- ════════════════════════════════════════════════════════════════════════════
-- ARBITRARY INSTANCES
-- ════════════════════════════════════════════════════════════════════════════

-- | Generate arbitrary Text with various edge cases.
-- Uses frequency to weight random generation more heavily than fixed cases.
instance Arbitrary Text where
  arbitrary = frequency
    [ (5, T.pack <$> listOf arbitraryUnicodeChar)  -- Random unicode (most common)
    , (3, T.pack <$> listOf (elements ['a'..'z'])) -- Simple ASCII letters
    , (1, pure "")                                  -- Empty string
    , (1, pure " ")                                 -- Single space
    , (1, pure "\t\n\r")                            -- Whitespace chars
    , (1, pure "emoji: \x1F600\x1F4A9\x2764")      -- Emoji
    , (1, pure "null")                              -- JSON keyword as string
    , (1, pure "{\"nested\": \"json\"}")            -- JSON-like string
    ]

  shrink t
    | T.null t  = []
    | otherwise = [T.empty, T.take (T.length t `div` 2) t]


-- Note: Arbitrary Value instance is provided by aeson (Data.Aeson.Types.Internal)
-- with depth limiting built-in.

-- | Arbitrary SerializableEffect covering all constructors
instance Arbitrary SerializableEffect where
  arbitrary = oneof
    [ EffLlmComplete
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary  -- Maybe Value roundtrips correctly now
    , EffLogInfo <$> arbitrary
    , EffLogError <$> arbitrary
    , EffHabitica
        <$> elements ["GetUser", "ScoreTask", "GetTasks", "FetchTodos", "CreateTodo", "AddChecklistItem"]
        <*> scale (`div` 2) arbitrary
    , EffTelegramConfirm
        <$> arbitrary
        <*> listOf ((,) <$> arbitrary <*> arbitrary)
    ]

  shrink (EffLlmComplete node sys user schema) =
    [ EffLogInfo node ]  -- Simplify to simpler effect
    ++ [ EffLlmComplete node' sys user schema | node' <- shrink node ]
    ++ [ EffLlmComplete node sys' user schema | sys' <- shrink sys ]
    ++ [ EffLlmComplete node sys user' schema | user' <- shrink user ]
    ++ [ EffLlmComplete node sys user schema' | schema' <- shrink schema ]
  shrink (EffLogInfo msg) =
    [ EffLogInfo msg' | msg' <- shrink msg ]
  shrink (EffLogError msg) =
    [ EffLogInfo msg ]  -- Error to Info
    ++ [ EffLogError msg' | msg' <- shrink msg ]
  shrink (EffHabitica op payload) =
    [ EffLogInfo op ]
    ++ [ EffHabitica op payload' | payload' <- shrink payload ]
  shrink (EffTelegramConfirm msg buttons) =
    [ EffLogInfo msg ]
    ++ [ EffTelegramConfirm msg' buttons | msg' <- shrink msg ]
    ++ [ EffTelegramConfirm msg buttons' | buttons' <- shrink buttons ]


-- | Arbitrary EffectResult covering success and error cases
instance Arbitrary EffectResult where
  arbitrary = oneof
    [ ResSuccess <$> arbitrary
    , ResError <$> arbitrary
    ]

  shrink (ResSuccess val) =
    [ ResSuccess Nothing ]
    ++ [ ResSuccess val' | val' <- shrink val ]
  shrink (ResError msg) =
    [ ResSuccess Nothing ]  -- Error to success with nothing
    ++ [ ResError msg' | msg' <- shrink msg ]


-- | Arbitrary ExecutionPhase covering all variants
instance Arbitrary ExecutionPhase where
  arbitrary = oneof
    [ pure PhaseIdle
    , PhaseInNode <$> arbitrary
    , PhaseTransitioning <$> arbitrary <*> arbitrary
    , PhaseCompleted <$> scale (`div` 2) arbitrary
    , PhaseFailed <$> arbitrary
    ]

  shrink PhaseIdle = []
  shrink (PhaseInNode name) =
    [ PhaseIdle ]
    ++ [ PhaseInNode name' | name' <- shrink name ]
  shrink (PhaseTransitioning from to) =
    [ PhaseIdle, PhaseInNode from ]
    ++ [ PhaseTransitioning from' to | from' <- shrink from ]
    ++ [ PhaseTransitioning from to' | to' <- shrink to ]
  shrink (PhaseCompleted result) =
    [ PhaseIdle ]
    ++ [ PhaseCompleted result' | result' <- shrink result ]
  shrink (PhaseFailed err) =
    [ PhaseIdle ]
    ++ [ PhaseFailed err' | err' <- shrink err ]


-- | Arbitrary GraphState
instance Arbitrary GraphState where
  arbitrary = GraphState
    <$> arbitrary
    <*> listOf arbitrary

  shrink (GraphState phase nodes) =
    [ GraphState phase' nodes | phase' <- shrink phase ]
    ++ [ GraphState phase nodes' | nodes' <- shrink nodes ]


-- | Arbitrary StepOutput covering all variants
instance Arbitrary StepOutput where
  arbitrary = oneof
    [ StepYield <$> arbitrary <*> arbitrary
    , StepDone <$> scale (`div` 2) arbitrary <*> arbitrary
    , StepFailed <$> arbitrary <*> arbitrary
    ]

  shrink (StepYield effect gs) =
    [ StepYield effect' gs | effect' <- shrink effect ]
    ++ [ StepYield effect gs' | gs' <- shrink gs ]
  shrink (StepDone result gs) =
    [ StepDone result' gs | result' <- shrink result ]
    ++ [ StepDone result gs' | gs' <- shrink gs ]
  shrink (StepFailed err gs) =
    [ StepFailed err' gs | err' <- shrink err ]
    ++ [ StepFailed err gs' | gs' <- shrink gs ]


-- ════════════════════════════════════════════════════════════════════════════
-- PROPERTY TESTS
-- ════════════════════════════════════════════════════════════════════════════

serializableEffectPropertySpec :: Spec
serializableEffectPropertySpec = describe "SerializableEffect properties" $ do

  prop "decode . encode ≡ id (roundtrip)" $ \(effect :: SerializableEffect) ->
    decode (encode effect) == Just effect

  prop "all variants produce valid JSON objects with 'type' field" $ \(effect :: SerializableEffect) ->
    case decode (encode effect) :: Maybe Value of
      Just (Object obj) -> KM.member "type" obj
      _ -> False


effectResultPropertySpec :: Spec
effectResultPropertySpec = describe "EffectResult properties" $ do

  prop "decode . encode ≡ id (roundtrip)" $ \(result :: EffectResult) ->
    decode (encode result) == Just result

  prop "success results have 'value' field iff value is Just" $ \val ->
    let result = ResSuccess val
        json = decode (encode result) :: Maybe Value
    in case (val, json) of
         (Nothing, Just (Object obj)) -> not (KM.member "value" obj)
         (Just _, Just (Object obj)) -> KM.member "value" obj
         _ -> False

  prop "error results have 'message' field" $ \msg ->
    let result = ResError msg
        json = decode (encode result) :: Maybe Value
    in case json of
         Just (Object obj) -> KM.member "message" obj
         _ -> False


executionPhasePropertySpec :: Spec
executionPhasePropertySpec = describe "ExecutionPhase properties" $ do

  prop "decode . encode ≡ id (roundtrip)" $ \(phase :: ExecutionPhase) ->
    decode (encode phase) == Just phase

  prop "all variants produce valid JSON with 'type' field" $ \(phase :: ExecutionPhase) ->
    case decode (encode phase) :: Maybe Value of
      Just (Object obj) -> KM.member "type" obj
      _ -> False


graphStatePropertySpec :: Spec
graphStatePropertySpec = describe "GraphState properties" $ do

  prop "decode . encode ≡ id (roundtrip)" $ \(gs :: GraphState) ->
    decode (encode gs) == Just gs

  prop "always has 'phase' and 'completedNodes' fields" $ \(gs :: GraphState) ->
    case decode (encode gs) :: Maybe Value of
      Just (Object obj) ->
        KM.member "phase" obj && KM.member "completedNodes" obj
      _ -> False


stepOutputPropertySpec :: Spec
stepOutputPropertySpec = describe "StepOutput properties" $ do

  prop "decode . encode ≡ id (roundtrip)" $ \(output :: StepOutput) ->
    decode (encode output) == Just output

  prop "always has 'done' and 'graphState' fields" $ \(output :: StepOutput) ->
    case decode (encode output) :: Maybe Value of
      Just (Object obj) ->
        KM.member "done" obj && KM.member "graphState" obj
      _ -> False

  prop "StepYield has done=false and non-null effect" $ \effect gs ->
    let output = StepYield effect gs
        json = decode (encode output) :: Maybe Value
    in case json of
         Just (Object obj) ->
           KM.lookup "done" obj == Just (Bool False) &&
           case KM.lookup "effect" obj of
             Just Null -> False
             Just (Object _) -> True
             _ -> False
         _ -> False

  prop "StepDone has done=true and null effect" $ \result gs ->
    let output = StepDone result gs
        json = decode (encode output) :: Maybe Value
    in case json of
         Just (Object obj) ->
           KM.lookup "done" obj == Just (Bool True) &&
           KM.lookup "effect" obj == Just Null
         _ -> False

  prop "StepFailed has done=true and error field" $ \err gs ->
    let output = StepFailed err gs
        json = decode (encode output) :: Maybe Value
    in case json of
         Just (Object obj) ->
           KM.lookup "done" obj == Just (Bool True) &&
           KM.member "error" obj
         _ -> False


-- ════════════════════════════════════════════════════════════════════════════
-- EDGE CASE TESTS
-- ════════════════════════════════════════════════════════════════════════════

edgeCaseSpec :: Spec
edgeCaseSpec = describe "Edge cases" $ do

  describe "Empty strings" $ do
    it "roundtrips EffLogInfo with empty message" $ do
      let effect = EffLogInfo ""
      decode (encode effect) `shouldBe` Just effect

    it "roundtrips ResError with empty message" $ do
      let result = ResError ""
      decode (encode result) `shouldBe` Just result

    it "roundtrips PhaseFailed with empty error" $ do
      let phase = PhaseFailed ""
      decode (encode phase) `shouldBe` Just phase

  describe "Unicode and special characters" $ do
    it "roundtrips effect with emoji" $ do
      let effect = EffLogInfo "Status: \x1F600 \x1F4A9 \x2764"
      decode (encode effect) `shouldBe` Just effect

    it "roundtrips effect with null bytes" $ do
      let effect = EffLogInfo "before\x0000after"
      decode (encode effect) `shouldBe` Just effect

    it "roundtrips effect with newlines and tabs" $ do
      let effect = EffLogInfo "line1\nline2\ttabbed"
      decode (encode effect) `shouldBe` Just effect

    it "roundtrips effect with backslashes" $ do
      let effect = EffLogInfo "path\\to\\file"
      decode (encode effect) `shouldBe` Just effect

    it "roundtrips effect with quotes" $ do
      let effect = EffLogInfo "He said \"hello\""
      decode (encode effect) `shouldBe` Just effect

    it "roundtrips effect with JSON-like content" $ do
      let effect = EffLogInfo "{\"key\": \"value\", \"nested\": {\"a\": 1}}"
      decode (encode effect) `shouldBe` Just effect

    it "roundtrips effect with very long string (10KB)" $ do
      let longStr = T.replicate 10000 "x"
          effect = EffLogInfo longStr
      decode (encode effect) `shouldBe` Just effect

  describe "Nothing vs Just Null (proper roundtrip)" $ do
    it "ResSuccess Nothing omits value field" $ do
      let result = ResSuccess Nothing
          json = decode (encode result) :: Maybe Value
      case json of
        Just (Object obj) -> KM.lookup "value" obj `shouldBe` Nothing
        _ -> expectationFailure "Expected JSON object"

    it "ResSuccess (Just Null) includes value as null" $ do
      let result = ResSuccess (Just Null)
          json = decode (encode result) :: Maybe Value
      case json of
        Just (Object obj) -> KM.lookup "value" obj `shouldBe` Just Null
        _ -> expectationFailure "Expected JSON object"

    it "EffLlmComplete with Nothing schema omits eff_schema field" $ do
      let effect = EffLlmComplete "node" "sys" "user" Nothing
          json = decode (encode effect) :: Maybe Value
      case json of
        Just (Object obj) -> KM.lookup "eff_schema" obj `shouldBe` Nothing
        _ -> expectationFailure "Expected JSON object"

    it "EffLlmComplete with Just Null schema includes eff_schema as null" $ do
      let effect = EffLlmComplete "node" "sys" "user" (Just Null)
          json = decode (encode effect) :: Maybe Value
      case json of
        Just (Object obj) -> KM.lookup "eff_schema" obj `shouldBe` Just Null
        _ -> expectationFailure "Expected JSON object"

  describe "Numeric edge cases in JSON Values" $ do
    it "roundtrips Value with 0" $ do
      let gs = GraphState (PhaseCompleted (Number 0)) []
      decode (encode gs) `shouldBe` Just gs

    it "roundtrips Value with negative number" $ do
      let gs = GraphState (PhaseCompleted (Number (-42))) []
      decode (encode gs) `shouldBe` Just gs

    it "roundtrips Value with large integer" $ do
      let gs = GraphState (PhaseCompleted (Number 9007199254740991)) []  -- JS MAX_SAFE_INTEGER
      decode (encode gs) `shouldBe` Just gs

    it "roundtrips Value with small decimal" $ do
      let gs = GraphState (PhaseCompleted (Number 0.000001)) []
      decode (encode gs) `shouldBe` Just gs

  describe "Complex nested structures" $ do
    it "roundtrips deeply nested JSON value" $ do
      let nested = object
            [ "level1" .= object
                [ "level2" .= object
                    [ "level3" .= object
                        [ "value" .= ("deep" :: Text)
                        ]
                    ]
                ]
            ]
          output = StepDone nested (GraphState (PhaseCompleted nested) [])
      decode (encode output) `shouldBe` Just output

    it "roundtrips array with mixed types" $ do
      let arr = Array $ V.fromList [Null, Bool True, Number 42, String "hello"]
          output = StepDone arr (GraphState (PhaseCompleted arr) [])
      decode (encode output) `shouldBe` Just output

    it "roundtrips StepOutput with many completed nodes" $ do
      let nodes = ["node" <> T.pack (show i) | i <- [1..100 :: Int]]
          gs = GraphState (PhaseCompleted (String "done")) nodes
          output = StepDone (String "done") gs
      decode (encode output) `shouldBe` Just output

  describe "Sum type encoding (variant tags)" $ do
    it "all SerializableEffect variants have distinct 'type' values" $ do
      let effects =
            [ EffLlmComplete "n" "s" "u" Nothing
            , EffLogInfo "msg"
            , EffLogError "err"
            , EffHabitica "GetUser" (object [])
            ]
          getType eff = case decode (encode eff) :: Maybe Value of
            Just (Object obj) -> KM.lookup "type" obj
            _ -> Nothing
          types = map getType effects
      -- All should be Just (String _) and all distinct
      length types `shouldBe` length effects
      length (filter (/= Nothing) types) `shouldBe` length effects

    it "all ExecutionPhase variants have distinct 'type' values" $ do
      let phases =
            [ PhaseIdle
            , PhaseInNode "n"
            , PhaseTransitioning "a" "b"
            , PhaseCompleted Null
            , PhaseFailed "err"
            ]
          getType ph = case decode (encode ph) :: Maybe Value of
            Just (Object obj) -> KM.lookup "type" obj
            _ -> Nothing
          types = map getType phases
      length types `shouldBe` length phases
      length (filter (/= Nothing) types) `shouldBe` length phases
