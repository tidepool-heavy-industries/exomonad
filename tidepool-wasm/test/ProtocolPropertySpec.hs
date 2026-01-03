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
  , Gen
  , oneof
  , elements
  , listOf
  , scale
  , frequency
  , arbitraryUnicodeChar
  )
import Data.Aeson (encode, decode, Value(..), object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V

import Tidepool.Wasm.WireTypes
import Tidepool.Anthropic.Types (ImageSource(..))


spec :: Spec
spec = do
  serializableEffectPropertySpec
  effectResultPropertySpec
  executionPhasePropertySpec
  graphStatePropertySpec
  stepOutputPropertySpec
  graphInfoPropertySpec
  wireContentBlockPropertySpec
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

-- | Generate Maybe Value that roundtrips correctly.
-- Aeson's .:? treats null and missing field the same (both → Nothing),
-- so we never generate Just Null since it would decode to Nothing.
arbMaybeNonNullValue :: Gen (Maybe Value)
arbMaybeNonNullValue = do
  mv <- arbitrary
  pure $ mv >>= \v -> case v of
    Null -> Nothing
    _    -> Just v

-- | Generate Maybe (Map Text Value) for log fields.
-- Avoid null values in the map since they don't roundtrip correctly.
arbMaybeFields :: Gen (Maybe (Map Text Value))
arbMaybeFields = oneof
  [ pure Nothing
  , Just . Map.fromList <$> listOf ((,) <$> arbitrary <*> arbNonNullValue)
  ]
  where
    arbNonNullValue = arbitrary `suchThat` (/= Null)
    suchThat gen p = do
      x <- gen
      if p x then pure x else suchThat gen p

-- | Arbitrary SerializableEffect covering all constructors
instance Arbitrary SerializableEffect where
  arbitrary = oneof
    [ EffLlmComplete
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbMaybeNonNullValue  -- Avoid Just Null (doesn't roundtrip)
        <*> arbitrary             -- model: Maybe Text
    , EffLogInfo <$> arbitrary <*> arbMaybeFields
    , EffLogError <$> arbitrary <*> arbMaybeFields
    , EffHabitica
        <$> elements ["GetUser", "ScoreTask", "GetTasks", "FetchTodos", "CreateTodo", "AddChecklistItem"]
        <*> scale (`div` 2) arbitrary
    , EffTelegramSend
        <$> arbitrary
        <*> elements ["PlainText", "Markdown", "HTML"]
        <*> arbitrary  -- threadId: Maybe Int
    , EffTelegramAsk
        <$> arbitrary
        <*> elements ["PlainText", "Markdown", "HTML"]
        <*> listOf ((,) <$> arbitrary <*> arbitrary)
        <*> arbitrary  -- threadId: Maybe Int
    ]

  shrink (EffLlmComplete node sys user schema model) =
    [ EffLogInfo node Nothing ]  -- Simplify to simpler effect
    ++ [ EffLlmComplete node' sys user schema model | node' <- shrink node ]
    ++ [ EffLlmComplete node sys' user schema model | sys' <- shrink sys ]
    ++ [ EffLlmComplete node sys user' schema model | user' <- shrink user ]
    ++ [ EffLlmComplete node sys user schema' model
       | schema' <- shrink schema
       , schema' /= Just Null  -- Avoid Just Null (doesn't roundtrip)
       ]
    ++ [ EffLlmComplete node sys user schema model' | model' <- shrink model ]
  shrink (EffLogInfo msg fields) =
    [ EffLogInfo msg Nothing | Just _ <- [fields] ]  -- Remove fields first
    ++ [ EffLogInfo msg' fields | msg' <- shrink msg ]
  shrink (EffLogError msg fields) =
    [ EffLogInfo msg Nothing ]  -- Error to Info
    ++ [ EffLogError msg Nothing | Just _ <- [fields] ]  -- Remove fields first
    ++ [ EffLogError msg' fields | msg' <- shrink msg ]
  shrink (EffHabitica op payload) =
    [ EffLogInfo op Nothing ]
    ++ [ EffHabitica op payload' | payload' <- shrink payload ]
  shrink (EffTelegramSend txt parseMode threadId) =
    [ EffLogInfo txt Nothing ]
    ++ [ EffTelegramSend txt' parseMode threadId | txt' <- shrink txt ]
    ++ [ EffTelegramSend txt parseMode Nothing | Just _ <- [threadId] ]  -- Remove threadId
  shrink (EffTelegramAsk txt parseMode buttons threadId) =
    [ EffLogInfo txt Nothing ]
    ++ [ EffTelegramAsk txt' parseMode buttons threadId | txt' <- shrink txt ]
    ++ [ EffTelegramAsk txt parseMode buttons' threadId | buttons' <- shrink buttons ]
    ++ [ EffTelegramAsk txt parseMode buttons Nothing | Just _ <- [threadId] ]  -- Remove threadId


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


-- | Arbitrary TypeInfoWire
instance Arbitrary TypeInfoWire where
  arbitrary = TypeInfoWire
    <$> arbitrary
    <*> arbitrary

  shrink (TypeInfoWire name mod') =
    [ TypeInfoWire name' mod' | name' <- shrink name ]
    ++ [ TypeInfoWire name mod'' | mod'' <- shrink mod' ]


-- | Arbitrary GotoTargetWire
instance Arbitrary GotoTargetWire where
  arbitrary = GotoTargetWire
    <$> arbitrary
    <*> arbitrary

  shrink (GotoTargetWire target payload) =
    [ GotoTargetWire target' payload | target' <- shrink target ]
    ++ [ GotoTargetWire target payload' | payload' <- shrink payload ]


-- | Arbitrary NodeInfoWire
instance Arbitrary NodeInfoWire where
  arbitrary = NodeInfoWire
    <$> arbitrary
    <*> elements ["LLM", "Logic"]
    <*> scale (`div` 2) (listOf arbitrary)
    <*> arbitrary
    <*> scale (`div` 2) (listOf arbitrary)

  shrink (NodeInfoWire name kind needs schema targets) =
    [ NodeInfoWire name' kind needs schema targets | name' <- shrink name ]
    ++ [ NodeInfoWire name kind needs' schema targets | needs' <- shrink needs ]
    ++ [ NodeInfoWire name kind needs schema' targets | schema' <- shrink schema ]
    ++ [ NodeInfoWire name kind needs schema targets' | targets' <- shrink targets ]


-- | Arbitrary EdgeInfoWire
instance Arbitrary EdgeInfoWire where
  arbitrary = EdgeInfoWire
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary

  shrink (EdgeInfoWire from to payload) =
    [ EdgeInfoWire from' to payload | from' <- shrink from ]
    ++ [ EdgeInfoWire from to' payload | to' <- shrink to ]
    ++ [ EdgeInfoWire from to payload' | payload' <- shrink payload ]


-- | Arbitrary GraphInfoWire
instance Arbitrary GraphInfoWire where
  arbitrary = GraphInfoWire
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> scale (`div` 2) (listOf arbitrary)
    <*> scale (`div` 2) (listOf arbitrary)

  shrink (GraphInfoWire name entry exit nodes edges) =
    [ GraphInfoWire name' entry exit nodes edges | name' <- shrink name ]
    ++ [ GraphInfoWire name entry' exit nodes edges | entry' <- shrink entry ]
    ++ [ GraphInfoWire name entry exit' nodes edges | exit' <- shrink exit ]
    ++ [ GraphInfoWire name entry exit nodes' edges | nodes' <- shrink nodes ]
    ++ [ GraphInfoWire name entry exit nodes edges' | edges' <- shrink edges ]


-- | Arbitrary ImageSource
instance Arbitrary ImageSource where
  arbitrary = oneof
    [ Base64Image
        <$> elements ["image/jpeg", "image/png", "image/webp", "image/gif"]
        <*> arbitrary  -- Base64 text
    , UrlImage <$> arbitrary  -- URL text
    ]

  shrink (Base64Image mediaType imgData) =
    [ UrlImage "http://example.com/image.png" ]  -- Simplify to URL
    ++ [ Base64Image mediaType imgData' | imgData' <- shrink imgData ]
  shrink (UrlImage url) =
    [ UrlImage url' | url' <- shrink url ]


-- | Arbitrary WireContentBlock
instance Arbitrary WireContentBlock where
  arbitrary = oneof
    [ WCBText <$> arbitrary
    , WCBImage <$> arbitrary  -- Uses Arbitrary ImageSource
    , WCBToolUse <$> arbitrary <*> arbitrary <*> arbitrary
    , WCBToolResult <$> arbitrary <*> arbitrary <*> arbitrary
    ]

  shrink (WCBText txt) = [WCBText txt' | txt' <- shrink txt]
  shrink (WCBImage source) = [WCBImage source' | source' <- shrink source]
  shrink (WCBToolUse toolId name input) =
    [WCBText "tool"]  -- Simplify to text
    ++ [WCBToolUse toolId' name input | toolId' <- shrink toolId]
    ++ [WCBToolUse toolId name' input | name' <- shrink name]
  shrink (WCBToolResult toolId content isErr) =
    [WCBText content]  -- Simplify to text
    ++ [WCBToolResult toolId' content isErr | toolId' <- shrink toolId]
    ++ [WCBToolResult toolId content' isErr | content' <- shrink content]


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


graphInfoPropertySpec :: Spec
graphInfoPropertySpec = describe "GraphInfo wire type properties" $ do

  describe "TypeInfoWire" $ do
    prop "decode . encode ≡ id (roundtrip)" $ \(ti :: TypeInfoWire) ->
      decode (encode ti) == Just ti

    prop "has typeName and typeModule fields" $ \(ti :: TypeInfoWire) ->
      case decode (encode ti) :: Maybe Value of
        Just (Object obj) ->
          KM.member "typeName" obj && KM.member "typeModule" obj
        _ -> False

  describe "GotoTargetWire" $ do
    prop "decode . encode ≡ id (roundtrip)" $ \(gt :: GotoTargetWire) ->
      decode (encode gt) == Just gt

    prop "has gtTarget and gtPayloadType fields" $ \(gt :: GotoTargetWire) ->
      case decode (encode gt) :: Maybe Value of
        Just (Object obj) ->
          KM.member "gtTarget" obj && KM.member "gtPayloadType" obj
        _ -> False

  describe "NodeInfoWire" $ do
    prop "decode . encode ≡ id (roundtrip)" $ \(ni :: NodeInfoWire) ->
      decode (encode ni) == Just ni

    prop "has all required fields" $ \(ni :: NodeInfoWire) ->
      case decode (encode ni) :: Maybe Value of
        Just (Object obj) ->
          KM.member "niName" obj &&
          KM.member "niKind" obj &&
          KM.member "niNeeds" obj &&
          KM.member "niGotoTargets" obj
        _ -> False

  describe "EdgeInfoWire" $ do
    prop "decode . encode ≡ id (roundtrip)" $ \(ei :: EdgeInfoWire) ->
      decode (encode ei) == Just ei

    prop "has all required fields" $ \(ei :: EdgeInfoWire) ->
      case decode (encode ei) :: Maybe Value of
        Just (Object obj) ->
          KM.member "eiFrom" obj &&
          KM.member "eiTo" obj &&
          KM.member "eiPayloadType" obj
        _ -> False

  describe "GraphInfoWire" $ do
    prop "decode . encode ≡ id (roundtrip)" $ \(gi :: GraphInfoWire) ->
      decode (encode gi) == Just gi

    prop "has all required fields" $ \(gi :: GraphInfoWire) ->
      case decode (encode gi) :: Maybe Value of
        Just (Object obj) ->
          KM.member "name" obj &&
          KM.member "entryType" obj &&
          KM.member "exitType" obj &&
          KM.member "nodes" obj &&
          KM.member "edges" obj
        _ -> False


wireContentBlockPropertySpec :: Spec
wireContentBlockPropertySpec = describe "WireContentBlock properties" $ do

  prop "decode . encode ≡ id (roundtrip)" $ \(block :: WireContentBlock) ->
    decode (encode block) == Just block

  prop "all variants produce valid JSON with 'type' field" $ \(block :: WireContentBlock) ->
    case decode (encode block) :: Maybe Value of
      Just (Object obj) -> KM.member "type" obj
      _ -> False

  describe "WCBImage with base64 source" $ do
    it "round-trips WCBImage with base64 source" $ do
      let img = WCBImage (Base64Image "image/jpeg" "base64data")
      decode (encode img) `shouldBe` Just img

    it "encodes WCBImage with correct JSON structure" $ do
      let img = WCBImage (Base64Image "image/png" "abc123")
      let json = decode (encode img) :: Maybe Value
      case json of
        Just (Object obj) -> do
          KM.lookup "type" obj `shouldBe` Just (String "image")
          case KM.lookup "source" obj of
            Just (Object srcObj) -> do
              KM.lookup "type" srcObj `shouldBe` Just (String "base64")
              KM.lookup "media_type" srcObj `shouldBe` Just (String "image/png")
              KM.lookup "data" srcObj `shouldBe` Just (String "abc123")
            _ -> expectationFailure "Expected source to be an object"
        _ -> expectationFailure "Expected JSON object"

  describe "WCBImage with URL source" $ do
    it "round-trips WCBImage with URL source" $ do
      let img = WCBImage (UrlImage "https://example.com/image.png")
      decode (encode img) `shouldBe` Just img

    it "encodes WCBImage URL with correct JSON structure" $ do
      let img = WCBImage (UrlImage "https://example.com/image.png")
      let json = decode (encode img) :: Maybe Value
      case json of
        Just (Object obj) -> do
          KM.lookup "type" obj `shouldBe` Just (String "image")
          case KM.lookup "source" obj of
            Just (Object srcObj) -> do
              KM.lookup "type" srcObj `shouldBe` Just (String "url")
              KM.lookup "url" srcObj `shouldBe` Just (String "https://example.com/image.png")
            _ -> expectationFailure "Expected source to be an object"
        _ -> expectationFailure "Expected JSON object"


-- ════════════════════════════════════════════════════════════════════════════
-- EDGE CASE TESTS
-- ════════════════════════════════════════════════════════════════════════════

edgeCaseSpec :: Spec
edgeCaseSpec = describe "Edge cases" $ do

  describe "Empty strings" $ do
    it "roundtrips EffLogInfo with empty message" $ do
      let effect = EffLogInfo "" Nothing
      decode (encode effect) `shouldBe` Just effect

    it "roundtrips ResError with empty message" $ do
      let result = ResError ""
      decode (encode result) `shouldBe` Just result

    it "roundtrips PhaseFailed with empty error" $ do
      let phase = PhaseFailed ""
      decode (encode phase) `shouldBe` Just phase

  describe "Unicode and special characters" $ do
    it "roundtrips effect with emoji" $ do
      let effect = EffLogInfo "Status: \x1F600 \x1F4A9 \x2764" Nothing
      decode (encode effect) `shouldBe` Just effect

    it "roundtrips effect with null bytes" $ do
      let effect = EffLogInfo "before\x0000after" Nothing
      decode (encode effect) `shouldBe` Just effect

    it "roundtrips effect with newlines and tabs" $ do
      let effect = EffLogInfo "line1\nline2\ttabbed" Nothing
      decode (encode effect) `shouldBe` Just effect

    it "roundtrips effect with backslashes" $ do
      let effect = EffLogInfo "path\\to\\file" Nothing
      decode (encode effect) `shouldBe` Just effect

    it "roundtrips effect with quotes" $ do
      let effect = EffLogInfo "He said \"hello\"" Nothing
      decode (encode effect) `shouldBe` Just effect

    it "roundtrips effect with JSON-like content" $ do
      let effect = EffLogInfo "{\"key\": \"value\", \"nested\": {\"a\": 1}}" Nothing
      decode (encode effect) `shouldBe` Just effect

    it "roundtrips effect with very long string (10KB)" $ do
      let longStr = T.replicate 10000 "x"
          effect = EffLogInfo longStr Nothing
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
      let effect = EffLlmComplete "node" "sys" "user" Nothing Nothing
          json = decode (encode effect) :: Maybe Value
      case json of
        Just (Object obj) -> KM.lookup "eff_schema" obj `shouldBe` Nothing
        _ -> expectationFailure "Expected JSON object"

    it "EffLlmComplete with Just Null schema includes eff_schema as null" $ do
      let effect = EffLlmComplete "node" "sys" "user" (Just Null) Nothing
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
            [ EffLlmComplete "n" "s" "u" Nothing Nothing
            , EffLogInfo "msg" Nothing
            , EffLogError "err" Nothing
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
