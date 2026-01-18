# Task 05: Tool Execution Wrapper

**Status:** Complete
**Assignee:** Claude Sonnet 4.5
**Dependencies:** 02-anthropic-client, 03-format-conversion, 04-recording
**Blocks:** 06-scout-integration

## Objective

Create the main execution wrapper that orchestrates teaching mode (integrates Tasks 02-04).

## Deliverables

### 1. Execute.hs Module

Create `haskell/dsl/teaching/src/Tidepool/Teaching/Execute.hs`

### 2. Main Execution Function

```haskell
-- Execute tool with optional teaching
executeWithTeaching
  :: forall tool effect es.
     ( ToolDef tool
     , FineTrainingTeacher effect
     , Member effect es
     , LastMember IO es
     )
  => TeachingConfig
  -> RecordingHandles
  -> tool
  -> ToolInput tool
  -> Eff es (ToolOutput tool)
```

### 3. Implementation Flow

```haskell
executeWithTeaching config handles tool input
  -- Production mode: skip teaching
  | not (tcEnabled config) = toolExecute tool input

  -- Teaching mode: record Haiku execution
  | otherwise = do
      -- 1. Build user prompt from input
      let userPrompt = buildToolPrompt tool input

      -- 2. Get teacher guidance
      let guidance = teacherGuidance @effect
      let systemPrompt = baseSystemPrompt <> "\n\n" <> guidance

      -- 3. Call Haiku
      anthropicResp <- sendM $ callAnthropicMessages
        (tcAnthropicKey config)
        systemPrompt
        userPrompt
        [toAnthropicTool tool]

      case anthropicResp of
        Left err -> error $ "Haiku API failed: " <> T.unpack err
        Right rawJson -> do
          -- 4. Extract teaching turn
          case extractTeachingTurn rawJson of
            Left parseErr -> error $ "Parse failed: " <> T.unpack parseErr
            Right turn -> do
              -- 5. Convert to FunctionGemma format
              let gemmaLine = anthropicToFunctionGemma userPrompt turn

              -- 6. Record both formats
              now <- sendM getCurrentTime
              let example = TrainingExample
                    { teAnthropicRaw = rawJson
                    , teFunctionGemmaFormatted = gemmaLine
                    , teTeacherGuidance = Just guidance
                    , teTimestamp = now
                    , teToolName = turn.ttToolName
                    }
              sendM $ recordExample handles example

              -- 7. Parse tool output and continue execution
              case fromJSON turn.ttToolArgs of
                Error err -> error $ "Tool output parse failed: " <> err
                Success output -> pure output
```

### 4. User Prompt Building

```haskell
-- Build context string from tool input
buildToolPrompt :: forall tool. (ToolDef tool, ToJSON (ToolInput tool)) => tool -> ToolInput tool -> Text
buildToolPrompt tool input =
  let inputJson = toJSON input
      toolDesc = toolDescription tool
  in T.unlines
    [ "Tool: " <> toolName tool
    , "Description: " <> toolDesc
    , ""
    , "Input:"
    , TL.toStrict $ TL.decodeUtf8 $ encode inputJson
    ]
```

### 5. Error Handling

```haskell
-- Wrapper with better error context
executeWithTeachingSafe
  :: (ToolDef tool, FineTrainingTeacher effect, Member effect es, LastMember IO es)
  => TeachingConfig
  -> RecordingHandles
  -> tool
  -> ToolInput tool
  -> Eff es (Either TeachingError (ToolOutput tool))

data TeachingError
  = HaikuAPIError Text
  | ParseError Text
  | ToolOutputError Text
  deriving (Show, Eq)
```

## Acceptance Criteria

- [ ] Builds: `cabal build tidepool-teaching`
- [ ] Production mode (tcEnabled=False) calls toolExecute directly
- [ ] Teaching mode calls Haiku, records, parses output
- [ ] Integrates all three modules (Anthropic, Convert, Record)
- [ ] Error messages are clear and actionable
- [ ] Integration test passes (see below)

## Testing

Create `test/Tidepool/Teaching/ExecuteSpec.hs`:

```haskell
-- This requires a mock tool and effect
data MockTool = MockTool
data MockEffect a where
  MockOp :: Text -> MockEffect Text

instance ToolDef MockTool where
  type ToolInput MockTool = MockInput
  type ToolOutput MockTool = MockOutput
  type ToolEffects MockTool = '[MockEffect]
  toolName _ = "mock_tool"
  toolDescription _ = "Mock tool for testing"
  toolExecute _ input = pure MockOutput { result = input.value }

instance FineTrainingTeacher MockEffect where
  teacherGuidance = "Mock guidance for testing"

spec :: Spec
spec = do
  describe "executeWithTeaching" $ do
    it "uses normal execution when disabled" $ do
      let config = TeachingConfig
            { tcEnabled = False
            , tcOutputDir = "/tmp"
            , tcSessionId = nil
            , tcAnthropicKey = ""
            }
      -- Test that it calls toolExecute directly
      -- (This is tricky - may need to mock the effect interpreter)

    it "records when enabled" $ do
      -- This requires mocking Anthropic API
      -- OR use integration test with real API (Task 07)
      pending "Integration test in Task 07"
```

**Note:** Full E2E test will be in Task 07 after CLI integration.

## Integration Point

Consumed by Task 06 (Scout ToolDef wrapper):
```haskell
-- In SelectSymbolsTool's toolExecute:
toolExecute _ input = executeWithTeaching config handles SelectSymbolsTool input
```

## Dependencies

Module imports:
```haskell
import Tidepool.Teaching.Types
import Tidepool.Teaching.Teacher
import Tidepool.Teaching.Anthropic
import Tidepool.Teaching.Convert
import Tidepool.Teaching.Record
import Tidepool.Graph.Tool (ToolDef(..))
import Tidepool.Tool.Convert (toAnthropicTool)
```

## Estimated Effort

3-4 hours (integration logic + error handling)

## Notes

- This is the critical integration point - all prior work comes together here
- Error handling should preserve context (which step failed)
- Consider adding debug logging for each step
- User prompt formatting should be clear (helps Haiku understand task)
- Parse failures should include both error message AND raw response
