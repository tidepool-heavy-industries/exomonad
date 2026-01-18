# Task 02: Anthropic Messages API Client

**Status:** Complete (re-export wrapper)
**Assignee:** Claude Sonnet 4.5
**Dependencies:** 01-foundation
**Blocks:** 05-execute-wrapper

## Objective

Implement HTTP client for Anthropic Messages API with tool use support.

## Deliverables

### 1. Anthropic.hs Module

Create `haskell/dsl/teaching/src/Tidepool/Teaching/Anthropic.hs`

### 2. Key Functions

```haskell
-- Main API call
callAnthropicMessages
  :: Text                    -- API key
  -> Text                    -- System prompt (base + guidance)
  -> Text                    -- User prompt (tool context)
  -> [AnthropicTool]         -- Available tools
  -> IO (Either Text Value)  -- Raw response or error

-- Parse tool use from response
parseToolUseResponse :: Value -> Either Text (Text, Value)
-- Returns: (toolName, arguments JSON)

-- Extract reasoning text from response
extractReasoningText :: Value -> Maybe Text
-- Returns: text content blocks (if any)
```

### 3. Request Format

```haskell
buildRequest :: Text -> Text -> [AnthropicTool] -> Value
buildRequest systemPrompt userPrompt tools = object
  [ "model" .= ("claude-3-5-haiku-20241022" :: Text)
  , "max_tokens" .= (4096 :: Int)
  , "system" .= [object
      [ "type" .= ("text" :: Text)
      , "text" .= systemPrompt
      , "cache_control" .= object ["type" .= ("ephemeral" :: Text)]
      ]]
  , "messages" .= [object
      [ "role" .= ("user" :: Text)
      , "content" .= userPrompt
      ]]
  , "tools" .= tools
  ]
```

### 4. Response Parsing

Handle Anthropic response format:
```json
{
  "role": "assistant",
  "content": [
    {"type": "text", "text": "Reasoning..."},
    {
      "type": "tool_use",
      "id": "toolu_123",
      "name": "select_symbols",
      "input": {"selected": ["Foo", "Bar"]}
    }
  ]
}
```

### 5. Error Handling

```haskell
data AnthropicError
  = NetworkError Text
  | APIError Int Text      -- HTTP status + message
  | ParseError Text        -- JSON decode failure
  | NoToolUse             -- Response missing tool_use block
  deriving (Show, Eq)
```

## Acceptance Criteria

- [ ] Builds: `cabal build tidepool-teaching`
- [ ] HTTP POST to api.anthropic.com/v1/messages works
- [ ] Tool use responses parse correctly
- [ ] Reasoning text extraction works
- [ ] Error cases handled gracefully
- [ ] Unit tests pass (see below)

## Testing

Create `test/Tidepool/Teaching/AnthropicSpec.hs`:

```haskell
spec :: Spec
spec = do
  describe "parseToolUseResponse" $ do
    it "extracts tool name and arguments" $ do
      let response = [aesonQQ|
            { "content": [
                {"type": "text", "text": "Reasoning"},
                {"type": "tool_use", "name": "select_symbols",
                 "input": {"selected": ["Foo", "Bar"]}}
              ]
            }
          |]
      parseToolUseResponse response `shouldBe`
        Right ("select_symbols", object ["selected" .= ["Foo", "Bar"]])

  describe "extractReasoningText" $ do
    it "extracts text content blocks" $ do
      let response = [aesonQQ|
            { "content": [
                {"type": "text", "text": "First"},
                {"type": "text", "text": "Second"}
              ]
            }
          |]
      extractReasoningText response `shouldBe` Just "First\nSecond"
```

## Dependencies

Add to `tidepool-teaching.cabal`:
```
build-depends:
  , req           -- HTTP client
  , http-client
  , http-types
```

## Integration Point

Consumed by Task 05 (Execute.hs):
```haskell
-- In executeWithTeaching:
anthropicResp <- sendM $ callAnthropicMessages
  apiKey systemPrompt userPrompt tools
```

## Estimated Effort

3-4 hours (HTTP client + parsing)

## Notes

- Use `req` library (same as llm-interpreter)
- Cache system prompt with `cache_control: ephemeral`
- Return raw `Value` (don't parse tool args yet - that's Task 05)
- Include retry logic for transient errors (3 retries, exponential backoff)
