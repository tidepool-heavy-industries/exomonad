# Task 03: Format Conversion

**Status:** Pending
**Assignee:** TBD
**Dependencies:** 01-foundation
**Blocks:** 05-execute-wrapper

## Objective

Convert Anthropic Messages API responses to FunctionGemma JSONL training format.

## Deliverables

### 1. Convert.hs Module

Create `haskell/dsl/teaching/src/Tidepool/Teaching/Convert.hs`

### 2. Key Functions

```haskell
-- Extract all components from Anthropic response
extractTeachingTurn :: Value -> Either Text TeachingTurn
data TeachingTurn = TeachingTurn
  { ttReasoning :: Text      -- Combined text blocks
  , ttToolName :: Text       -- Tool invoked
  , ttToolArgs :: Value      -- Tool arguments
  }

-- Format as FunctionGemma conversation
anthropicToFunctionGemma
  :: Text          -- User turn (context)
  -> TeachingTurn  -- Extracted from Anthropic
  -> Text          -- Complete JSONL line ({"text": "..."})

-- Format function call in FunctionGemma syntax
formatFunctionCall :: Text -> Value -> Text
-- Example: select_symbols{selected:<escape>Foo,Bar<escape>}
```

### 3. FunctionGemma Format

```haskell
formatGemmaConversation :: Text -> TeachingTurn -> Text
formatGemmaConversation userTurn turn = T.unlines
  [ "<start_of_turn>developer"
  , "You are a semantic code analysis assistant."
  , "You have access to the following functions:"
  , ""
  , "<start_function_declaration>" <> turn.ttToolName
  , "Select relevant symbols from candidates."
  , "Parameters:"
  , "  selected (array): Array of selected symbols. (required)"
  , "<end_function_declaration>"
  , ""
  , "<end_of_turn>"
  , "<start_of_turn>user"
  , userTurn
  , "<end_of_turn>"
  , "<start_of_turn>model"
  , "/* " <> turn.ttReasoning <> " */"  -- Reasoning → inline comment
  , "<start_function_call>"
  , "call:" <> formatFunctionCall turn.ttToolName turn.ttToolArgs
  , "<end_function_call>"
  , "<end_of_turn>"
  ]
```

### 4. Function Call Formatting

Handle different argument types:
```haskell
formatFunctionCall toolName args =
  let argsText = case args of
        Object obj -> formatObjectArgs obj
        _ -> error "Tool args must be object"
  in toolName <> "{" <> argsText <> "}"

formatObjectArgs :: Object -> Text
formatObjectArgs obj = T.intercalate "," $ map formatField $ H.toList obj
  where
    formatField (k, v) = k <> ":<escape>" <> formatValue v <> "<escape>"

formatValue :: Value -> Text
formatValue (String s) = s
formatValue (Array arr) = T.intercalate "," (map formatValue $ V.toList arr)
formatValue (Number n) = T.pack $ show n
formatValue (Bool b) = if b then "true" else "false"
formatValue Null = "null"
formatValue (Object _) = error "Nested objects not supported"
```

### 5. JSONL Wrapper

```haskell
wrapAsJSONL :: Text -> Text
wrapAsJSONL conversation =
  let encoded = encode $ object ["text" .= conversation]
  in decodeUtf8 $ BL.toStrict encoded
```

## Acceptance Criteria

- [ ] Builds: `cabal build tidepool-teaching`
- [ ] Converts Anthropic responses to FunctionGemma format
- [ ] Inline comments preserve reasoning
- [ ] Function calls use correct `<escape>` delimiters
- [ ] Output is valid JSONL (one JSON object per line)
- [ ] Unit tests pass (see below)

## Testing

Create `test/Tidepool/Teaching/ConvertSpec.hs`:

```haskell
spec :: Spec
spec = do
  describe "extractTeachingTurn" $ do
    it "extracts reasoning and tool use" $ do
      let resp = [aesonQQ|
            { "content": [
                {"type": "text", "text": "Analyzing..."},
                {"type": "tool_use", "name": "select_symbols",
                 "input": {"selected": ["Foo", "Bar"]}}
              ]
            }
          |]
      let Right turn = extractTeachingTurn resp
      turn.ttReasoning `shouldBe` "Analyzing..."
      turn.ttToolName `shouldBe` "select_symbols"

  describe "anthropicToFunctionGemma" $ do
    it "formats complete conversation" $ do
      let turn = TeachingTurn
            { ttReasoning = "Selecting symbols..."
            , ttToolName = "select_symbols"
            , ttToolArgs = object ["selected" .= ["Foo", "Bar"]]
            }
      let gemma = anthropicToFunctionGemma "Context here" turn
      gemma `shouldContain` "/* Selecting symbols... */"
      gemma `shouldContain` "call:select_symbols{selected:<escape>Foo,Bar<escape>}"
      gemma `shouldContain` "<start_of_turn>model"

  describe "formatFunctionCall" $ do
    it "formats array arguments with escape delimiters" $ do
      let args = object ["selected" .= ["A", "B", "C"]]
      formatFunctionCall "select_symbols" args `shouldBe`
        "select_symbols{selected:<escape>A,B,C<escape>}"
```

## Reference Implementation

See existing patterns in:
- `haskell/tools/training-generator/src/Tidepool/Training/Format.hs`
- Look for `formatGemmaConversation`, `formatCodeExample`

## Integration Point

Consumed by Task 05 (Execute.hs):
```haskell
-- After Anthropic call:
let gemmaLine = anthropicToFunctionGemma userPrompt turn
```

## Estimated Effort

2-3 hours (text formatting + tests)

## Notes

- Multiple text blocks should be concatenated with `\n`
- `<escape>` delimiters are critical (Haskell code has brackets/commas)
- Validate JSONL output manually: `jq . output.jsonl`
- Follow FunctionGemma 3-turn format (developer, user, model)
