# Task 06: Scout Integration

**Status:** Pending
**Assignee:** TBD
**Dependencies:** 05-execute-wrapper
**Blocks:** 07-cli-integration

## Objective

Create ToolDef wrapper for TeachGemma effect and add FineTrainingTeacher instance.

## Deliverables

### 1. Tools.hs Module

Create `haskell/control-server/src/Tidepool/Control/Scout/Tools.hs`

### 2. ToolDef Wrapper

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Tidepool.Control.Scout.Tools
  ( SelectSymbolsTool(..)
  , SelectSymbolsInput(..)
  , SelectSymbolsOutput(..)
  ) where

import Tidepool.Graph.Tool (ToolDef(..))
import Tidepool.Tool.Convert (ToAnthropicTool, ToCfTool)
import Tidepool.Control.Scout.Teach.Gemma (selectRelevantSymbols)
import Tidepool.Control.Scout.Teach.Types (LSPSymbol(..))

-- Tool marker
data SelectSymbolsTool = SelectSymbolsTool

-- Input (flattened from effect args)
data SelectSymbolsInput = SelectSymbolsInput
  { topic :: Text
  , symbolName :: Text
  , symbolSignature :: Text
  , symbolLocation :: Text
  , candidates :: [Text]
  } deriving (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, HasJSONSchema)

-- Output
newtype SelectSymbolsOutput = SelectSymbolsOutput
  { selected :: [Text]
  } deriving (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, HasJSONSchema)

-- ToolDef instance
instance ToolDef SelectSymbolsTool where
  type ToolInput SelectSymbolsTool = SelectSymbolsInput
  type ToolOutput SelectSymbolsTool = SelectSymbolsOutput
  type ToolEffects SelectSymbolsTool = '[TeachGemma]

  toolName _ = "select_symbols"
  toolDescription _ = "Select relevant symbols from candidates for understanding a topic"

  toolExecute _ input = do
    -- Reconstruct LSPSymbol from flattened input
    let symbol = LSPSymbol
          { lsName = input.symbolName
          , lsSignature = input.symbolSignature
          , lsLocation = parseLocation input.symbolLocation
          , lsKind = SKFunction  -- Default (not critical for selection)
          , lsDocComment = Nothing
          }

    -- Bridge to effect
    selected <- selectRelevantSymbols input.topic symbol input.candidates
    pure SelectSymbolsOutput { selected }

-- Auto-derive conversions
instance ToAnthropicTool SelectSymbolsTool
instance ToCfTool SelectSymbolsTool

-- Helper: parse location string back to Location type
parseLocation :: Text -> Location
parseLocation loc = ... -- Implementation details
```

### 3. FineTrainingTeacher Instance

Create `haskell/control-server/src/Tidepool/Control/Scout/Teach/Teacher.hs`

```haskell
module Tidepool.Control.Scout.Teach.Teacher where

import Tidepool.Teaching.Teacher
import Tidepool.Control.Scout.Teach.Gemma (TeachGemma)

instance FineTrainingTeacher TeachGemma where
  teacherGuidance = T.unlines
    [ "# Symbol Selection Strategy"
    , ""
    , "When selecting symbols for understanding a topic:"
    , ""
    , "- **Prioritize symbols that break on changes:**"
    , "  - Exhaustive pattern matches (adding variant breaks all matches)"
    , "  - Type families and instances (changes affect type-level computation)"
    , "  - Hardcoded dependencies (changes require manual updates)"
    , ""
    , "- **Include core dependencies that explain behavior:**"
    , "  - Input types (what the function requires)"
    , "  - Output types (what the function produces)"
    , "  - Constraints (type class requirements)"
    , ""
    , "- **Avoid primitive/common types:**"
    , "  - Int, Text, String, ByteString, Bool"
    , "  - Maybe, Either, IO, List, Map, Set"
    , "  - Monad, Functor, Applicative"
    , ""
    , "Think step-by-step about semantic relationships and architectural impact."
    ]
```

### 4. Update control-server.cabal

Add new modules:
```
exposed-modules:
  ...
  Tidepool.Control.Scout.Tools
  Tidepool.Control.Scout.Teach.Teacher
```

Add dependency:
```
build-depends:
  ...
  , tidepool-teaching
```

## Acceptance Criteria

- [ ] Builds: `cabal build tidepool-control-server`
- [ ] SelectSymbolsTool compiles without errors
- [ ] ToAnthropicTool instance auto-derives
- [ ] FineTrainingTeacher instance provides clear guidance
- [ ] toolExecute bridges to TeachGemma effect correctly
- [ ] Unit test passes (see below)

## Testing

Create `test/Tidepool/Control/Scout/ToolsSpec.hs`:

```haskell
spec :: Spec
spec = do
  describe "SelectSymbolsTool" $ do
    it "has correct tool name" $ do
      toolName SelectSymbolsTool `shouldBe` "select_symbols"

    it "derives Anthropic tool schema" $ do
      let tool = toAnthropicTool SelectSymbolsTool
      tool.atName `shouldBe` "select_symbols"
      -- Check schema has required fields

  describe "TeachGemma FineTrainingTeacher" $ do
    it "provides teacher guidance" $ do
      let guidance = teacherGuidance @TeachGemma
      guidance `shouldContain` "Symbol Selection Strategy"
      guidance `shouldContain` "Prioritize symbols that break"
```

## Integration Point

Consumed by Task 07 (Handler/MCP.hs):
```haskell
-- In handleTeachTool:
let config = TeachingConfig { ... }
handles <- initRecording config.tcOutputDir config.tcSessionId

-- Run with teaching (implicit via executeWithTeaching)
result <- runM
  $ runTeachGemmaHTTP endpoint  -- Still uses HTTP interpreter
  $ runLSP session
  $ teach defaultTeachConfig query
```

**Note:** The teaching wrapper intercepts at the tool execution level, not the interpreter level.

## Estimated Effort

2-3 hours (wrapper code + tests)

## Notes

- LSPSymbol reconstruction may lose some fields - use sensible defaults
- Teacher guidance should be concise but specific
- ToAnthropicTool derives schema from SelectSymbolsInput's HasJSONSchema
- This bridges the "effect vs tool" gap cleanly
