# Task 01: Foundation (Types + Typeclass)

**Status:** Complete (PR #219)
**Assignee:** Claude Sonnet 4.5
**Dependencies:** None
**Blocks:** 02, 03, 04

## Objective

Create the `tidepool-teaching` package with core types and the minimal `FineTrainingTeacher` typeclass.

## Deliverables

### 1. Package Structure

Create `haskell/dsl/teaching/` with:
- `tidepool-teaching.cabal`
- `CLAUDE.md` (architecture documentation)
- `src/Tidepool/Teaching/Types.hs`
- `src/Tidepool/Teaching/Teacher.hs`

### 2. Core Types (Types.hs)

```haskell
-- Training example pair
data TrainingExample = TrainingExample
  { teAnthropicRaw :: Value              -- Raw Anthropic response
  , teFunctionGemmaFormatted :: Text     -- Converted JSONL line
  , teTeacherGuidance :: Maybe Text      -- Domain context
  , teTimestamp :: UTCTime
  , teToolName :: Text
  } deriving (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- Global teaching config
data TeachingConfig = TeachingConfig
  { tcEnabled :: Bool
  , tcOutputDir :: FilePath
  , tcSessionId :: UUID
  , tcAnthropicKey :: Text
  } deriving (Show, Eq)

-- Recording file handles
data RecordingHandles = RecordingHandles
  { rhRawHandle :: Handle      -- anthropic.jsonl
  , rhGemmaHandle :: Handle    -- gemma.jsonl
  , rhSessionDir :: FilePath
  }
```

### 3. Typeclass (Teacher.hs)

```haskell
{-# LANGUAGE AllowAmbiguousTypes #-}

-- Minimal typeclass - just guidance text
class FineTrainingTeacher effect where
  teacherGuidance :: Text

-- Base system prompt (all teaching sessions)
baseSystemPrompt :: Text
baseSystemPrompt = "You are a semantic code analysis assistant..."
```

### 4. Cabal File

```cabal
cabal-version: 2.2
name: tidepool-teaching
version: 0.1.0.0

library
  exposed-modules:
    Tidepool.Teaching.Types
    Tidepool.Teaching.Teacher
  hs-source-dirs: src
  build-depends:
      base >=4.14
    , aeson
    , text
    , time
    , uuid
    , bytestring
  default-language: Haskell2010
  default-extensions:
    DeriveGeneric
    DeriveAnyClass
    OverloadedStrings
```

### 5. Update cabal.project

Add to `packages:` list:
```
haskell/dsl/teaching
```

### 6. Documentation (CLAUDE.md)

Create comprehensive doc explaining:
- Architecture overview
- ADR 001 reference
- Module purposes
- Usage examples
- Integration points

## Acceptance Criteria

- [ ] Package builds: `cabal build tidepool-teaching`
- [ ] Types compile without errors
- [ ] Typeclass has clear haddock comments
- [ ] CLAUDE.md explains architecture
- [ ] cabal.project includes new package
- [ ] No runtime dependencies (pure types)

## Testing

No tests needed yet (pure types, no logic).

## Estimated Effort

1-2 hours (straightforward type definitions)

## Notes

- Keep types simple - no business logic
- RecordingHandles will be used by Task 04
- TeachingConfig will be threaded through Tasks 05-07
- Use AllowAmbiguousTypes for typeclass (no Proxy needed)
