# Diagrams vs Actual System: Gap Analysis

This document captures discrepancies between the Mermaid diagrams and the actual codebase. Use this to understand what's simplified, omitted, or needs updating.

---

## 1. ItemDisposition Enum (Not in Diagrams)

The Question DSL uses `ItemDisposition` for tool-based disposition choices:

```haskell
data ItemDisposition
  = PlaceAt Text      -- Specific location
  | Trash             -- Garbage
  | Donate            -- Give away
  | Recycle           -- Recycling bin
  | SkipForNow        -- Put it back, come back later
  | NeedMoreInfo      -- Agent needs more context
```

**Design philosophy**: No `Unsure` option - "unsure pile is an antipattern that accumulates shame."

**Location**: `Question.hs:72-79`

---

## 2. Return Type Error in Diagrams

### Diagram Says:
```
chaosLevel :: Maybe PhotoAnalysis -> ChaosLevel
```

### Actual Code (`Decide.hs`):
```haskell
chaosLevel :: Maybe PhotoAnalysis -> Maybe ChaosLevel
chaosLevel = fmap (.paChaosLevel)
```

The function returns `Maybe ChaosLevel`, not `ChaosLevel`. This affects routing logic that must handle `Nothing`.

---

## 3. Overwhelm Detection (Not in Diagrams)

### `isOverwhelmedSignal` Function (`State.hs`)

```haskell
isOverwhelmedSignal :: Maybe Text -> Bool
isOverwhelmedSignal Nothing = False
isOverwhelmedSignal (Just t) = any (`T.isInfixOf` T.toLower t) signals
  where
    signals =
      [ "idk"
      , "don't know"
      , "no idea"
      , "where to start"
      , "overwhelm"
      , "too much"
      , "help"
      ]
```

This triggers fast-track to Sorting when user seems overwhelmed.

---

## 4. Split Suggestion Logic (Not in Diagrams)

### `suggestSplit` Function (`Decide.hs`)

Pattern-matches item names to suggest categories:

| Pattern | Categories |
|---------|------------|
| "cable", "cord" | `cables`, `other` |
| "paper", "document" | `papers`, `other` |
| "book" | `books`, `other` |
| "clothes", "shirt" | `clothes`, `other` |
| (default) | `keep-maybe`, `donate-maybe` |

---

## 5. State Transition Helpers (Not in Diagrams)

### `transitionPhaseData` (`Loop.hs`)

The actual state machine transition logic. Pattern matching on `(PhaseData, Phase)` pairs.

Key transitions not fully shown in diagrams:
- `Surveying → Sorting`: Creates `ActiveState` from gathered data
- `Sorting → Splitting`: Creates category list from `InstructSplit` action
- `Splitting → Refining`: Distributes unsure items to first category
- `Any → DecisionSupport`: Stores return phase for later

### `updatePilesFromExtract` (`Loop.hs`)

Updates piles based on `Extract.exIntent` and `Extract.exChoice`.

### `extractNextCategory` (`Loop.hs`)

Parses category name from `AckProgress "Let's do cables"` → `CategoryName "cables"`.

---

## 6. Additional Tools (Partially Diagrammed)

### AskSpaceFunction Tool (`Tools.hs`)

- Presents choice of space functions: workspace, creative, bedroom, storage, living
- Syncs choice to `SessionState.phaseData` via `updateFunctionInState`
- Emits `FunctionChosen` event

### ConfirmDone Tool (`Tools.hs`)

- Shows items processed count
- Emits `SessionConfirmedDone` event on confirmation

### ProposedChoice Type (`Tools.hs`)

```haskell
data ProposedChoice = ProposedChoice
  { pcLabel :: Text        -- "Kitchen counter"
  , pcDisposition :: Text  -- "kitchen" or "trash"
  , pcIsTrash :: Bool      -- True if trash option
  , pcIsDonate :: Bool     -- True if donate option
  }
```

---

## 7. Vision Prompts (Not in Diagrams)

### System Prompt (`Loop.hs`)

```
You are a tidying assistant analyzing a photo of a space.
Your job is to understand what you see and identify the best first step.

Be specific about visible items - mention actual objects, not vague categories.
For 'blocked_function', describe what the mess prevents (sitting, working, sleeping).
For 'first_target', pick something quick to clear that would create visible progress.
```

### User Prompt (`Loop.hs`)

```
Analyze this space. What do you see? How messy is it? What should we tackle first?
```

---

## 8. TidyingEvent Full List (Partial in Diagrams)

### All 11 Events (`Events.hs`)

| Event | Description |
|-------|-------------|
| `PhotoAnalyzed Text` | Photo was analyzed |
| `SituationClassified Text` | Situation/intent classified |
| `ActionTaken Action` | Action was decided |
| `PhaseChanged Phase Phase` | Phase transition (from, to) |
| `SessionEnded Int` | Session ended with item count |
| `UserInputReceived Text` | User input for chat display |
| `ResponseGenerated Text` | Response for chat display |
| `ItemProposed Text [Text]` | Tool: item + dispositions |
| `UserConfirmed Text Text` | Tool: item + chosen disposition |
| `UserCorrected Text Text` | Tool: item + user-provided location |
| `FunctionChosen Text` | Tool: space function selected |
| `SessionConfirmedDone` | Tool: user confirmed done |

---

## 9. Question DSL Details (Partial in Diagrams)

### ConditionalQ (Not Implemented in GUI)

```haskell
| ConditionalQ
    { cqWhen :: Condition
    , cqThen :: Question
    }
```

Code exists but GUI renders unconditionally.

### Condition Type

```haskell
data Condition
  = AnswerEquals Text Text    -- question_id, value
  | AnswerContains Text Text  -- question_id, substring
  | And [Condition]
  | Or [Condition]
  | Not Condition
```

### QuestionGroup (Only First Rendered)

```haskell
| QuestionGroup
    { qgLabel :: Maybe Text
    , qgQuestions :: [Question]
    }
```

GUI only renders first question, drops the rest.

### Choose Reveals (Not Implemented)

```haskell
| Choose
    { chReveals :: Map Text [Question]  -- Option-as-key nesting
    }
```

The `chReveals` map exists but GUI ignores it.

---

## 10. Action Classification (Not in Diagrams)

### `isQuestion` (`Action.hs`)

```haskell
isQuestion :: Action -> Bool
isQuestion AskFunction = True
isQuestion AskAnchors = True
isQuestion AskWhatIsIt = True
isQuestion AskWhereLive = True
isQuestion (AskItemDecision _) = True
isQuestion EnergyCheck = True
isQuestion _ = False
```

### `isInstruction` (`Action.hs`)

```haskell
isInstruction :: Action -> Bool
isInstruction FirstInstruction = True
isInstruction InstructTrash = True
isInstruction (InstructPlace _) = True
isInstruction InstructUnsure = True
isInstruction InstructNext = True
isInstruction InstructBag = True
isInstruction (InstructSplit _) = True
isInstruction _ = False
```

### `needsLLM` (`Action.hs`)

| Returns True | Returns False (Canned) |
|--------------|------------------------|
| FirstInstruction | InstructTrash |
| DecisionAid _ | InstructUnsure |
| PivotAway _ _ | InstructNext |
| Summary | InstructBag |
| AckProgress _ | InstructPlace _ |
| InstructSplit _ | AskFunction, etc. |

---

## 11. Constants (Not in Diagrams)

| Constant | Value | Location |
|----------|-------|----------|
| `unsureThreshold` | 5 | `Decide.hs` |
| Question handler timeout | 5 minutes (300000000 μs) | `GUI/Runner.hs` |
| Polling interval | 100ms | `GUI/App.hs` |

---

## Summary: What Diagrams Should Add

1. **ItemDisposition** enum
2. **Fix** `chaosLevel` return type to `Maybe ChaosLevel`
3. **Overwhelm signals** list
4. **Split suggestion patterns** table
5. **AskSpaceFunction** and **ConfirmDone** tool flows
6. **Full TidyingEvent** list
7. **Constants** reference
8. **Question DSL limitations** (reveals/conditional not implemented)

---

## Summary: What Code Should Fix

1. **Fix QuestionGroup** to render all questions OR remove from type
2. **Implement Choose reveals** OR remove from type
3. **Implement ConditionalQ** evaluation OR remove from type
