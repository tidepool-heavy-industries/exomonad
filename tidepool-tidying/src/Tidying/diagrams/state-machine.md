# State Machine

Phase states, transitions, and PhaseData types for the Tidying agent.

## Phase State Machine

```mermaid
stateDiagram-v2
    [*] --> Surveying: newSession

    Surveying --> Surveying: gathering function/anchors
    Surveying --> Sorting: hasFunction (+ hasAnchors OR overwhelmed)

    Sorting --> Sorting: processing items
    Sorting --> Splitting: unsureCount >= 5
    Sorting --> DecisionSupport: item stuck
    Sorting --> [*]: IntentStop → Summary

    Splitting --> Sorting: split confirmed

    Refining --> Refining: processing category items
    Refining --> DecisionSupport: item stuck
    Refining --> Sorting: category done, more unsure
    Refining --> [*]: all categories done

    DecisionSupport --> Sorting: decision made (typical)
    DecisionSupport --> Refining: decision made (if came from Refining)
```

## PhaseData Types

The `ActiveState` record factors out common fields shared by all phases except Surveying:

```mermaid
classDiagram
    class ActiveState {
        asFunction: SpaceFunction
        asAnchors: [ItemName]
    }

    class SurveyingData {
        sdGatheredFunction: Maybe SpaceFunction
        sdGatheredAnchors: [ItemName]
    }

    class SortingData {
        soActive: ActiveState
        soCurrentItem: Maybe ItemName
    }

    class SplittingData {
        spActive: ActiveState
        spCategories: NonEmpty CategoryName
    }

    class RefiningData {
        rfActive: ActiveState
        rfEmergentCats: Map CategoryName [ItemName]
        rfCurrentCategory: CategoryName
        rfCurrentItem: Maybe ItemName
    }

    class DecisionSupportData {
        dsActive: ActiveState
        dsStuckItem: ItemName
        dsReturnPhase: Phase
    }

    SortingData --> ActiveState
    SplittingData --> ActiveState
    RefiningData --> ActiveState
    DecisionSupportData --> ActiveState
```

## Transition Triggers

| From | To | Trigger |
|------|-----|---------|
| Surveying | Surveying | AskFunction, AskAnchors |
| Surveying | Sorting | Buried chaos |
| Surveying | Sorting | hasBlockedFunction + hasFunction |
| Surveying | Sorting | overwhelmed signal + hasFunction |
| Surveying | Sorting | hasFunction + hasAnchors |
| Sorting | Sorting | InstructTrash, InstructPlace, InstructUnsure, InstructNext |
| Sorting | Splitting | ChoiceUnsure + unsureCount >= 5 |
| Sorting | DecisionSupport | IntentHelp or Stuck |
| Splitting | Sorting | Split confirmed |
| Refining | Refining | Processing category |
| Refining | DecisionSupport | IntentHelp |
| DecisionSupport | [return] | Decision made |
| Any | Summary | IntentStop |

## Action Types

```mermaid
mindmap
  root((Action))
    Questions
      AskFunction
      AskAnchors
      AskWhatIsIt
      AskWhereLive
      AskItemDecision ItemName
      EnergyCheck
    Instructions
      FirstInstruction
      InstructTrash
      InstructPlace Location
      InstructUnsure
      InstructNext
      InstructBag
    Splitting
      InstructSplit NonEmpty CategoryName
    DecisionSupport
      DecisionAid ItemName
    Pivoting
      PivotAway AnxietyTrigger Location
    Completion
      AckProgress Text
      Summary
```

## Response Generation

All responses go through LLM for natural, context-aware conversation.
No canned responses - the Action just informs the system prompt.

Key actions that use tools:
- `AskFunction` → calls `ask_space_function` tool for UI choices
- `AskItemDecision` → calls `propose_disposition` tool with thoughtful options
- `FirstInstruction` → calls `propose_disposition` for first item

## Split Suggestion Patterns

When unsure pile >= 5, `suggestSplit` pattern-matches items to suggest categories:

| Pattern | Suggested Categories |
|---------|---------------------|
| `cable`, `cord` | `cables`, `other` |
| `paper`, `document` | `papers`, `other` |
| `book` | `books`, `other` |
| `clothes`, `shirt` | `clothes`, `other` |
| (default) | `keep-maybe`, `donate-maybe` |

## Constants

| Constant | Value | Location |
|----------|-------|----------|
| `unsureThreshold` | 5 items | `Decide.hs` |
| Question handler timeout | 5 minutes | `GUI/Runner.hs` |
| Polling interval | 100ms | `GUI/App.hs` |

## Key Files

- `State.hs` - Phase, PhaseData, ActiveState, SessionState
- `Decide.hs` - decideFromExtract, suggestSplit
- `Action.hs` - Action ADT
