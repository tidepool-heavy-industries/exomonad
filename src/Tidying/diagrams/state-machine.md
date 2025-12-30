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

## Canned vs LLM-Generated Responses

| Canned (no LLM) | LLM-Generated |
|-----------------|---------------|
| AskFunction → "What do you need to DO in this space?" | FirstInstruction |
| AskAnchors → "What's definitely staying no matter what?" | DecisionAid |
| AskWhatIsIt → "What is it?" | PivotAway |
| AskWhereLive → "Desk or somewhere else?" | Summary |
| AskItemDecision → "[item]. Trash, keep, or not sure?" | AckProgress |
| InstructTrash → "Trash. Toss it by the door. Next." | InstructSplit |
| InstructUnsure → "Unsure pile, floor to your right. Next." | |
| InstructNext → "Next thing." | |
| InstructBag → "Bag the trash by the door." | |
| InstructPlace → "[loc]. Next." | |
| EnergyCheck → "Energy check: keep going, or stop here for today?" | |

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
