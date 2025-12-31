# Data Flow

Type transformations through the Tidying agent pipeline.

## Complete Type Pipeline

```mermaid
flowchart TB
    subgraph Input["User Input"]
        I1["UserInput<br/>inputText: Maybe Text<br/>inputPhotos: [Photo]"]
    end

    subgraph Observe["OBSERVE"]
        O1["Photo<br/>photoData, photoMime"]
        O2["PhotoAnalysisOutput<br/>(LLM output)"]
        O3["PhotoAnalysis<br/>paRoomType, paChaosLevel<br/>paVisibleItems, paBlockedFunction<br/>paFirstTarget"]
    end

    subgraph Orient["ORIENT"]
        R1["Extract<br/>exIntent: Intent<br/>exItem: Maybe ItemName<br/>exChoice: Maybe Choice<br/>exPlace: Maybe Location<br/>exFunction: Maybe SpaceFunction<br/>exAnchors: Maybe [ItemName]"]
    end

    subgraph Decide["DECIDE"]
        D1["SessionState<br/>phaseData, piles<br/>itemsProcessed, sessionStart"]
        D2["Action<br/>(20+ constructors)"]
        D3["Phase<br/>(5 values)"]
    end

    subgraph Context["CONTEXT"]
        C1["TidyingContext<br/>tcPhase, tcFunction<br/>tcAnchors, tcPiles<br/>tcEmergentCategories<br/>tcCurrentCategory<br/>tcPhotoAnalysis<br/>tcUserText"]
    end

    subgraph Act["ACT"]
        A1["ActOutput<br/>aoResponse: Text<br/>aoSuggestedSplits: [Text]"]
        A2["Response<br/>responseText, responsePhase<br/>responseItemsProcessed<br/>responseSessionEnded"]
    end

    I1 --> O1 --> O2 --> O3
    I1 --> R1
    O3 --> R1
    R1 --> D2
    D1 --> D2
    O3 --> D2
    D2 --> D3
    D1 --> C1
    O3 --> C1
    C1 --> A1
    D2 --> A1
    A1 --> A2
```

## Intent → Action Mapping (Sorting Phase)

```mermaid
flowchart TD
    I["Extract.exIntent"]

    I -->|IntentDecided| C["Check exChoice"]
    C -->|ChoiceTrash| AT["InstructTrash"]
    C -->|ChoicePlace| AP["InstructPlace location"]
    C -->|ChoiceKeep| AK["InstructPlace 'keep pile'"]
    C -->|ChoiceUnsure| U{"unsureCount >= 5?"}
    U -->|yes| AS["InstructSplit"]
    U -->|no| AU["InstructUnsure"]

    I -->|IntentItem| AI["AskItemDecision"]
    I -->|IntentContinue| AN["InstructNext"]
    I -->|IntentHelp| AH["DecisionAid"]
    I -->|IntentStop| SUM["Summary"]
```

## Pile Updates

```mermaid
flowchart TD
    A["Extract with choice"]
    B{"exChoice?"}
    C["ChoiceTrash"]
    D["Item → out pile"]
    E["ChoicePlace/Keep"]
    F["Item → belongs pile"]
    G["ChoiceUnsure"]
    H["Item → unsure pile"]
    I{"Action == Split?"}
    J["Clear unsure pile<br/>→ emergent categories"]

    A --> B
    B -->|trash| C --> D
    B -->|place/keep| E --> F
    B -->|unsure| G --> H
    B -->|no choice| I
    I -->|yes| J
```

## Type Reference

### Input Types

```haskell
data UserInput = UserInput
  { inputPhotos :: [Photo]
  , inputText   :: Maybe Text
  }

data Photo = Photo
  { photoData :: Text  -- Base64 or URL
  , photoMime :: Text  -- "image/jpeg", "image/png"
  }
```

### Photo Analysis Types

```haskell
data PhotoAnalysis = PhotoAnalysis
  { paRoomType        :: Text
  , paChaosLevel      :: ChaosLevel  -- Buried | Cluttered | Moderate | Clear
  , paVisibleItems    :: [Text]
  , paBlockedFunction :: Maybe Text
  , paFirstTarget     :: Maybe Text
  }
```

### Extract Types

```haskell
data Extract = Extract
  { exIntent   :: Intent           -- REQUIRED
  , exItem     :: Maybe ItemName
  , exChoice   :: Maybe Choice
  , exPlace    :: Maybe Location
  , exFunction :: Maybe SpaceFunction
  , exAnchors  :: Maybe [ItemName]
  }

data Intent = IntentStart | IntentContinue | IntentItem
            | IntentDecided | IntentHelp | IntentStop

data Choice = ChoiceTrash | ChoiceKeep | ChoicePlace | ChoiceUnsure
```

### Output Types

```haskell
data Response = Response
  { responseText           :: Text
  , responsePhase          :: Phase
  , responseItemsProcessed :: Int
  , responseSessionEnded   :: Bool
  }
```

## Key Files

- `State.hs` - UserInput, Photo, SessionState, Piles
- `Context.hs` - TidyingContext, PhotoAnalysis
- `Output.hs` - Extract, Intent, Choice, ActOutput
- `Loop.hs` - Response, type transformations
