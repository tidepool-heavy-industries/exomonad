# Data Flow

Type pipeline and data transformations in the Tidying agent.

## Complete Type Pipeline

```mermaid
flowchart TB
    subgraph Input["User Input"]
        I1["UserInput<br/>inputText: Maybe Text<br/>inputPhotos: [Photo]"]
    end

    subgraph State["Session State"]
        S1["SessionState<br/>mode: Mode<br/>piles: Piles<br/>spaceFunction: Maybe SpaceFunction<br/>anchors: [ItemName]<br/>itemsProcessed: Int"]
    end

    subgraph Mode["Mode (Sum Type)"]
        M1["Surveying SurveyingData"]
        M2["Sorting SortingData"]
        M3["Clarifying ClarifyingData"]
        M4["DecisionSupport DecisionSupportData"]
        M5["WindingDown WindingDownData"]
    end

    subgraph Template["Template Rendering"]
        T1["modeTemplate(mode)<br/>+ mode data as context"]
        T2["System Prompt"]
    end

    subgraph LLM["LLM Call"]
        L1["Multimodal call:<br/>system + history + photos + text"]
    end

    subgraph Output["Structured Output"]
        O1["Mode-specific schema<br/>{response, ...fields}"]
    end

    subgraph Update["State Update"]
        U1["Update mode data"]
        U2["Update session fields"]
    end

    I1 --> L1
    S1 --> T1
    M1 & M2 & M3 & M4 & M5 --> T1
    T1 --> T2 --> L1
    L1 --> O1
    O1 --> U1 --> S1
    O1 --> U2 --> S1
```

## Mode as Sum Type

Mode is NOT an enum - each variant carries its own data:

```mermaid
classDiagram
    class Mode {
        <<sum type>>
    }

    class SurveyingData {
        (empty - discoveries go to SessionState)
    }

    class SortingData {
        sdCurrentItem: Maybe Text
        sdItemLocation: Maybe Text
    }

    class ClarifyingData {
        cdItem: Text
        cdPhotoContext: Text
        cdReason: Text
    }

    class DecisionSupportData {
        dsdStuckItem: Text
    }

    class WindingDownData {
        wdSessionSummary: Maybe Text
        wdNextTime: [Text]
    }

    Mode <|-- SurveyingData : Surveying
    Mode <|-- SortingData : Sorting
    Mode <|-- ClarifyingData : Clarifying
    Mode <|-- DecisionSupportData : DecisionSupport
    Mode <|-- WindingDownData : WindingDown
```

## SessionState

```haskell
data SessionState = SessionState
  { mode :: Mode                          -- Sum type with mode-specific data
  , piles :: Piles                        -- belongs/out/unsure
  , itemsProcessed :: Int                 -- Progress count
  , sessionStart :: Maybe UTCTime
  -- Discovered in Surveying, persists across all modes:
  , spaceFunction :: Maybe SpaceFunction
  , anchors :: [ItemName]
  }

data Piles = Piles
  { belongs :: [ItemName]   -- Items that stay
  , out :: [ItemName]       -- Items to remove
  , unsure :: [ItemName]    -- Not yet classified
  }
```

## Mode-Specific Output Schemas

Each mode has its own structured output that the LLM produces:

```mermaid
flowchart TD
    subgraph Surveying["Surveying Output"]
        SV1["response: Text"]
        SV2["discovered_function: Maybe Text"]
        SV3["discovered_anchors: Maybe [Text]"]
    end

    subgraph Sorting["Sorting Output"]
        SO1["response: Text"]
        SO2["current_item: Maybe Text"]
        SO3["item_location: Maybe Text"]
    end

    subgraph Clarifying["Clarifying Output"]
        CL1["response: Text"]
        CL2["describing_item: Maybe Text"]
        CL3["spatial_refs: Maybe [Text]"]
        CL4["physical_traits: Maybe [Text]"]
    end

    subgraph DecisionSupport["DecisionSupport Output"]
        DS1["response: Text"]
        DS2["stuck_item: Maybe Text"]
        DS3["reframe_question: Maybe Text"]
    end

    subgraph WindingDown["WindingDown Output"]
        WD1["response: Text"]
        WD2["session_summary: Maybe Text"]
        WD3["next_time: Maybe [Text]"]
    end
```

## Output → Mode Data Updates

```mermaid
flowchart LR
    subgraph Output["LLM Output"]
        O["Structured Output"]
    end

    subgraph Update["Update Target"]
        U1["Mode Data Fields"]
        U2["Session-Level Fields"]
    end

    O -->|"current_item, item_location"| U1
    O -->|"discovered_function, discovered_anchors"| U2
```

| Mode | Output Fields | Updates |
|------|--------------|---------|
| Surveying | discovered_function, discovered_anchors | SessionState.spaceFunction, SessionState.anchors |
| Sorting | current_item, item_location | SortingData |
| Clarifying | describing_item, spatial_refs, physical_traits | ClarifyingData |
| DecisionSupport | stuck_item, reframe_question | DecisionSupportData |
| WindingDown | session_summary, next_time | WindingDownData |

## Transition Tool → Mode Data

When a transition tool is called, its arguments become the new mode's initial data:

```mermaid
flowchart LR
    subgraph Tool["Transition Tool Call"]
        T["need_to_clarify(item, photo_context, reason)"]
    end

    subgraph NewMode["New Mode"]
        M["Clarifying ClarifyingData"]
        D["cdItem = item<br/>cdPhotoContext = photo_context<br/>cdReason = reason"]
    end

    T --> M
    M --> D
```

## Input Types

```haskell
data UserInput = UserInput
  { inputPhotos :: [Photo]
  , inputText   :: Maybe Text
  }

data Photo = Photo
  { photoData :: Text  -- Base64 encoded or URL
  , photoMime :: Text  -- "image/jpeg", "image/png"
  }
```

## Template Context

Mode data becomes the context for Jinja template rendering:

```mermaid
flowchart TD
    subgraph Mode["Current Mode"]
        M["Clarifying ClarifyingData<br/>{ cdItem, cdPhotoContext, cdReason }"]
    end

    subgraph Session["Session State"]
        S["spaceFunction, anchors, piles, itemsProcessed"]
    end

    subgraph Context["Template Context"]
        C["{ item: cdItem<br/>  photoContext: cdPhotoContext<br/>  reason: cdReason<br/>  function: spaceFunction<br/>  anchors: anchors<br/>  piles: pilesSummary<br/>  itemsProcessed: itemsProcessed }"]
    end

    subgraph Template["Jinja Template"]
        T["mode_clarifying.jinja"]
    end

    subgraph Prompt["System Prompt"]
        P["Rendered prompt text"]
    end

    M --> C
    S --> C
    C --> T --> P
```

## Piles Updates

Tools update piles based on user decisions:

```mermaid
flowchart TD
    A["User responds to propose_disposition"]
    B{"Disposition?"}
    C["Trash/Donate/Recycle"]
    D["Item → piles.out"]
    E["PlaceAt/Keep"]
    F["Item → piles.belongs"]
    G["SkipForNow"]
    H["No change"]

    A --> B
    B -->|out| C --> D
    B -->|belongs| E --> F
    B -->|skip| G --> H
```

## Type Definitions

```haskell
-- Newtypes for type safety
newtype ItemName = ItemName Text
newtype SpaceFunction = SpaceFunction Text
newtype Location = Location Text

-- Mode sum type
data Mode
  = Surveying SurveyingData
  | Sorting SortingData
  | Clarifying ClarifyingData
  | DecisionSupport DecisionSupportData
  | WindingDown WindingDownData

-- Mode data types
data SurveyingData = SurveyingData
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data SortingData = SortingData
  { sdCurrentItem :: Maybe Text
  , sdItemLocation :: Maybe Text
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data ClarifyingData = ClarifyingData
  { cdItem :: Text
  , cdPhotoContext :: Text
  , cdReason :: Text
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data DecisionSupportData = DecisionSupportData
  { dsdStuckItem :: Text
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data WindingDownData = WindingDownData
  { wdSessionSummary :: Maybe Text
  , wdNextTime :: [Text]
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)
```

## Key Files

- `State.hs` - Mode sum type, mode data types, SessionState
- `Output.hs` - Mode-specific output schemas
- `Context.hs` - Template context building
- `Loop.hs` - Output parsing and state updates
