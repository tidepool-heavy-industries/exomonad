# Tidying Agent - Complete Architecture Guide

A prosthetic executive function for tackling overwhelming spaces. Uses OODA (Observe-Orient-Decide-Act) pattern with tool-based LLM interaction.

## Quick Start

```bash
# Build
cabal build

# Run (requires ANTHROPIC_API_KEY)
ANTHROPIC_API_KEY=sk-... cabal run tidepool-tidy-gui

# Opens browser at localhost:8024
```

## Core Concept

The tidying agent helps users who feel overwhelmed by messy spaces:

1. **Analyzes photos** of the space (via Claude's vision)
2. **Proposes dispositions** for items (via tool calls)
3. **Gets user confirmation** with one tap
4. **Maintains momentum** without judgment

**Key insight**: Users with executive function challenges need external structure. The agent provides that structure while keeping them in control.

---

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────────┐
│                           tidyingTurn (OODA Loop)                        │
│                                                                          │
│  OBSERVE ──► ORIENT ──► DECIDE ──► ACT                                  │
│     │          │          │         │                                    │
│     │          │          │         └─► actResponse (canned or LLM)     │
│     │          │          │                    │                         │
│     │          │          │             ┌──────┴──────┐                  │
│     │          │          │             │ LLM + Tools │                  │
│     │          │          │             │             │                  │
│     │          │          │             │ propose_    │                  │
│     │          │          │             │ disposition │──► QuestionUI   │
│     │          │          │             └─────────────┘        │         │
│     │          │          │                                    ▼         │
│     │          │          └─► decideFromExtract ◄─────── User taps      │
│     │          │                  (pure routing)                         │
│     │          │                  + PhotoAnalysis                        │
│     │          │                                                         │
│     │          └─► extractFromInput (LLM extracts facts)                │
│     │                                                                    │
│     └─► analyzePhotos (LLM vision → ChaosLevel, firstTarget)           │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## Module Dependency Graph

```
Tidying.hs (re-exports)
    │
    ├── Types.hs ◄──────────── Newtypes: ItemName, Location, ChaosLevel, etc.
    │       ▲
    │       │ (used by all)
    │       │
    ├── State.hs ─────────────► PhaseData sum type, SessionState, Piles
    │       │
    ├── Agent.hs ──────────────► Loop.hs ──────────► Decide.hs
    │       │                       │                    │
    │       │                       │                    └─► Action.hs
    │       │                       │                    └─► Situation.hs
    │       │                       │
    │       │                       ├─► Tools.hs ──────► Question.hs (DSL)
    │       │                       │
    │       │                       ├─► Context.hs ────► PhotoAnalysis
    │       │                       │
    │       │                       ├─► Output.hs ─────► Extract, Intent, Choice
    │       │                       │
    │       │                       └─► Templates.hs
    │       │
    │       └─► Events.hs (shared, breaks import cycles)
    │
    └── GUI/
        ├── App.hs ────────────► Main layout, chat interface
        ├── Runner.hs ─────────► Effect stack wiring, QuestionHandler
        ├── Theme.hs ──────────► Calm aesthetic colors
        └── Widgets/
            ├── Input.hs ──────► Photo upload, text input
            └── Question.hs ───► Question rendering (disposition cards, etc.)
```

---

## Type System: Newtypes (Types.hs)

**Critical for type safety** - prevents mixing up different Text values:

```haskell
-- Items being sorted
newtype ItemName = ItemName { unItemName :: Text }

-- Where items go
newtype Location = Location { unLocation :: Text }

-- What the space is for
newtype SpaceFunction = SpaceFunction { unSpaceFunction :: Text }

-- Topics that cause anxiety
newtype AnxietyTrigger = AnxietyTrigger { unAnxietyTrigger :: Text }

-- Emergent categories from splitting
newtype CategoryName = CategoryName { unCategoryName :: Text }

-- Photo chaos level (parsed at JSON boundary)
data ChaosLevel = Buried | Cluttered | Moderate | Clear
  deriving (Eq, Show)
```

**Usage**: Use smart constructors (`mkItemName`, etc.) which strip whitespace.

---

## State Machine (State.hs)

### Phases

```haskell
data Phase
  = Surveying        -- Gathering photos, function, anchors
  | Sorting          -- Main loop: belongs/out/unsure
  | Splitting        -- Breaking unsure pile into categories
  | Refining         -- Working through sub-piles
  | DecisionSupport  -- Helping with stuck items
```

### ActiveState (Common Fields)

Every phase except Surveying shares common fields. These are factored out into `ActiveState`:

```haskell
data ActiveState = ActiveState
  { asFunction    :: SpaceFunction     -- What this space is FOR
  , asAnchors     :: [ItemName]        -- Things that definitely stay
  , asLastAnxiety :: Maybe AnxietyTrigger  -- Thing user was anxious about
  }
```

### PhaseData Sum Type

**Each phase has different valid fields** - this prevents invalid states:

```haskell
data PhaseData
  = SurveyingData
      { sdGatheredFunction :: Maybe SpaceFunction
      , sdGatheredAnchors  :: [ItemName]
      }
  | SortingData
      { soActive      :: ActiveState       -- Common state (function, anchors, lastAnxiety)
      , soCurrentItem :: Maybe ItemName
      }
  | SplittingData
      { spActive     :: ActiveState
      , spCategories :: NonEmpty CategoryName  -- REQUIRED, non-empty
      }
  | RefiningData
      { rfActive          :: ActiveState
      , rfEmergentCats    :: Map CategoryName [ItemName]
      , rfCurrentCategory :: CategoryName     -- REQUIRED
      , rfCurrentItem     :: Maybe ItemName
      }
  | DecisionSupportData
      { dsActive      :: ActiveState
      , dsStuckItem   :: ItemName             -- REQUIRED
      , dsReturnPhase :: Phase
      }
```

**Key invariants enforced by types**:
- Can't be Sorting without a function (in ActiveState)
- Can't be Refining without a current category
- Can't be in DecisionSupport without a stuck item
- Can't split into zero categories (NonEmpty)

### SessionState

```haskell
data SessionState = SessionState
  { phaseData      :: PhaseData    -- Phase-specific data
  , piles          :: Piles        -- belongs/out/unsure item lists
  , itemsProcessed :: Int          -- Progress count
  , sessionStart   :: Maybe UTCTime -- Set on first turn (via Time effect)
  }

-- Phase is DERIVED, not stored (prevents inconsistency)
phase :: SessionState -> Phase
phase st = case st.phaseData of
  SurveyingData {}       -> Surveying
  SortingData {}         -> Sorting
  SplittingData {}       -> Splitting
  RefiningData {}        -> Refining
  DecisionSupportData {} -> DecisionSupport
```

### Piles

```haskell
data Piles = Piles
  { belongs :: [ItemName]  -- Items that stay
  , out     :: [ItemName]  -- Items to remove
  , unsure  :: [ItemName]  -- Not yet classified
  }
```

### Accessor Functions

Since PhaseData is a sum type, use accessors to get common fields:

```haskell
-- Get the ActiveState (Nothing only during Surveying)
getActiveState :: SessionState -> Maybe ActiveState

-- Individual accessors (delegate to ActiveState when available)
getFunction :: SessionState -> Maybe SpaceFunction
getAnchors :: SessionState -> [ItemName]
getCurrentItem :: SessionState -> Maybe ItemName
getLastAnxiety :: SessionState -> Maybe AnxietyTrigger
getCurrentCategory :: SessionState -> Maybe CategoryName
getEmergentCats :: SessionState -> Map CategoryName [ItemName]
```

---

## OODA Loop Implementation (Loop.hs)

### tidyingTurn - Main Entry Point

```haskell
tidyingTurn
  :: ( State SessionState :> es
     , LLM :> es
     , Emit TidyingEvent :> es
     , Log :> es
     , Time :> es
     )
  => UserInput -> Eff es Response

tidyingTurn input = do
  st <- get @SessionState

  -- OBSERVE: Analyze photos via vision
  mPhotoAnalysis <- analyzePhotos input.inputPhotos

  -- ORIENT: Extract structured info (LLM call)
  extract <- extractFromInput mPhotoAnalysis input

  -- DECIDE: Pure routing based on state + extract + photo analysis
  let (action, nextPhase) = decideFromExtract st mPhotoAnalysis extract

  -- Update state BEFORE response (so counts are accurate)
  applyStateTransitionFromExtract extract action nextPhase

  -- ACT: Generate response (may use tools, may call LLM)
  st' <- get @SessionState
  let ctx = buildTidyingContext st' mPhotoAnalysis input.inputText
  response <- actResponse ctx action

  pure Response
    { responseText = response
    , responsePhase = nextPhase
    , responseItemsProcessed = st'.itemsProcessed
    , responseSessionEnded = extract.exIntent == IntentStop
    }
```

### OBSERVE: Photo Analysis

```haskell
analyzePhotos :: [Photo] -> Eff es (Maybe PhotoAnalysis)
```

Calls LLM with vision to understand the space:
- `paRoomType`: "office", "bedroom", etc.
- `paChaosLevel`: `Buried | Cluttered | Moderate | Clear`
- `paVisibleItems`: What's visible
- `paBlockedFunction`: "can't sit", "can't reach desk"
- `paFirstTarget`: Best first thing to address

### ORIENT: Extraction

```haskell
extractFromInput :: Maybe PhotoAnalysis -> UserInput -> Eff es Extract
```

LLM extracts structured facts from user input:

```haskell
data Extract = Extract
  { exIntent   :: Intent           -- start/continue/item/decided/help/stop
  , exItem     :: Maybe ItemName   -- Item mentioned
  , exChoice   :: Maybe Choice     -- trash/keep/place/unsure
  , exPlace    :: Maybe Location   -- Where to put it
  , exFunction :: Maybe SpaceFunction  -- What space is for
  , exAnchors  :: Maybe [ItemName]     -- Anchor items
  }

data Intent = IntentStart | IntentContinue | IntentItem
            | IntentDecided | IntentHelp | IntentStop

data Choice = ChoiceTrash | ChoiceKeep | ChoicePlace | ChoiceUnsure
```

### DECIDE: Pure Routing (Decide.hs)

**No LLM calls** - pure function from state to action:

```haskell
decideFromExtract
  :: SessionState
  -> Maybe PhotoAnalysis  -- Photo-aware routing
  -> Extract
  -> (Action, Phase)
```

**Photo-aware routing patterns**:
- `ChaosLevel == Buried` → skip questions, fast-track to `FirstInstruction`
- `hasBlockedFunction` + have function → start sorting immediately
- `ChaosLevel == Clear` → acknowledge progress

**Phase-aware routing patterns**:
- `Surveying` + no function → `AskFunction`
- `Sorting` + `IntentDecided` + `ChoiceTrash` → `InstructTrash`
- `Sorting` + unsure pile > 5 → `InstructSplit`
- Any phase + `IntentStop` → `Summary`

### State Transitions (Loop.hs)

```haskell
applyStateTransitionFromExtract :: Extract -> Action -> Phase -> Eff es ()
```

Calls `transitionPhaseData` which handles:
- Phase transitions (Surveying → Sorting, etc.)
- Item distribution during splits (unsure items → first category)
- Setting `sessionStart` on first turn

```haskell
transitionPhaseData
  :: PhaseData -> Extract -> Action -> Phase -> [ItemName] -> PhaseData
```

**Critical**: Items from unsure pile are passed through so they can be distributed to emergent categories when entering Refining phase.

### ACT: Response Generation

```haskell
actResponse :: TidyingContext -> Action -> Eff es Text
```

Uses canned responses when possible, LLM for complex ones:

```haskell
cannedResponse :: Action -> Maybe Text
cannedResponse InstructTrash = Just "Trashed! Next?"
cannedResponse InstructUnsure = Just "Unsure pile. Next?"
cannedResponse InstructNext = Just "What's next?"
cannedResponse _ = Nothing  -- Need LLM
```

---

## Actions (Action.hs)

```haskell
data Action
  -- Questions (surveying)
  = AskFunction              -- "What do you need to DO in this space?"
  | AskAnchors               -- "What's definitely staying?"
  | AskWhatIsIt              -- "What is it?"
  | AskItemDecision ItemName -- "Trash, keep, or not sure?"

  -- Instructions (sorting)
  | FirstInstruction         -- Initial momentum-building action
  | InstructTrash            -- "Trash. Next."
  | InstructPlace Location   -- "Put it on [shelf]. Next."
  | InstructUnsure           -- "Unsure pile, floor right. Next."
  | InstructNext             -- "Next thing."

  -- Splitting (NonEmpty guarantees at least one category)
  | InstructSplit (NonEmpty CategoryName)

  -- Decision support
  | DecisionAid ItemName     -- Reframe using function
  | EnergyCheck              -- "Keep going or stop?"

  -- Pivoting (anxiety avoidance)
  | PivotAway AnxietyTrigger Location

  -- Completion
  | AckProgress Text         -- Acknowledge + context
  | Summary                  -- Session summary
```

---

## Tools (Tools.hs)

Tools run during LLM turns, enabling **mid-turn user interaction**.

### ProposeDisposition (Main Tool)

LLM proposes where an item should go, user confirms with one tap:

```haskell
data ProposeDispositionInput = ProposeDispositionInput
  { pdiItem    :: Text
  , pdiChoices :: [ProposedChoice]
  , pdiReason  :: Maybe Text
  }

data ProposeDispositionResult = ProposeDispositionResult
  { pdrDisposition :: ItemDisposition  -- What user chose
  , pdrWasCorrection :: Bool           -- Did they override suggestion?
  }
```

### Tool Execution Pattern

All tools wrap question handler in **try/catch** for robustness:

```haskell
executeProposeDisposition
  :: (State SessionState :> es, Emit TidyingEvent :> es, Log :> es, IOE :> es)
  => QuestionHandler -> ProposeDispositionInput -> Eff es ProposeDispositionResult

executeProposeDisposition askQuestion input = do
  emit $ ItemProposed input.pdiItem (map (.pcLabel) input.pdiChoices)

  -- Ask user via IO callback (wrapped in try/catch)
  result <- liftIO $ try @SomeException $ askQuestion question

  case result of
    Left err -> do
      logWarn $ "Question handler failed: " <> T.pack (show err)
      pure ProposeDispositionResult { pdrDisposition = SkipForNow, pdrWasCorrection = False }
    Right answer -> processAnswer answer
```

### QuestionHandler

IO callback because tools run inside LLM interpreter where `QuestionUI` effect has been consumed:

```haskell
type QuestionHandler = Question -> IO Answer
```

---

## Question DSL (Question.hs)

Rich question types for user interaction:

```haskell
data Question
  = ProposeDisposition Text [Choice] (Maybe Text)  -- Item disposition choices
  | Choose Text QuestionId [ChoiceOption] (Map Text Question)  -- Generic choice
  | FreeText Text (Maybe Text)                     -- Text input with placeholder
  | Confirm Text Bool                              -- Yes/No with default
  | QuestionGroup Text [Question]                  -- Multiple questions
  | ConditionalQ Condition Question                -- Conditional display

data Answer
  = DispositionAnswer ItemDisposition
  | ChoiceAnswer Text
  | TextAnswer Text
  | ConfirmAnswer Bool
  | GroupAnswer (Map QuestionId Answer)
```

---

## Context (Context.hs)

What gets rendered into the LLM prompt:

```haskell
data TidyingContext = TidyingContext
  { tcPhase              :: Phase
  , tcFunction           :: Maybe Text
  , tcAnchors            :: [Text]
  , tcPiles              :: PilesSummary
  , tcEmergentCategories :: [Text]
  , tcCurrentCategory    :: Maybe Text
  , tcItemsProcessed     :: Int
  , tcLastAnxiety        :: Maybe Text
  , tcPhotoAnalysis      :: Maybe PhotoAnalysis
  , tcUserText           :: Maybe Text
  }

data PhotoAnalysis = PhotoAnalysis
  { paRoomType        :: Text
  , paChaosLevel      :: ChaosLevel  -- Parsed enum, not raw text
  , paVisibleItems    :: [Text]
  , paBlockedFunction :: Maybe Text
  , paFirstTarget     :: Maybe Text
  }
```

---

## Events (Events.hs)

Emitted for GUI updates and debugging:

```haskell
data TidyingEvent
  = UserInputReceived Text
  | ResponseGenerated Text
  | PhotoAnalyzed Text
  | SituationClassified Text
  | ActionTaken Action
  | PhaseChanged Phase Phase
  | SessionEnded Int
  -- Tool events
  | ItemProposed Text [Text]
  | UserConfirmed Text Text
  | UserCorrected Text Text
  | FunctionChosen Text
  | SessionConfirmedDone
```

---

## GUI Architecture

### Runner.hs - Effect Stack Wiring

```haskell
tidyingGameLoopWithGUI :: GUIBridge SessionState -> IO ()
tidyingGameLoopWithGUI bridge = do
  let questionHandler = makeQuestionHandler bridge
      toolDispatcher = makeTidyingDispatcher questionHandler

  runEff
    . runTime           -- Time effect (IO-blind)
    . runRandom
    . runEmit handleEvent
    . runStateWithGUISync bridge initialState
    . runChatHistory
    . runLogWithBridge bridge Debug
    . runRequestInput inputHandler
    . runLLMWithToolsHooked spinnerHooks llmConfig toolDispatcher
    . runQuestionUI questionHandler
    . inject
    $ tidyingRun
```

### QuestionHandler with Timeout

Routes questions from tools to GUI with **5-minute timeout**:

```haskell
makeQuestionHandler :: GUIBridge state -> QuestionHandler
makeQuestionHandler bridge = \question -> do
  atomically $ writeTVar bridge.gbPendingRequest
    (Just $ PendingCustom "question" (toJSON question))

  -- Block with timeout
  mResponse <- timeout 300000000 $ takeMVar bridge.gbRequestResponse
  atomically $ writeTVar bridge.gbPendingRequest Nothing

  case mResponse of
    Nothing -> do
      logError bridge "Question handler timed out"
      pure $ fallbackAnswer question
    Just response -> parseResponse response
```

### State Synchronization

State syncs to GUI TVar after every `Put`:

```haskell
runStateWithGUISync bridge initial = reinterpret (runState initial) $ \_ -> \case
  Get -> EState.get
  Put s -> do
    EState.put s
    liftIO $ updateState bridge (const s)  -- Sync to TVar
```

---

## Effect Stack

### Agent sees (IO-blind):
```haskell
'[QuestionUI, LLM, State SessionState, Emit TidyingEvent,
  RequestInput, Log, ChatHistory, Random, Time]
```

### After inject (adds IOE):
```haskell
'[QuestionUI, ..., Time, IOE]
```

### Interpretation order (outermost first):
```
runTime        -- Time → IOE (getCurrentTime)
runRandom      -- Random → IOE
runEmit        -- Emit → IO callback
runState       -- State → local state + GUI sync
runChatHistory -- ChatHistory → in-memory
runLog         -- Log → GUI bridge
runRequestInput -- RequestInput → GUI handler
runLLMWithToolsHooked -- LLM → Anthropic API + tool dispatch
runQuestionUI  -- QuestionUI → question handler
```

---

## Data Flow: Complete Turn Trace

**User sends "trash it":**

1. **GUI** receives input, posts to `gbPendingRequest`
2. **tidyingRun** calls `tidyingTurn` with `UserInput { inputText = Just "trash it", inputPhotos = [] }`
3. **OBSERVE**: `analyzePhotos []` → `Nothing` (no photos)
4. **ORIENT**: `extractFromInput Nothing input` → LLM extracts:
   ```haskell
   Extract { exIntent = IntentDecided, exChoice = Just ChoiceTrash, exItem = Nothing, ... }
   ```
5. **DECIDE**: `decideFromExtract st Nothing extract` → `(InstructTrash, Sorting)`
6. **State Update**: `applyStateTransitionFromExtract` → adds item to `piles.out`
7. **ACT**: `actResponse ctx InstructTrash` → `cannedResponse` returns `"Trashed! Next?"`
8. **Response** returned to GUI: `Response { responseText = "Trashed! Next?", ... }`

---

## Common Patterns

### Adding a New Tool

1. **Define types** in `Tools.hs`:
   ```haskell
   data MyToolInput = MyToolInput { ... }
   data MyToolResult = MyToolResult { ... }
   ```

2. **Create schema**:
   ```haskell
   myToolSchema :: Value
   myToolSchema = object [...]
   ```

3. **Implement execution** with try/catch:
   ```haskell
   executeMyTool :: (...) => QuestionHandler -> MyToolInput -> Eff es MyToolResult
   executeMyTool askQuestion input = do
     result <- liftIO $ try @SomeException $ askQuestion question
     case result of
       Left err -> logWarn "..." >> return fallback
       Right answer -> processAnswer answer
   ```

4. **Register** in `tidyingTools` list
5. **Add case** in `makeTidyingDispatcher`

### Adding a New Phase

1. Add constructor to `Phase` in `State.hs`
2. Add `PhaseData` variant with required fields
3. Add accessor cases in `getFunction`, `getAnchors`, etc.
4. Add transition cases in `transitionPhaseData` (Loop.hs)
5. Add routing cases in `decideFromExtract` (Decide.hs)

### Adding a New Action

1. Add constructor to `Action` in `Action.hs`
2. Add routing case in `Decide.hs`
3. Add response in `Act.hs`:
   - `cannedResponse` if deterministic
   - LLM-generated if context-dependent
4. Update `itemDelta` in `Loop.hs` if action processes items

---

## Error Handling

### LLM Failures

```haskell
case result of
  TurnCompleted (TurnParsed tr) -> use tr.trOutput
  TurnCompleted (TurnParseFailed {tpfError}) ->
    logWarn "Parse failed" >> use fallback
  TurnBroken reason ->
    logWarn reason >> use fallback
```

### Tool Failures

All question handlers wrapped in try/catch → return fallback on error.

### Timeout

QuestionHandler has 5-minute timeout → returns `fallbackAnswer` on timeout.

---

## Known Issues (P2/P3)

1. **QuestionGroup only renders first question** (P2)
2. **Choose reveals not implemented** (P2)
3. **ConditionalQ conditions not evaluated** (P2)
4. **No state persistence across sessions** (P2)
5. **lastAnxiety captured but not used in routing** (P2)

---

## Design Philosophy

- **Momentum over perfection**: Keep user moving, don't block on decisions
- **LLM extracts, Haskell decides**: Structured extraction → pure routing
- **Tools for interaction**: Mid-turn user confirmation via tool calls
- **IO-blind agents**: Time effect instead of IOE for testability
- **Graceful degradation**: Try/catch on all IO, fallback answers on timeout
- **Type safety**: Newtypes prevent mixing up ItemName/Location/etc.
- **Invalid states unrepresentable**: PhaseData sum type, NonEmpty constraints
