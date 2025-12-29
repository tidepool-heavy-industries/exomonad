# Tidying Agent - Architecture & Implementation Guide

A prosthetic executive function for tackling overwhelming spaces. Uses OODA (Observe-Orient-Decide-Act) pattern with tool-based LLM interaction.

## Core Concept

The tidying agent helps users who feel overwhelmed by messy spaces. It:
1. Analyzes photos of the space (via Claude's vision)
2. Proposes where items should go (via tool calls)
3. Gets user confirmation with one tap
4. Provides momentum and structure without judgment

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
│     │          │                                                         │
│     │          └─► extractFromInput (LLM extracts facts)                │
│     │                                                                    │
│     └─► analyzePhotos (LLM vision)                                      │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

## Module Dependency Graph

```
Tidying (re-exports)
    │
    ├── Agent.hs ──────────► Loop.hs ──────────► Decide.hs
    │       │                   │                    │
    │       │                   ├─► Tools.hs         └─► Action.hs
    │       │                   │       │
    │       │                   │       └─► Question.hs
    │       │                   │
    │       │                   ├─► Context.hs
    │       │                   │
    │       │                   ├─► Output.hs
    │       │                   │
    │       │                   └─► Templates.hs
    │       │
    │       └─► State.hs
    │
    └── Events.hs (shared, breaks import cycle)
```

## Key Files

### State.hs - Session State

```haskell
data Phase
  = Surveying        -- Gathering photos, function, anchors
  | Sorting          -- Main loop: belongs/out/unsure
  | Splitting        -- Breaking unsure pile into categories
  | Refining         -- Working through sub-piles
  | DecisionSupport  -- Helping with stuck items

data SessionState = SessionState
  { phase           :: Phase
  , function        :: Maybe Text      -- "workspace", "bedroom", etc.
  , anchors         :: [Text]          -- Things that definitely stay
  , piles           :: Piles           -- belongs/out/unsure lists
  , emergentCats    :: Map Text [Text] -- "cables" -> [items]
  , currentItem     :: Maybe Text      -- For context ("trash it")
  , itemsProcessed  :: Int
  , sessionStart    :: Maybe UTCTime   -- Set on first turn
  }
```

### Decide.hs - Pure Routing

The heart of the state machine. NO LLM calls - pure function from (State, Extract) to (Action, Phase).

```haskell
decideFromExtract :: SessionState -> Extract -> (Action, Phase)
```

Key routing patterns:
- `IntentStop` → `Summary` action
- `IntentDecided` with `ChoiceTrash` → `InstructTrash`
- Unsure pile > 5 items → `InstructSplit`
- Phase-specific routing (Surveying needs function, Sorting handles items, etc.)

### Action.hs - Action ADT

```haskell
data Action
  = AskFunction              -- "What does this space need to DO?"
  | AskAnchors               -- "What definitely stays?"
  | FirstInstruction         -- "Let's start with..."
  | InstructTrash            -- "Trash it!"
  | InstructPlace Text       -- "Put it in the kitchen"
  | InstructUnsure           -- "Set it aside for now"
  | InstructSplit [Text]     -- "Let's split: cables vs other"
  | InstructNext             -- "What's next?"
  | DecisionAid Text         -- Help with stuck item
  | Summary                  -- End of session recap
  | ...
```

### Tools.hs - LLM Tools

Tools run during LLM turns, enabling mid-turn user interaction.

```haskell
-- Main tool: LLM proposes, user confirms
data ProposeDisposition = ProposeDisposition

executeProposeDisposition :: QuestionHandler -> ProposeDispositionInput -> Eff es ProposeDispositionResult
```

Tools use `QuestionHandler` (IO callback) because they run inside the LLM interpreter where `QuestionUI` has already been consumed.

**Error handling**: All tool executions wrap question handler in `try/catch`:
```haskell
result <- liftIO $ try @SomeException $ askQuestion question
case result of
  Left err -> logWarn "Question handler failed" >> return fallback
  Right answer -> processAnswer answer
```

### Question.hs - Question DSL

Rich question types for user interaction:

```haskell
data Question
  = ProposeDisposition Text [Choice] (Maybe Text)  -- Item choices
  | Choose Text QuestionId [ChoiceOption] (Map Text Question)  -- Generic choice
  | FreeText Text (Maybe Text)                     -- Text input
  | Confirm Text Bool                              -- Yes/No
  | QuestionGroup Text [Question]                  -- Multiple questions
  | ConditionalQ Condition Question                -- Conditional display

data Answer
  = DispositionAnswer ItemDisposition
  | ChoiceAnswer Text
  | TextAnswer Text
  | ConfirmAnswer Bool
  | GroupAnswer (Map QuestionId Answer)
```

### Loop.hs - OODA Implementation

```haskell
tidyingTurn :: (State SessionState :> es, LLM :> es, ..., Time :> es)
            => UserInput -> Eff es Response

tidyingTurn input = do
  -- OBSERVE: Analyze photos
  mPhotoAnalysis <- analyzePhotos input.inputPhotos

  -- ORIENT: Extract structured info (LLM call)
  extract <- extractFromInput mPhotoAnalysis input

  -- DECIDE: Pure routing (no LLM)
  let (action, nextPhase) = decideFromExtract st extract

  -- Update state BEFORE response (so counts are accurate)
  applyStateTransitionFromExtract extract action nextPhase

  -- ACT: Generate response (may use tools)
  st' <- get @SessionState
  let ctx' = buildTidyingContext st' mPhotoAnalysis input.inputText
  response <- actResponse ctx' action

  pure Response { responseText = response, ... }
```

### Events.hs - Event Types

Separated to break import cycles between Loop.hs and Tools.hs.

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

## GUI Architecture

### Runner.hs - Effect Stack Wiring

```haskell
tidyingGameLoopWithGUI :: GUIBridge SessionState -> IO ()
tidyingGameLoopWithGUI bridge = do
  -- Create question handler for tools
  let questionHandler = makeQuestionHandler bridge

  -- Create tool dispatcher
  let toolDispatcher = makeTidyingDispatcher questionHandler

  -- Run with full effect stack
  runEff
    . runTime           -- Time effect (IO-blind time access)
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

### QuestionHandler

Routes questions from tools to GUI with 5-minute timeout:

```haskell
makeQuestionHandler :: GUIBridge state -> QuestionHandler
makeQuestionHandler bridge = \question -> do
  -- Post to bridge
  atomically $ writeTVar bridge.gbPendingRequest
    (Just $ PendingCustom "question" (toJSON question))

  -- Wait with timeout (5 minutes)
  mResponse <- timeout 300000000 $ takeMVar bridge.gbRequestResponse

  case mResponse of
    Nothing -> fallbackAnswer question  -- Timeout
    Just response -> parseResponse response
```

### State Synchronization

State syncs to GUI after every `Put`:

```haskell
runStateWithGUISync bridge initial = reinterpret (runState initial) $ \_ -> \case
  Get -> EState.get
  Put s -> do
    EState.put s
    liftIO $ updateState bridge (const s)  -- Sync to TVar
```

## Common Patterns

### Adding a New Tool

1. Define input/output types in `Tools.hs`:
```haskell
data MyToolInput = MyToolInput { ... }
data MyToolResult = MyToolResult { ... }
```

2. Create schema:
```haskell
myToolSchema :: Value
myToolSchema = schemaToValue $ objectSchema [...]
```

3. Implement execution with try/catch:
```haskell
executeMyTool :: (State SessionState :> es, Emit TidyingEvent :> es, Log :> es, IOE :> es)
              => QuestionHandler -> MyToolInput -> Eff es MyToolResult
executeMyTool askQuestion input = do
  result <- liftIO $ try @SomeException $ askQuestion myQuestion
  case result of
    Left err -> logWarn "..." >> return fallback
    Right answer -> processAnswer answer
```

4. Register in `tidyingTools` list
5. Add case in `makeTidyingDispatcher`

### Adding a New Phase

1. Add constructor to `Phase` in `State.hs`
2. Add routing cases in `Decide.hs`:
   - Handle phase-specific situations
   - Consider transitions to/from other phases
3. Update `decideFromExtract` for phase-aware routing
4. Add any new actions if needed

### Adding a New Action

1. Add constructor to `Action` in `Action.hs`
2. Add routing case in `Decide.hs`
3. Add response generation in `Act.hs`:
   - Canned response if deterministic
   - LLM-generated if context-dependent
4. Update `itemDelta` in `Loop.hs` if action processes items

## Effect Stack Order

The effect stack must be interpreted in correct order:

```
Agent code sees:
  '[QuestionUI, LLM, State, Emit, RequestInput, Log, ChatHistory, Random, Time]

After inject (adds IOE):
  '[QuestionUI, ..., Time, IOE]

Interpretation order (outermost first):
  runTime        -- Time → IOE
  runRandom      -- Random → IOE
  runEmit        -- Emit → IO callback
  runState       -- State → local state + GUI sync
  runChatHistory -- ChatHistory → in-memory
  runLog         -- Log → GUI bridge
  runRequestInput -- RequestInput → GUI handler
  runLLMWithToolsHooked -- LLM → Anthropic API + tool dispatch
  runQuestionUI  -- QuestionUI → question handler
```

## Testing

```bash
# Build
cabal build

# Run GUI
ANTHROPIC_API_KEY=... cabal run tidepool-tidy-gui

# Opens browser at localhost:8024
```

## Known Issues / TODO

1. **Items lost during split** (P0): When unsure pile splits, items are cleared but not moved to emergent categories. Fix: distribute items to named categories.

2. **Photo analysis not used in routing** (P1): Photos are analyzed but routing doesn't react to chaos level or blocked function.

3. **Question reveals not implemented** (P2): `choiceReveals` in Question.hs parsed but not rendered.

4. **No state persistence** (P2): Browser refresh loses session.

## Design Philosophy

- **Momentum over perfection**: Keep user moving, don't block on decisions
- **LLM extracts, Haskell decides**: Structured extraction → pure routing
- **Tools for interaction**: Mid-turn user confirmation via tool calls
- **IO-blind agents**: Time effect instead of IOE for testability
- **Graceful degradation**: Try/catch on all IO, fallback answers on timeout
