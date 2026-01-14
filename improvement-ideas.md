# Tidepool DSL Improvement Ideas

Context: These emerged from teaching the library to a new user building a Habitica task router. Pain points around boilerplate, type safety, and ergonomics.

---

## Type-Level DSL Improvements

### 1. Effect Inference from Handlers

**Current Problem:**
```haskell
-- Graph: manually declare effects
, router :: mode :- G.LLMNode
    :@ Input TaskRouterInput
    :@ Schema TaskRouterExit
    :@ UsesEffects '[Goto "router" TaskRouterInput, Goto Exit Result]
    --             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    --             Redundant with handler implementation

-- Handler: actually uses effects
routerAfter input exit = case exit of
  MoreData -> gotoSelf input        -- Self loop
  Done -> gotoExit result            -- Exit
```

If handler changes, graph annotation must change manually. Duplication leads to drift.

**Proposed Solution: Infer from Handler Type**
```haskell
-- Graph: NO UsesEffects annotation needed
, router :: mode :- G.LLMNode
    :@ Input TaskRouterInput
    :@ Schema TaskRouterExit
    -- Effects automatically derived from handler type

-- Handler signature is single source of truth
routerAfter
  :: TaskRouterInput
  -> TaskRouterExit
  -> Eff '[Goto "router" TaskRouterInput, Goto Exit Result] (GotoChoice ...)
```

**Implementation Sketch:**
```haskell
-- Type family to extract effects from handler type
type family HandlerEffects (handlerType :: Type) :: [Effect] where
  HandlerEffects (input -> Eff effs output) = effs
  HandlerEffects _ = '[]

-- Validation: handler effects must match node effects
type family ValidateHandlerEffects graph handlers :: Constraint where
  ValidateHandlerEffects graph handlers =
    ( HandlerEffects (handlers.router) ~ NodeEffects (graph.router)
    , ... -- for each node
    )

-- NodeEffects extracts from UsesEffects or infers
type family NodeEffects node :: [Effect] where
  NodeEffects (node :@ UsesEffects effs) = effs
  NodeEffects node = InferEffects node  -- default case
```

**Trade-offs:**
- ✅ Single source of truth (handler signature)
- ✅ Less graph boilerplate
- ✅ Refactoring safer (change handler, graph updates)
- ❌ Handler signatures become load-bearing (can't inline lambdas easily)
- ❌ Type error messages on mismatch might be cryptic

**Alternative Approach:**
Infer handler types FROM graph (reverse direction). Graph is source of truth, handler types generated.

---

### 2. Single Deriving Clause (`deriving Tidepool`)

**Current Problem:**
```haskell
data TaskRouterExit = ...
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, HasJSONSchema)
  -- Easy to forget HasJSONSchema → runtime failure
  -- Must remember which strategy for which class
  -- Boilerplate on every exit type
```

**Proposed Solution:**
```haskell
data TaskRouterExit = ...
  deriving Tidepool  -- One line, all standard instances

-- Expands to all needed instances:
--   deriving stock (Show, Eq, Generic)
--   deriving anyclass (FromJSON, ToJSON, HasJSONSchema, ToDecisionTools)
```

**Implementation Options:**

**Option A: GHC Plugin**
```haskell
plugin :: Plugin
plugin = defaultPlugin
  { derivingStrategyPlugin = installDerivingStrategy
  }

installDerivingStrategy :: [DerivStrategy GhcPs] -> TcM [DerivStrategy GhcTc]
installDerivingStrategy strats = mapM expandTidepoolDeriving strats
  where
    expandTidepoolDeriving (DerivStrategy "Tidepool") =
      pure [ StockStrategy [''Show, ''Eq, ''Generic]
           , AnyclassStrategy [''FromJSON, ''ToJSON, ''HasJSONSchema, ''ToDecisionTools]
           ]
```

**Option B: Template Haskell**
```haskell
data TaskRouterExit = ...
$(deriveTidepool ''TaskRouterExit)

-- Generates all instances via TH splice
```

**Option C: DerivingVia (no plugin needed)**
```haskell
data TaskRouterExit = ...
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON, HasJSONSchema)
    via TidepoolDefault TaskRouterExit

newtype TidepoolDefault a = TidepoolDefault a
  deriving newtype (FromJSON, ToJSON, HasJSONSchema, ToDecisionTools)
```

**Trade-offs:**
- ✅ Less boilerplate (1 line vs 2-3)
- ✅ Can't forget required instances
- ✅ Can add new standard instances without code changes
- ❌ Hides what instances exist (discoverability issue)
- ❌ Plugin/TH adds build complexity
- ❌ Debugging derived instances harder

**Recommendation:** Start with DerivingVia (Option C) - no plugin needed, clear opt-in.

---

### 3. Type-Safe Goto Targets

**Current Problem:**
```haskell
Goto "router" Input  -- String target, typo-prone

-- If you rename 'router' field → runtime error or confusing compile error
```

**Proposed Solution:**
```haskell
Goto router Input  -- 'router' is the field name, validated at type level

-- Type error if field doesn't exist
-- Automatic rename when field renamed
```

**Implementation:**
Requires GHC.OverloadedRecordDot-style magic or custom type checker plugin. Complex but high value.

---

### 4. Template Context Inference

**Current Problem:**
```haskell
-- Must manually define context matching template needs
data RouterContext = RouterContext
  { userMessage :: Text
  , habiticaTasks :: [Text]
  }

-- If template changes, must update context type manually
```

**Proposed Solution:**
```haskell
-- Parse template at compile time, generate context type
$(deriveContextFromTemplate "templates/router.jinja")

-- Generates:
-- data RouterContext = RouterContext
--   { userMessage :: Text      -- from {{ userMessage }}
--   , habiticaTasks :: [Text]  -- from {% for task in habiticaTasks %}
--   }
```

**Implementation:**
- TH splice that parses Jinja template
- Extracts all variable references
- Generates record type with those fields
- Could infer types from usage (strings in text, lists in loops)

**Trade-offs:**
- ✅ Template and context never drift
- ✅ Refactoring template updates type automatically
- ❌ Can't add extra context fields not used in template
- ❌ Type inference might guess wrong (everything Text?)

---

### 5. Schema Constraint Checking

**Current Problem:**
```haskell
data BadExit = A { x :: Int } | B { y :: Text }
  deriving StructuredOutput
  -- Compiles fine, FAILS AT RUNTIME: oneOf not supported

-- Error only appears when LLM tries to use the schema
```

**Proposed Solution:**
```haskell
data BadExit = A { x :: Int } | B { y :: Text }
  deriving StructuredOutput
  --       ^^^^^^^^^^^^^^^^^
  -- ERROR at compile time: Sum types with data not allowed
  --        Use ToDecisionTools (tool calling) instead
```

**Implementation:**
```haskell
-- Type family validation
type family ValidStructuredOutput a :: Constraint where
  ValidStructuredOutput (SumTypeWithData a) =
    TypeError ( 'Text "Sum types with data cannot use StructuredOutput"
              ':$$: 'Text "Anthropic's structured output doesn't support oneOf schemas"
              ':$$: 'Text "Use ToDecisionTools instead (tool calling pattern)"
              )
  ValidStructuredOutput a = ()

-- Instance with constraint
instance (Generic a, ValidStructuredOutput a, ...)
  => StructuredOutput a where ...
```

**Trade-offs:**
- ✅ Catch errors at compile time
- ✅ Clear error messages with fix suggestions
- ✅ No runtime surprises
- ❌ Type-level programming complexity
- ❌ May reject valid cases (false positives)

---

## Value-Level DSL Improvements

### 6. Memory as StateT

**Current Problem:**
```haskell
routerAfter input exit = do
  mem <- getMem @RouterMem
  updateMem @RouterMem $ \m -> m { attemptCount = m.attemptCount + 1 }
  -- Need to re-get if using mem later:
  mem2 <- getMem @RouterMem

  -- Imperative, verbose, type applications everywhere
```

**Proposed Solution:**
```haskell
routerAfter input exit = do
  -- StateT-style API
  modify @RouterMem $ \m -> m { attemptCount = m.attemptCount + 1 }

  -- Or with operators:
  attemptCount @RouterMem += 1

  -- Current value available via get
  mem <- get @RouterMem
```

**Implementation:**

**Option A: Interpret Memory as StateT**
```haskell
interpretMemoryAsState
  :: s                                    -- Initial state
  -> Eff (Memory s ': effs) a            -- With Memory effect
  -> Eff effs (a, s)                     -- Returns final state
interpretMemoryAsState initial =
  runState initial . reinterpret \case
    GetMem -> get
    UpdateMem f -> modify f
```

**Option B: MTL-style classes**
```haskell
class Monad m => MonadMemory s m | m -> s where
  getMem :: m s
  putMem :: s -> m ()
  modifyMem :: (s -> s) -> m ()

instance Member (Memory s) effs => MonadMemory s (Eff effs) where
  getMem = send GetMem
  putMem = send . UpdateMem . const
  modifyMem = send . UpdateMem

-- Handlers use MonadMemory constraint
routerAfter
  :: (MonadMemory RouterMem m, MonadGoto m)
  => Input -> Exit -> m (GotoChoice ...)
```

**Option C: Optics-first**
```haskell
-- Memory with lenses
data RouterMem = RouterMem
  { _attemptCount :: Int
  , _history :: [Text]
  }
makeLenses ''RouterMem

-- Lens operators work directly
routerAfter input exit = do
  attemptCount @RouterMem %= (+1)
  history @RouterMem %= (newAttempt :)
  count <- use (attemptCount @RouterMem)
```

**Trade-offs:**
- ✅ Familiar StateT/MTL API
- ✅ Can use standard combinators (modify, gets, zoom)
- ✅ Less type application noise
- ✅ Optics enable powerful compositions
- ❌ StateT isn't actually StateT under the hood (just looks like it)
- ❌ Lens integration needs additional deps/setup
- ❌ Type application still needed (which memory?)

**Recommendation:** Start with MTL classes (Option B), add lens support later.

---

### 7. Pattern Matching Sugar

**Current Problem:**
```haskell
routerAfter input exit = case exit of
  MoreData q -> gotoSelf input
  Done r -> gotoExit r
  -- Verbose, especially with many constructors
```

**Proposed Solutions:**

**Option A: `on` combinator**
```haskell
routerAfter input exit =
  on @MoreData (\q -> gotoSelf input) exit
  <|> on @Done (\r -> gotoExit r) exit
  <|> exhaustive  -- Error if no match

-- Type-safe: can't forget a constructor
```

**Option B: Pattern DSL**
```haskell
routerAfter input = matchExit
  [ MoreData ~~> \q -> gotoSelf input
  , Done ~~> \r -> gotoExit r
  ]
```

**Implementation:**
```haskell
on :: forall ctor m. MatchConstructor ctor => (CtorFields ctor -> m) -> Exit -> m -> m
on handler exit fallback = case matchCtor @ctor exit of
  Just fields -> handler fields
  Nothing -> fallback

-- Type family to extract constructor field types
type family CtorFields (ctor :: Symbol) :: Type
```

**Trade-offs:**
- ✅ More concise than case
- ✅ Type-safe constructor matching
- ✅ Exhaustiveness checking possible
- ❌ Less familiar syntax
- ❌ GHC.Generics magic can be complex

---

### 8. Auto-Context Building

**Current Problem:**
```haskell
-- Manual context construction
buildContext input = do
  tasks <- loadHabiticaTasks      -- Effect
  habits <- loadHabiticaHabits    -- Effect
  dailies <- loadHabiticaDailies  -- Effect
  pure RouterContext
    { userMessage = input.triUserMessage
    , habiticaTasks = tasks
    , habiticaHabits = habits
    , habiticaDailies = dailies
    }
```

**Proposed Solution:**
```haskell
-- Context fields auto-populated from input and effect results
buildContext input = autoContext @RouterContext $ do
  habiticaTasks <- loadHabiticaTasks
  habiticaHabits <- loadHabiticaHabits
  habiticaDailies <- loadHabiticaDailies
  -- userMessage pulled from input automatically
  -- Context built from field names matching variables

-- Or even more magical:
buildContext = autoContextFromInput @RouterContext loadHabiticaData
```

**Implementation:**
```haskell
-- Template Haskell to generate context builder
autoContext :: forall ctx input. (Generic ctx, Generic input) => input -> Eff effs () -> Eff effs ctx
autoContext input effectsBlock = do
  -- Run effects, capture bindings
  bindings <- captureBindings effectsBlock
  -- Match context field names to input fields + bindings
  -- Generate context constructor call
  pure $ constructFromBindings @ctx input bindings
```

**Trade-offs:**
- ✅ Eliminates boilerplate context construction
- ✅ Field name matching is explicit (rename-safe)
- ❌ Magic behavior (where do values come from?)
- ❌ Hard to debug when it guesses wrong

---

### 9. Validation Combinators

**Current Problem:**
```haskell
routerAfter input exit = do
  -- Manual validation scattered through code
  when (length (htTasks exit) == 0) $
    error "Must have at least one task"
  when (any T.null (htTasks exit)) $
    error "Tasks must be non-empty"
  -- Continue with validated data...
```

**Proposed Solution:**
```haskell
routerAfter input exit =
  validate exit
    & ensure (length . htTasks > 0) "Must have at least one task"
    & ensure (all (not . T.null) . htTasks) "Tasks must be non-empty"
    & ensureM loadTasksValid "Tasks don't exist in Habitica"
    >>= \validated ->
      -- validated has type Validated TaskRouterExit
      -- Can only proceed if validation passed
      processValidated validated
```

**Implementation:**
```haskell
newtype Validated a = Validated a  -- Phantom type for validated data

validate :: a -> Validator a
ensure :: (a -> Bool) -> Text -> Validator a -> Validator a
ensureM :: (a -> Eff es Bool) -> Text -> Validator a -> Validator (Eff es) a

-- Validator monad with validation errors
data Validator a = Validator
  { runValidator :: Either [ValidationError] (Validated a)
  }
```

**Trade-offs:**
- ✅ Declarative validation
- ✅ Accumulate all errors (not fail-fast)
- ✅ Type-safe: can't use unvalidated data
- ❌ Another abstraction to learn
- ❌ May be overkill for simple cases

---

## Impact Analysis

**High Impact (implement first):**
1. Memory as StateT (#6) - Immediate ergonomics win, minimal risk
2. deriving Tidepool (#2) - Prevents common mistakes, easy with DerivingVia
3. Schema constraint checking (#5) - Catches runtime errors at compile time

**Medium Impact (implement if time permits):**
4. Effect inference (#1) - Big ergonomics win, but complex implementation
5. Pattern matching sugar (#7) - Nice to have, but case works fine
6. Validation combinators (#9) - Useful but can be library on top

**Lower Priority (explore later):**
7. Type-safe Goto (#3) - Requires significant type system work
8. Template context inference (#4) - Fragile, TH complexity
9. Auto-context building (#8) - Too much magic, unclear benefit

---

## Implementation Priorities

**Phase 1: Low-hanging fruit**
- Memory as StateT (MonadMemory class)
- deriving via TidepoolDefault
- Schema validation in StructuredOutput instance

**Phase 2: Ergonomics**
- Pattern matching `on` combinator
- Validation DSL as separate module

**Phase 3: Advanced type-level**
- Effect inference (requires design work)
- Type-safe Goto (requires plugin or compiler changes)

---

## Notes from Teaching Session

Key pain points observed:
1. **Confusion around Input vs Context** - Users expect input to flow directly to template
2. **Session management surprise** - ClaudeCode adds complexity, wasn't needed for this use case
3. **Exit type design is critical** - Spent most time here, need better guidance
4. **Documentation in types unclear** - User didn't realize comments → tool descriptions
5. **Handler boilerplate** - Before/after pattern is clear but verbose

Teaching insights:
- Exit type design should come FIRST (it's the routing logic)
- Template comes SECOND (asks LLM to make that decision)
- Handlers come LAST (execute the decision)
- Most bugs from mismatch between these three layers
