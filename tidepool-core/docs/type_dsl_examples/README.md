# Type DSL Examples

Sketches of state machines using the Tidepool type-level DSL.

**Core principle: Everything is an effect. Transitions are the `Goto` effect.**

## Examples

| File | Pattern | Key Features |
|------|---------|--------------|
| `01_rag_pipeline.hs` | RAG | Linear flow, Goto effects, implicit vs explicit edges |
| `02_support_router.hs` | Multi-way branch | Multiple Goto effects for branching |
| `03_code_gen_retry.hs` | Retry loop | Goto for loops, Goto Exit, Tools |
| `04_approval_workflow.hs` | Human-in-the-loop | When conditional, Vision, RequestInput |
| `05_parallel_analysis.hs` | Fan-out/fan-in | Parallel via Needs, join via multiple Goto |

## Pattern Coverage

| Pattern | Example | Description |
|---------|---------|-------------|
| `LLM + Schema` (implicit flow) | All | LLM output flows to whoever Needs it |
| `Logic + Goto` (explicit flow) | All | Logic nodes transition via Goto effect |
| `Goto Exit` | 01-05 | Terminate graph with result |
| Multiple `Goto` (branching) | 02, 03, 04 | Handler chooses which goto to call |
| `When` conditional | 04 | Node only runs if condition met |
| `Vision` | 04 | Image/photo analysis |
| `Tools` | 03 | LLM with tool invocation |
| `RequestInput` | 04 | Human-in-the-loop interaction |
| Loops via `Goto` | 03, 04 | Goto back to earlier node |
| Fan-in (join) | 01, 05 | Multiple Goto to same target |
| `Groups` annotation | 02, 05 | Organize nodes for Mermaid |
| `Requires` annotation | 01, 05 | Explicit effect requirements |

## Edge Derivation

Two sources of edges:

| Source | Type | Description |
|--------|------|-------------|
| `Schema X` → `Needs '[..X..]` | Implicit | LLM output flows to whoever needs it |
| `Goto "target" X` | Explicit | Logic node transitions to named target |

## Patterns Demonstrated

### Linear Flow (01)
```
Entry → embed(LLM) → retrieve(Logic) --Goto--> generate(LLM) → format(Logic) --Goto--> Exit
```

### Multi-Way Branch (02)
```
Entry → classify(LLM) → route(Logic) --Goto--> refund(LLM)    → format --Goto--> Exit
                                     --Goto--> technical(LLM) → format --Goto--> Exit
                                     --Goto--> billing(LLM)   → format --Goto--> Exit
                                     --Goto--> general(LLM)   → format --Goto--> Exit
```

### Retry Loop (03)
```
Entry → generateInitial(LLM) → runTests(Logic) --Goto--> evaluate(Logic) --Goto--> Exit
                 ^                                              |
                 |                                              v
                 +---- generateRetry(LLM) <--Goto---------------+
```

### Human-in-the-Loop with Conditional (04)
```
Entry -+-> analyzeAttachments(LLM, When) -.-> draft(LLM) → review(Logic) --Goto--> handle(Logic) --Goto--> Exit
       |                                                          ^                     |
       +----------------------------------------------------------+                     v
                                                                  +--- revise(LLM) <----+
```

### Fan-Out/Fan-In (05)
```
         +-> sentiment(Logic) --Goto--+
Entry ---+-> topics(Logic) ----Goto---+--> synthesize(LLM) → format(Logic) --Goto--> Exit
         +-> entities(Logic) --Goto---+
```

## Node Kinds

| Kind | Inputs | Outputs | Effects |
|------|--------|---------|---------|
| `LLM` | `Needs '[...]` | `Schema X` (implicit) | Implicit LLM effect |
| `Logic` | `Needs '[...]` | Via `Goto` (explicit) | `Eff '[... Goto ...]` |

## Key Syntax

```haskell
type MyGraph = Graph '[
    Entry :~> InputType

  , "llmNode" := LLM
      :@ Needs '[Dep1, Dep2]
      :@ When Condition           -- Optional: conditional
      :@ Template/Vision          -- Prompt source
      :@ Tools '[Tool1, Tool2]    -- Optional: tools
      :@ Schema OutputType        -- Implicit output

  , "logicNode" := Logic
      :@ Needs '[Dep1]
      :@ Eff '[
          State MyState           -- Optional: other effects
        , Goto "target1" Type1    -- Can transition here
        , Goto "target2" Type2    -- Or here
        , Goto Exit ResultType    -- Or exit
        ]

  , Exit :<~ ResultType
  ]
    :& Groups '[...]      -- Optional
    :& Requires '[...]    -- Optional
```

## The Goto Effect

```haskell
-- Definition
data Goto (target :: k) (a :: Type) :: Effect

-- Usage in handlers
goto :: forall target a. Goto target a :> es => a -> Eff es ()

-- Examples
goto @"nextNode" someData
goto @Exit finalResult
```

## Running

These are sketches, not runnable code. They demonstrate the DSL syntax
and patterns. See `docs/type_dsl.md` for the full specification.
