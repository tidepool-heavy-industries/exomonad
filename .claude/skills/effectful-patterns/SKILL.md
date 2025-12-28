---
name: effectful-patterns
description: Use when working with the effectful library, effect stacks, effect handlers, or debugging effect-related type errors in this Haskell codebase.
---

# Effectful Patterns in Tidepool

This codebase uses the `effectful` library for effect management.

## Core Effect Stack

```haskell
type GameEffects s event =
  '[ LLM
   , RequestInput
   , Log
   , ChatHistory
   , State s
   , Emit event
   , Random
   , IOE
   ]
```

## Key Patterns

### Defining Effects

```haskell
data MyEffect :: Effect where
  DoThing :: Arg -> MyEffect m Result

type instance DispatchOf MyEffect = 'Dynamic

doThing :: MyEffect :> es => Arg -> Eff es Result
doThing = send . DoThing
```

### Running Effects

Effects are interpreted by handlers. Order matters (outermost runs first):

```haskell
runEff
  . runRandom
  . runEmit eventHandler
  . runState initialState
  . runLLMWithTools config dispatcher  -- needs effects below it
  $ computation
```

### Constraint Pattern

Use `:>` for effect constraints:

```haskell
myFunction 
  :: ( State WorldState :> es
     , Emit DMEvent :> es
     , Random :> es
     )
  => Eff es ()
```

## Tool Execution in Effects

Tools run inside the effect stack, so they can use `emit`, `get`, `put`, `requestChoice`, etc:

```haskell
executeTool input = do
  state <- get
  emit (SomeEvent input)
  modify (\s -> s { field = newValue })
  return result
```

## ToolBreak for State Transitions

Transition tools (engage, resolve, accept) return `ToolBreak` to restart the turn:

```haskell
-- In dispatcher
if name `elem` transitionToolNames
  then return (Right (ToolBreak ("mood transition: " <> name)))
  else return (Right (ToolSuccess val))
```

## Common Issues

1. **Effect order matters** - `runLLMWithTools` needs `State`, `Emit`, `RequestInput` below it
2. **Type annotations** - Sometimes need `@WorldState` to disambiguate: `get @WorldState`
3. **Stub vs real tools** - `runLLM` uses stubs, `runLLMWithTools` uses real dispatcher
