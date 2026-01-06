---
name: tidepool-v2-graph
description: Use when working with v2 graph DSL, LLMHandler variants, or porting agents to the record-based graph system.
---

# V2 Graph DSL Quick Reference

The v2 graph uses record-based (Servant-style) syntax with mode-parameterized records.

## Graph Structure

```haskell
data MyGraph mode = MyGraph
  { entry     :: mode :- Entry InputType
  , myHandler :: mode :- LLMHandler :@ Input (Dep1, Dep2)
                                    :@ UsesEffects '[Log, Goto Exit OutputType]
  , exit      :: mode :- Exit OutputType
  }
```

**Two modes**:
- `AsGraph` - Type-level specification (structure, edges, effect requirements)
- `AsHandler` - Value-level handlers (actual implementations)

## LLMHandler

LLM handlers use the `LLMHandler` constructor with named record fields:

```haskell
sgProcess = LLMHandler
  { llmSystem = Nothing
  , llmUser   = templateCompiled @ProcessTpl
  , llmBefore = \input -> do
      st <- get @SessionState
      pure ProcessContext
        { topic = msgContent input
        , history = take 3 $ stPastTopics st
        }
  , llmAfter  = \output -> case output of
      RefundIntent -> pure $ gotoChoice @"refund" output
      _ -> pure $ gotoExit response
  }
```

All four fields are required:
- `llmSystem`: System template (use `Nothing` if not needed)
- `llmUser`: User template (required)
- `llmBefore`: Builds template context from input
- `llmAfter`: Routes based on LLM output

## Typed Routing

```haskell
-- Handler returns GotoChoice
result <- classify input
case result of
  GotoChoice choice -> dispatchGoto graph choice  -- Typeclass dispatch
```

**Key types**:
- `GotoChoice targets ret` - Wraps `OneOf targets ret`
- `DispatchGoto` - Typeclass for typed dispatch (no Dynamic!)

## Porting Pattern (DM Example)

```haskell
data DMGraph mode = DMGraph
  { entry      :: mode :- Entry UserInput
  , classify   :: mode :- LLMHandler :@ Input UserInput
                                     :@ UsesEffects '[Log, Telegram, Goto ChoiceOfThree Intent]
  , scene      :: mode :- LLMHandler :@ Input SceneState
                                     :@ UsesEffects '[Log, Telegram, Habitica, Goto Exit DMResponse]
  , action     :: mode :- LLMHandler :@ Input ActionState
                                     :@ UsesEffects '[Log, Telegram, Habitica, Goto Exit DMResponse]
  , downtime   :: mode :- LLMHandler :@ Input DowntimeState
                                     :@ UsesEffects '[Log, Telegram, Goto Exit DMResponse]
  , exit       :: mode :- Exit DMResponse
  }
```

## Effects Available

In LLMHandler implementations, use these via smart constructors:

```haskell
-- From Tidepool.Wasm.Effect
logInfo :: Text -> Eff effs ()
telegramSend :: Text -> Eff effs Int
telegramAsk :: Text -> [(Text, Text)] -> Eff effs TelegramAskResult
llmCall :: Text -> [WireMessage] -> Maybe Value -> [Value] -> Eff effs LlmCallResult
habitica :: HabiticaOp a -> Eff effs a
```

## Tool Use Pattern

For LLM-driven tool calling (requires PR #95):

```haskell
result <- llmCall "extract" messages (Just schema) [askUserToolSchema]
case result of
  LlmDone content -> parseContent content
  LlmNeedsTools toolCalls -> do
    results <- forM toolCalls $ \call -> case wtcFunction call of
      "ask_user" -> do
        let args = parseArgs (wtcInput call)
        telegramAsk (args.question) (maybe [] id args.options)
    llmCall "extract" (messages ++ toolResultMsgs results) schema tools
```

## Common Patterns

1. **State in context** - Use `Input StateType` + `LLMBefore` to build context
2. **Branching** - Use `ChoiceOfN` for multi-way routing
3. **Tool loops** - Handler owns the loop, calling `llmCall` until `LlmDone`
4. **Telegram UI** - Use `telegramAsk` for buttons, `telegramSend` for narration
