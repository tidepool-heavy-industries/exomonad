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
  , myHandler :: mode :- LLMHandler :@ Needs '[Dep1, Dep2]
                                    :@ UsesEffects '[Log, Goto Exit OutputType]
  , exit      :: mode :- Exit OutputType
  }
```

**Two modes**:
- `AsGraph` - Type-level specification (structure, edges, effect requirements)
- `AsHandler` - Value-level handlers (actual implementations)

## LLMHandler Variants

### LLMBefore - Implicit Routing

Build context, let `Needs` drive routing from output:

```haskell
sgClassify :: LLMHandler AsHandler '[UserMessage] '[Log, Goto ChoiceOfThree Intent]
sgClassify = LLMBefore $ \msg -> do
  st <- get @SessionState
  pure ClassifyContext
    { topic = msgContent msg
    , history = take 3 $ stPastTopics st
    }
```

Template returns `Intent`, `Needs '[Intent]` implies routing.

### LLMAfter - Explicit Routing

Use default context, route based on output:

```haskell
rgRoute :: LLMHandler AsHandler '[Intent] '[Goto ChoiceOfTwo Response]
rgRoute = LLMAfter $ \intent -> pure $ case intent of
  IntentRefund   -> gotoChoice @"rgProcess" intent
  IntentQuestion -> gotoExit (Response "FAQ response")
```

### LLMBoth - Full Control

Both phases explicit:

```haskell
rgProcess :: LLMHandler AsHandler '[Intent] '[Goto Exit Response]
rgProcess = LLMBoth
  Nothing  -- No Needs (manual context)
  (templateCompiled @RefundTpl)
  (\intent -> pure SimpleContext { intentText = show intent })  -- before
  (pure . gotoExit)                                              -- after
```

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
  , classify   :: mode :- LLMHandler :@ Needs '[UserInput]
                                     :@ UsesEffects '[Log, Telegram, Goto ChoiceOfThree Intent]
  , scene      :: mode :- LLMHandler :@ Needs '[SceneState]
                                     :@ UsesEffects '[Log, Telegram, Habitica, Goto Exit DMResponse]
  , action     :: mode :- LLMHandler :@ Needs '[ActionState]
                                     :@ UsesEffects '[Log, Telegram, Habitica, Goto Exit DMResponse]
  , downtime   :: mode :- LLMHandler :@ Needs '[DowntimeState]
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

1. **State in context** - Use `Needs '[StateType]` + `LLMBefore` to build context
2. **Branching** - Use `ChoiceOfN` for multi-way routing
3. **Tool loops** - Handler owns the loop, calling `llmCall` until `LlmDone`
4. **Telegram UI** - Use `telegramAsk` for buttons, `telegramSend` for narration
