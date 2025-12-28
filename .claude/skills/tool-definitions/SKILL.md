---
name: tool-definitions
description: Use when defining new tools, creating tool schemas, implementing tool execution, or working with the Tool typeclass and dispatcher.
---

# Defining Tools

Tools are mid-turn capabilities the LLM can invoke. They execute in the effect stack.

## Tool Typeclass

```haskell
class Tool t event s | t -> event s where
  type ToolInput t
  type ToolOutput t
  
  toolName :: Text
  toolDescription :: Text
  inputSchema :: Value  -- JSON Schema
  executeTool :: (State s :> es, Emit event :> es, ...) 
              => ToolInput t -> Eff es (ToolOutput t)
```

## Minimal Example

```haskell
data MyTool = MyTool
  deriving (Show, Eq, Generic)

data MyInput = MyInput { field :: Text }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance Tool MyTool DMEvent WorldState where
  type ToolInput MyTool = MyInput
  type ToolOutput MyTool = ()

  toolName = "my_tool"
  toolDescription = "Does something useful"
  inputSchema = schemaToValue $ objectSchema
    [("field", emptySchema TString)]
    ["field"]  -- required fields

  executeTool input = do
    emit (SomeEvent input.field)
    return ()
```

## Schema Helpers

```haskell
-- Object with fields
objectSchema :: [(Text, Value)] -> [Text] -> Value

-- Array of items
arraySchema :: Value -> Value

-- Enum of strings
enumSchema :: [Text] -> Value

-- Basic types
emptySchema TString
emptySchema TNumber
emptySchema TBool

-- With description
describeField "name" "Description here" (emptySchema TString)
```

## Tool Registration

```haskell
-- Type-safe list of tools
dmToolList :: ToolList DMEvent WorldState '[Tool1, Tool2, Tool3]
dmToolList = TCons (Proxy @Tool1)
           $ TCons (Proxy @Tool2)
           $ TCons (Proxy @Tool3)
           $ TNil

-- Convert to JSON for API
dmTools :: [Value]
dmTools = toolListToJSON dmToolList
```

## Transition Tools

Tools that change mood should be detected in the dispatcher:

```haskell
transitionToolNames :: [Text]
transitionToolNames = ["engage", "resolve", "accept"]

makeDMDispatcher name input = do
  moodBefore <- get @WorldState >>= \s -> return s.mood
  result <- makeDispatcher dmToolList name input
  
  if name `elem` transitionToolNames
    then case result of
      Right (ToolSuccess val) -> do
        moodAfter <- get @WorldState >>= \s -> return s.mood
        if moodChanged moodBefore moodAfter
          then return (Right (ToolBreak ("mood transition: " <> name)))
          else return (Right (ToolSuccess val))
      other -> return other
    else return result
```

## Tools That Request Input

```haskell
executeTool input = do
  -- Present choices to player
  choice <- requestChoice "Pick one:" [("Option A", valA), ("Option B", valB)]
  
  -- Or get free text
  text <- requestText "What do you say?"
  
  return (SomeResult choice)
```
