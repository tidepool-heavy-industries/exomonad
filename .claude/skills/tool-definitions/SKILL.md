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

instance Tool MyTool MyEvent AppState where
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
describeField "Description here" (emptySchema TString)
```

## Tool Registration

```haskell
-- Type-safe list of tools
myToolList :: ToolList MyEvent AppState '[Tool1, Tool2, Tool3]
myToolList = TCons (Proxy @Tool1)
           $ TCons (Proxy @Tool2)
           $ TCons (Proxy @Tool3)
           $ TNil

-- Convert to JSON for API
myTools :: [Value]
myTools = toolListToJSON myToolList
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
