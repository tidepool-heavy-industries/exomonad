# Task 02: Templates and Schemas for LLM Node

**Epic:** [00-epic-docgen-graph.md](00-epic-docgen-graph.md)
**Depends On:** [01-docgen-graph-design.md](01-docgen-graph-design.md)
**Blocks:** 03

## Goal

Create the template and schema for the `dgSelect` LLM node:
- Jinja template that renders the selection prompt
- Output schema for structured response
- Template context type with ToGVal instance

## Context

### Current Prototype

`Scout/DocGen/Gemma.hs` formats prompts inline:

```haskell
formatSelectionPrompt :: Text -> LSPSymbol -> [Text] -> Text
formatSelectionPrompt topic sym candidates = T.unlines
  [ "Topic: " <> topic
  , "Symbol: " <> lsName sym
  , "Signature: " <> cleanSignature (lsSignature sym)
  , "Candidates: " <> T.intercalate ", " candidates
  , ""
  , "Select the candidates that help understand this symbol in context of the topic."
  ]
```

### Target: Typed Jinja Template

```haskell
-- Template context type
data SelectContext = SelectContext
  { scTopic :: Text
  , scSymbolName :: Text
  , scSignature :: Text
  , scDocComment :: Maybe Text
  , scCandidates :: [Text]
  }

-- Template definition
instance TemplateDef SelectTpl where
  type TemplateContext SelectTpl = SelectContext
  templateName = "select_symbols"
  templateCompiled = $(typedTemplateFile ''SelectContext "templates/select_symbols.jinja")
```

## Acceptance Criteria

- [ ] **Template file exists** at `templates/select_symbols.jinja`
- [ ] **Template renders correctly** with SelectContext
- [ ] **Schema is valid** - SelectOutput has HasJSONSchema + FromJSON
- [ ] **No oneOf in schema** - Anthropic structured output compatible
- [ ] **Template context has ToGVal** - Can be rendered by ginger
- [ ] **Template compiles at build time** - TH splice succeeds

## Subtasks

### 2.1 Create Template File

`control-server/templates/select_symbols.jinja`:

```jinja
You are analyzing Haskell code to identify relevant type dependencies.

## Topic
{{ topic }}

## Symbol Under Analysis
**Name:** {{ symbol_name }}
**Signature:**
```haskell
{{ signature }}
```
{% if doc_comment %}
**Documentation:** {{ doc_comment }}
{% endif %}

## Candidate Types
The following types appear in the signature:
{% for candidate in candidates %}
- {{ candidate }}
{% endfor %}

## Task
Select which candidates are **directly relevant** to understanding this symbol in the context of "{{ topic }}".

Criteria for relevance:
- The type is domain-specific (not a primitive like Int, Text, Maybe)
- Understanding this type helps understand the symbol's purpose
- The type is part of the symbol's core functionality, not incidental

Respond with your selection using the provided schema.
```

### 2.2 Define Template Context

```haskell
-- In Graph/Templates.hs
data SelectContext = SelectContext
  { scTopic :: Text
  , scSymbolName :: Text
  , scSignature :: Text
  , scDocComment :: Maybe Text
  , scCandidates :: [Text]
  }
  deriving (Generic)

-- ToGVal instance for template rendering
instance ToGVal m SelectContext where
  toGVal ctx = dict
    [ ("topic", toGVal $ scTopic ctx)
    , ("symbol_name", toGVal $ scSymbolName ctx)
    , ("signature", toGVal $ scSignature ctx)
    , ("doc_comment", toGVal $ scDocComment ctx)
    , ("candidates", toGVal $ scCandidates ctx)
    ]
```

### 2.3 Define Output Schema

```haskell
-- In Graph/Types.hs
data SelectOutput = SelectOutput
  { soSelected :: [Text]
  , soReasoning :: Maybe Text  -- Optional: why these were selected
  }
  deriving (Generic, FromJSON, ToJSON, HasJSONSchema)

-- Verify no oneOf (this is a simple record, should be fine)
```

### 2.4 Define TemplateDef Instance

```haskell
-- Template marker type
data SelectTpl

instance TemplateDef SelectTpl where
  type TemplateContext SelectTpl = SelectContext
  type TemplateConstraint SelectTpl es = ()

  templateName = "select_symbols"
  templateDescription = "Select relevant type dependencies from candidates"
  templateCompiled = $(typedTemplateFile ''SelectContext "templates/select_symbols.jinja")
  buildContext = error "Use handler's before function"
```

### 2.5 Wire into LLM Node

```haskell
-- In Graph.hs
dgSelect :: mode :- LLMNode
    :@ Input SelectInput
    :@ Template SelectTpl
    :@ Schema SelectOutput
```

## Files to Create/Modify

| File | Purpose |
|------|---------|
| `control-server/templates/select_symbols.jinja` | Prompt template |
| `control-server/src/Tidepool/Control/Scout/Graph/Templates.hs` | SelectContext, SelectTpl, TemplateDef |
| `control-server/src/Tidepool/Control/Scout/Graph/Types.hs` | SelectOutput schema |
| `control-server/tidepool-control-server.cabal` | Add templates to data-files |

## Cabal Configuration

```cabal
data-files:
  templates/*.jinja

-- Or use TemplateHaskell to embed at compile time
```

## Verification

```bash
# Template compiles
cabal build tidepool-control-server

# Schema is valid JSON Schema
cabal repl tidepool-control-server
>>> import Tidepool.Schema
>>> import Tidepool.Control.Scout.Graph.Types
>>> schemaToValue (jsonSchema @SelectOutput)
-- Should not contain "oneOf"

# Template renders
>>> import Text.Ginger
>>> import Tidepool.Control.Scout.Graph.Templates
>>> let ctx = SelectContext "scoring" "compositeScore" "Config -> Int" Nothing ["Config", "Int"]
>>> renderTemplate (templateCompiled @SelectTpl) ctx
-- Should produce valid prompt text
```

## Notes

- Template is intentionally verbose to help Haiku understand the task
- Reasoning field is optional - useful for teaching data but not required
- Candidates are presented as a list, not enum-constrained (Haiku picks from list)
