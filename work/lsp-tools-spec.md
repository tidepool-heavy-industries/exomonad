# LSP Tool Specifications

Specialized tools for Haskell codebase analysis via small-LLM-orchestrated LSP calls.

## Overview

7 tools that combine LSP operations with lightweight LLM classification to answer specific code analysis questions. Each tool:

- Takes structured JSON input
- Orchestrates multiple LSP calls (workspace/symbol, hover, references, definition)
- Uses small LLM (Haiku/Gemma) for semantic classification
- Returns structured JSON output
- Captures LSP sequences as training data for FunctionGemma

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│ MCP Tool Call (from Claude Code)                            │
│   { "tool": "impact-analysis", "args": {...} }              │
└─────────────────────────────────┬───────────────────────────┘
                                  │
                                  ▼
┌─────────────────────────────────────────────────────────────┐
│ Tool Orchestrator                                           │
│   1. Parse input                                            │
│   2. Execute LSP sequence                                   │
│   3. Collect intermediate results                           │
│   4. Call small LLM for classification                      │
│   5. Aggregate and format output                            │
└─────────────────────────────────┬───────────────────────────┘
                                  │
        ┌─────────────────────────┼─────────────────────────┐
        ▼                         ▼                         ▼
┌───────────────┐       ┌─────────────────┐       ┌─────────────────┐
│ LSP Session   │       │ Small LLM       │       │ Training Data   │
│ (HLS via TCP) │       │ (Haiku/Gemma)   │       │ (JSONL capture) │
└───────────────┘       └─────────────────┘       └─────────────────┘
```

## Shared Infrastructure

Located in `haskell/control-server/`:

```haskell
-- Existing
withLSPSession :: FilePath -> (LSPSession -> IO a) -> IO a
workspaceSymbol :: Text -> Eff '[LSP] [SymbolInformation]
hover :: TextDocumentIdentifier -> Position -> Eff '[LSP] (Maybe HoverInfo)
references :: Location -> Eff '[LSP] [Location]
definition :: Location -> Eff '[LSP] [Location]

-- New (to implement)
data ToolResult a = ToolResult
  { trOutput :: a
  , trLSPCalls :: [LSPCall]      -- For training data
  , trLLMCalls :: [LLMCall]      -- For training data
  }
```

---

## Tool 1: impact-analysis

**Purpose**: What breaks if I modify a type (add field, remove constructor, etc.)?

### Input Schema

```typescript
interface ImpactAnalysisInput {
  type: string;           // "ExploreState"
  change: "add-field" | "remove-field" | "add-constructor" | "remove-constructor";
  detail?: string;        // "esNewField :: Text" for add-field
}
```

```json
{
  "type": "object",
  "properties": {
    "type": { "type": "string", "description": "Type name to analyze" },
    "change": {
      "type": "string",
      "enum": ["add-field", "remove-field", "add-constructor", "remove-constructor"]
    },
    "detail": { "type": "string", "description": "Field/constructor being added/removed" }
  },
  "required": ["type", "change"]
}
```

### Output Schema

```typescript
interface ImpactAnalysisOutput {
  typeLocation: string;         // "Types.hs:45"
  impact: {
    constructorCalls: Array<{
      location: string;
      function: string;
      safe: boolean;
      reason: string;
      suggestedFix?: string;
    }>;
    patternMatches: Array<{
      location: string;
      function: string;
      safe: boolean;
      reason: string;           // "uses record wildcards" | "exhaustive match"
      suggestedFix?: string;
    }>;
    recordUpdates: Array<{
      location: string;
      function: string;
      safe: boolean;
      reason: string;
    }>;
    typeSignatures: Array<{
      location: string;
      function: string;
      safe: boolean;
    }>;
  };
  summary: {
    totalReferences: number;
    safeReferences: number;
    unsafeReferences: number;
    requiredChanges: number;
  };
}
```

### LSP Orchestration

```
1. workspaceSymbol(type) → find type definition location
2. definition(typeLocation) → confirm and get full definition
3. references(type) → all usage sites
4. For each reference:
   a. Read surrounding context (±5 lines via file read)
   b. LLM: Classify reference type
   c. LLM: Assess safety based on change type
5. Aggregate results by category
```

### LLM Classification Task

**System prompt**:
```
You are a Haskell code analyzer. You classify how a type is used at a specific location.
```

**User template**:
```
Classify this usage of type `{type_name}` at {location}:

```haskell
{context_lines}
```

The reference is on line {ref_line}.

Classify as one of:
- constructor_call: The type is being constructed (e.g., `MyType { field = value }` or `MyType value`)
- pattern_match: The type is being destructured (e.g., `case x of MyType { field } -> ...`)
- record_update: A record update (e.g., `x { field = newValue }`)
- type_signature: Used in a type signature only
- import: Just an import statement
- other: None of the above

Also assess:
- uses_wildcards: Does the pattern use `..` or `_` for unmatched fields?
- is_exhaustive: Does the pattern match all constructors/fields?

Respond with JSON:
{
  "classification": "constructor_call" | "pattern_match" | "record_update" | "type_signature" | "import" | "other",
  "uses_wildcards": boolean,
  "is_exhaustive": boolean,
  "confidence": 0.0-1.0,
  "reasoning": "brief explanation"
}
```

### Test Case

```json
{
  "input": {
    "type": "ExploreState",
    "change": "add-field",
    "detail": "esMaxDepth :: Int"
  },
  "project": "/home/inanna/exomonad-heavy-industries/haiku-training-data-synthesis-woktree",
  "expectedLocations": [
    "haskell/control-server/src/ExoMonad/Control/Scout/Graph/Types.hs",
    "haskell/control-server/src/ExoMonad/Control/Scout/Graph/Handlers.hs"
  ],
  "setupNotes": "Requires HLS indexed on exomonad-control-server package"
}
```

---

## Tool 2: dead-data-flows

**Purpose**: Find pattern variables prefixed with `_` that indicate ignored data, trace where that data comes from.

### Input Schema

```typescript
interface DeadDataFlowsInput {
  path: string;           // Directory or file to scan
  filePattern?: string;   // Glob pattern, e.g., "**/Handler*.hs"
}
```

```json
{
  "type": "object",
  "properties": {
    "path": { "type": "string", "description": "Directory or file to scan" },
    "filePattern": { "type": "string", "description": "Optional glob pattern" }
  },
  "required": ["path"]
}
```

### Output Schema

```typescript
interface DeadDataFlowsOutput {
  deadEnds: Array<{
    location: string;           // "ImplExit.hs:42"
    variable: string;           // "_diagnosis"
    variableType: string;       // "CritiqueList"
    typeDefinition: string;     // "Types.hs:87"

    producer?: {
      location: string;         // Where this data is created
      function: string;
      howProduced: string;      // "returned from v3Critique node"
    };

    potentialConsumers: Array<{
      location: string;
      field: string;            // Field that could receive this data
      currentValue: string;     // What's currently assigned there
    }>;

    severity: "high" | "medium" | "low";
    suggestion: string;
  }>;

  summary: {
    totalIgnored: number;
    highSeverity: number;
    mediumSeverity: number;
    lowSeverity: number;
  };
}
```

### LSP Orchestration

```
1. Grep for pattern: `_[a-z][a-zA-Z0-9]*` in .hs files within path
2. For each match:
   a. Parse to confirm it's a pattern binding (not comment/string)
   b. hover(location) → get type of the ignored binding
   c. workspaceSymbol(type) → find type definition
   d. references(type) → find where type is constructed (producer)
   e. Analyze surrounding function for potential consumer fields
3. LLM: Assess severity and suggest fix
4. Aggregate and sort by severity
```

### LLM Classification Task

**System prompt**:
```
You are a Haskell code quality analyzer. You assess whether an ignored pattern variable (`_foo`) represents a potential bug or intentional design.
```

**User template**:
```
Analyze this ignored variable:

Location: {location}
Variable: {variable}
Type: {type}

Context:
```haskell
{context_lines}
```

The variable `{variable}` is bound but ignored (prefixed with `_`).

Known producer: {producer_info}
Potential consumers in scope: {consumer_fields}

Assess:
1. Is this likely a bug (data should flow forward) or intentional (data not needed)?
2. If a bug, where should this data flow to?
3. Severity: high (definite bug), medium (suspicious), low (probably intentional)

Respond with JSON:
{
  "severity": "high" | "medium" | "low",
  "intentional": boolean,
  "reason": "explanation",
  "suggestedFix": "description of fix" | null,
  "suggestedCode": "code snippet" | null
}
```

### Test Case

```json
{
  "input": {
    "path": "haskell/control-server/src/ExoMonad/Control/Scout/Graph/",
    "filePattern": "**/*.hs"
  },
  "project": "/home/inanna/exomonad-heavy-industries/haiku-training-data-synthesis-woktree",
  "notes": "Should find _header in renderSection if that pattern exists"
}
```

---

## Tool 3: constraint-propagation

**Purpose**: If I add an effect constraint to a function, what call sites need updates?

### Input Schema

```typescript
interface ConstraintPropagationInput {
  function: string;           // "exploreEff"
  newConstraint: string;      // "Member LSP effs"
}
```

```json
{
  "type": "object",
  "properties": {
    "function": { "type": "string", "description": "Function to add constraint to" },
    "newConstraint": { "type": "string", "description": "Haskell constraint to add" }
  },
  "required": ["function", "newConstraint"]
}
```

### Output Schema

```typescript
interface ConstraintPropagationOutput {
  targetFunction: {
    location: string;
    currentSignature: string;
    newSignature: string;
  };

  directCallers: Array<{
    function: string;
    location: string;
    currentConstraints: string[];
    hasConstraint: boolean;
    status: "safe" | "needs-constraint" | "needs-interpreter";
    suggestedFix?: string;
  }>;

  transitiveCallers: Array<{
    function: string;
    location: string;
    callChain: string[];        // ["main", "runApp", "handleRequest", "exploreEff"]
    status: "safe" | "needs-constraint" | "needs-interpreter";
    interpreterNeeded?: string; // "runLSP" if this is where effect should be handled
  }>;

  interpreterLocations: Array<{
    interpreter: string;        // "runLSP"
    location: string;
    breaksChain: boolean;       // true = constraint doesn't propagate past here
  }>;

  summary: {
    totalCallers: number;
    safeCallers: number;
    needsConstraint: number;
    needsInterpreter: number;
  };
}
```

### LSP Orchestration

```
1. workspaceSymbol(function) → find function definition
2. hover(functionLocation) → get current type signature with constraints
3. references(function) → all call sites
4. For each caller:
   a. Identify the calling function (parse surrounding context)
   b. hover(callerFunction) → get caller's constraints
   c. LLM: Parse constraints, check if newConstraint is present
   d. If not present: references(callerFunction) → recurse
5. Track call chains until:
   - Constraint is present (safe)
   - Hit an interpreter (e.g., runLSP) - chain breaks here
   - Hit IO/main (needs interpreter)
6. Aggregate by propagation path
```

### LLM Classification Task

**System prompt**:
```
You are a Haskell effect system analyzer. You parse type signatures to identify effect constraints.
```

**User template**:
```
Parse the constraints from this Haskell type signature:

```haskell
{signature}
```

Target constraint to find: `{target_constraint}`

Extract:
1. All constraints (the part before `=>`)
2. Whether the target constraint is present (exact match or subsumed)
3. If this is an effect interpreter (like `runLSP`, `runM`, `interpretEffect`)

Respond with JSON:
{
  "constraints": ["Member LSP effs", "Member Log effs", "MonadIO m"],
  "hasTargetConstraint": boolean,
  "isInterpreter": boolean,
  "interpreterEffect": "LSP" | null,
  "effectVariable": "effs" | "es" | null
}
```

### Test Case

```json
{
  "input": {
    "function": "lookupSymbol",
    "newConstraint": "Member Log effs"
  },
  "project": "/home/inanna/exomonad-heavy-industries/haiku-training-data-synthesis-woktree",
  "expectedCallers": ["processHandler", "resolveSeedSymbols", "resolveTokens"]
}
```

---

## Tool 4: effect-usage-patterns

**Purpose**: How do different functions use a specific effect? Categorize usage patterns.

### Input Schema

```typescript
interface EffectUsagePatternsInput {
  effect: string;         // "Memory" or "LSP"
  scope?: string;         // Directory to search, defaults to project root
  groupBy?: "file" | "function" | "pattern";
}
```

```json
{
  "type": "object",
  "properties": {
    "effect": { "type": "string", "description": "Effect name (e.g., Memory, LSP, LLM)" },
    "scope": { "type": "string", "description": "Directory scope" },
    "groupBy": { "type": "string", "enum": ["file", "function", "pattern"] }
  },
  "required": ["effect"]
}
```

### Output Schema

```typescript
interface EffectUsagePatternsOutput {
  effect: string;
  totalUsages: number;

  patterns: Array<{
    pattern: "initialize" | "read-only" | "write-only" | "read-modify-write" | "conditional";
    count: number;
    functions: Array<{
      function: string;
      location: string;
      effectType?: string;        // For Memory: the state type
      operations: string[];       // ["getMem", "putMem"] or ["workspaceSymbol", "hover"]
    }>;
  }>;

  byFile?: Record<string, {
    functions: string[];
    dominantPattern: string;
  }>;

  anomalies: Array<{
    function: string;
    location: string;
    issue: string;              // "has constraint but never uses effect"
  }>;
}
```

### LSP Orchestration

```
1. Grep for `Member ({effect}` in .hs files within scope
2. For each match:
   a. Identify the function with this constraint
   b. definition(function) → get full function body
   c. Search function body for effect operations:
      - Memory: getMem, putMem, updateMem, modifyMem
      - LSP: workspaceSymbol, hover, references, definition
      - LLM: runTurn, RunTurnOp
   d. LLM: Classify usage pattern
3. Group by pattern, detect anomalies
```

### LLM Classification Task

**System prompt**:
```
You are a Haskell effect pattern classifier. You identify how effects are used in function bodies.
```

**User template**:
```
Classify the {effect} effect usage pattern in this function:

```haskell
{function_body}
```

Operations found: {operations}

Classify as:
- initialize: Only writes (putMem, first use)
- read-only: Only reads (getMem)
- write-only: Only writes after initialization
- read-modify-write: Reads, transforms, writes back
- conditional: Effect use depends on runtime conditions

Also check:
- Is the effect constraint actually used? (has constraint but no operations = anomaly)
- Is there a clear pattern or is it mixed/unclear?

Respond with JSON:
{
  "pattern": "initialize" | "read-only" | "write-only" | "read-modify-write" | "conditional" | "unused" | "mixed",
  "confidence": 0.0-1.0,
  "operations": ["getMem", "updateMem"],
  "isAnomaly": boolean,
  "anomalyReason": string | null
}
```

### Test Case

```json
{
  "input": {
    "effect": "Memory",
    "scope": "haskell/control-server/src/ExoMonad/Control/Scout/Graph/"
  },
  "project": "/home/inanna/exomonad-heavy-industries/haiku-training-data-synthesis-woktree",
  "expectedPatterns": ["initialize", "read-modify-write", "read-only"]
}
```

---

## Tool 5: type-instantiations

**Purpose**: Find all concrete instantiations of a polymorphic type parameter.

### Input Schema

```typescript
interface TypeInstantiationsInput {
  function: string;       // "getMem"
  typeParam: string;      // "s"
}
```

```json
{
  "type": "object",
  "properties": {
    "function": { "type": "string", "description": "Polymorphic function name" },
    "typeParam": { "type": "string", "description": "Type parameter to track (e.g., 's', 'a')" }
  },
  "required": ["function", "typeParam"]
}
```

### Output Schema

```typescript
interface TypeInstantiationsOutput {
  function: string;
  typeParam: string;
  signature: string;

  instantiations: Array<{
    concreteType: string;       // "ExploreState"
    callSites: Array<{
      location: string;         // "Handlers.hs:154"
      callExpression: string;   // "getMem @ExploreState"
      inFunction: string;       // "processHandler"
    }>;
    typeInfo: {
      definition: string;       // "Types.hs:45"
      kind: "record" | "sum" | "newtype" | "type-alias";
      fields?: string[];        // For records
      constructors?: string[];  // For sum types
    };
  }>;

  summary: {
    totalCallSites: number;
    uniqueTypes: number;
  };
}
```

### LSP Orchestration

```
1. workspaceSymbol(function) → find function definition
2. hover(function) → get signature, identify type param position
3. references(function) → all call sites
4. For each call site:
   a. Read surrounding context
   b. LLM: Extract type application (getMem @Foo → "Foo")
   c. If no explicit type app: hover(callSite) → infer from context
5. For each unique concrete type:
   a. workspaceSymbol(type) → find definition
   b. hover(type) → get structure (record fields, constructors)
6. Group by concrete type
```

### LLM Classification Task

**System prompt**:
```
You are a Haskell type application parser. You extract concrete types from function calls.
```

**User template**:
```
Extract the concrete type instantiation from this call site:

Function signature:
```haskell
{function_signature}
```

Call site at {location}:
```haskell
{context_lines}
```

The call to `{function}` is on line {line}.
We're looking for what type is used for parameter `{type_param}`.

Look for:
1. Explicit type application: `{function} @ConcreteType`
2. Type annotation: `({function} :: SomeType)`
3. Inference from surrounding context

Respond with JSON:
{
  "concreteType": "TypeName" | null,
  "source": "explicit-application" | "annotation" | "inference" | "unknown",
  "confidence": 0.0-1.0,
  "callExpression": "the relevant code snippet"
}
```

### Test Case

```json
{
  "input": {
    "function": "getMem",
    "typeParam": "s"
  },
  "project": "/home/inanna/exomonad-heavy-industries/haiku-training-data-synthesis-woktree",
  "expectedTypes": ["ExploreState"]
}
```

---

## Tool 6: trace-effect-calls

**Purpose**: What effect operations does a function use transitively?

### Input Schema

```typescript
interface TraceEffectCallsInput {
  function: string;       // "handleScoutTool"
  effect: string;         // "LSP"
  maxDepth?: number;      // Default 5
}
```

```json
{
  "type": "object",
  "properties": {
    "function": { "type": "string", "description": "Entry function" },
    "effect": { "type": "string", "description": "Effect to trace" },
    "maxDepth": { "type": "integer", "description": "Max call depth", "default": 5 }
  },
  "required": ["function", "effect"]
}
```

### Output Schema

```typescript
interface TraceEffectCallsOutput {
  function: string;
  effect: string;

  direct: Array<{
    operation: string;          // "workspaceSymbol"
    location: string;           // "Scout.hs:87"
    arguments?: string;         // "seedName"
  }>;

  transitive: Array<{
    operation: string;
    location: string;
    callChain: string[];        // ["handleScoutTool", "exploreEff", "lookupSymbol"]
    depth: number;
  }>;

  callGraph: {
    nodes: Array<{
      function: string;
      location: string;
      hasEffect: boolean;
    }>;
    edges: Array<{
      from: string;
      to: string;
    }>;
  };

  summary: {
    uniqueOperations: string[];
    maxDepth: number;
    totalFunctionsTraversed: number;
  };
}
```

### LSP Orchestration

```
1. workspaceSymbol(function) → find entry point
2. definition(function) → get function body
3. Parse function body for:
   a. Direct effect calls (e.g., workspaceSymbol, hover)
   b. Other function calls
4. For each non-effect call:
   a. hover(call) → get signature
   b. Check if has effect constraint
   c. If yes: definition → recurse (up to maxDepth)
5. Build call graph, track effect operations at each node
6. Deduplicate operations across paths
```

### LLM Classification Task

**System prompt**:
```
You are a Haskell function call analyzer. You identify function calls in code.
```

**User template**:
```
Identify all function calls in this Haskell function body:

```haskell
{function_body}
```

Known effect operations for {effect}: {effect_operations}

For each call, classify:
- Is it a direct effect operation?
- Is it a local helper (defined in same module)?
- Is it an imported function (needs LSP lookup)?

Respond with JSON:
{
  "effectCalls": [
    {"operation": "workspaceSymbol", "line": 42, "arguments": "name"}
  ],
  "functionCalls": [
    {"function": "exploreEff", "line": 45, "isLocal": false}
  ]
}
```

### Test Case

```json
{
  "input": {
    "function": "runDocGenGraph",
    "effect": "LSP",
    "maxDepth": 3
  },
  "project": "/home/inanna/exomonad-heavy-industries/haiku-training-data-synthesis-woktree",
  "expectedOperations": ["workspaceSymbol", "hover"]
}
```

---

## Tool 7: template-context-check

**Purpose**: Verify Jinja template variables match context type fields.

### Input Schema

```typescript
interface TemplateContextCheckInput {
  template: string;       // "SelectTpl" or path to template file
  context: string;        // "SelectContext"
}
```

```json
{
  "type": "object",
  "properties": {
    "template": { "type": "string", "description": "Template name or file path" },
    "context": { "type": "string", "description": "Context type name" }
  },
  "required": ["template", "context"]
}
```

### Output Schema

```typescript
interface TemplateContextCheckOutput {
  template: string;
  templateLocation: string;
  context: string;
  contextLocation: string;

  templateVariables: Array<{
    variable: string;           // "topic"
    accessPath: string;         // "topic" or "symbol.name"
    line: number;
  }>;

  contextFields: Array<{
    field: string;              // "topic"
    fieldType: string;          // "Text"
  }>;

  coverage: {
    usedFields: string[];
    unusedFields: string[];
    missingVariables: string[];   // Variables in template but not in context
    invalidPaths: Array<{
      path: string;
      reason: string;
    }>;
  };

  coveragePercent: number;
  isValid: boolean;
}
```

### LSP Orchestration

```
1. Find template:
   a. If path: read file directly
   b. If name: workspaceSymbol(template) → find TemplateDef instance → extract path
2. Parse template for {{ variable }} and {{ object.field }} patterns
3. workspaceSymbol(context) → find type definition
4. hover(context) → extract record fields
5. Compare:
   a. For each template variable, check if field exists
   b. For dotted paths (a.b), verify nested access is valid
6. Report coverage
```

### LLM Classification Task

**System prompt**:
```
You are a Jinja template parser. You extract variable references from templates.
```

**User template**:
```
Extract all variable references from this Jinja template:

```jinja
{template_content}
```

Find:
1. Simple variables: {{ foo }}
2. Dotted access: {{ foo.bar }}
3. Filter usage: {{ foo | filter }}
4. Conditional checks: {% if foo %}

Respond with JSON:
{
  "variables": [
    {"name": "topic", "accessPath": "topic", "line": 3, "inFilter": false},
    {"name": "symbol", "accessPath": "symbol.name", "line": 7, "inFilter": false}
  ],
  "conditionalVars": ["hasDoc"]
}
```

### Test Case

```json
{
  "input": {
    "template": "SelectTpl",
    "context": "SelectContext"
  },
  "project": "/home/inanna/exomonad-heavy-industries/haiku-training-data-synthesis-woktree",
  "expectedFields": ["topic", "symbol_name", "signature", "candidates"]
}
```

---

## Implementation Priority

| Priority | Tool | Value | Complexity |
|----------|------|-------|------------|
| 1 | impact-analysis | High (prevents bugs) | Medium |
| 2 | dead-data-flows | High (finds bugs) | Medium |
| 3 | constraint-propagation | High (effect refactoring) | High |
| 4 | effect-usage-patterns | Medium (codebase learning) | Low |
| 5 | type-instantiations | Medium (understanding) | Medium |
| 6 | trace-effect-calls | Medium (debugging) | High |
| 7 | template-context-check | Low (niche use case) | Low |

## Training Data Format

Each tool execution captures:

```json
{
  "tool": "impact-analysis",
  "input": { "type": "ExploreState", "change": "add-field" },
  "lspSequence": [
    {"call": "workspaceSymbol", "args": "ExploreState", "resultCount": 1},
    {"call": "references", "args": "Types.hs:45:6", "resultCount": 23},
    {"call": "hover", "args": "Handlers.hs:154:12", "result": "..."}
  ],
  "llmCalls": [
    {"prompt": "Classify usage...", "response": {"classification": "constructor_call"}}
  ],
  "output": { ... },
  "executionTimeMs": 1234
}
```

This format allows:
1. Training FunctionGemma on "given question, what LSP calls?"
2. Evaluating LLM classification accuracy
3. Optimizing LSP call sequences

---

## File Organization

```
haskell/control-server/
├── src/ExoMonad/Control/
│   ├── Handler/
│   │   ├── MCP.hs              # Existing MCP routing
│   │   └── Tools/              # NEW: Tool implementations
│   │       ├── ImpactAnalysis.hs
│   │       ├── DeadDataFlows.hs
│   │       ├── ConstraintPropagation.hs
│   │       ├── EffectUsagePatterns.hs
│   │       ├── TypeInstantiations.hs
│   │       ├── TraceEffectCalls.hs
│   │       └── TemplateContextCheck.hs
│   ├── Tools/
│   │   ├── Types.hs            # Shared input/output types
│   │   ├── LSPHelpers.hs       # Common LSP patterns
│   │   └── LLMClassifier.hs    # Shared LLM classification
│   └── ...
├── .exomonad/
│   └── mcp-tools.json          # Tool definitions for MCP
└── ...
```
