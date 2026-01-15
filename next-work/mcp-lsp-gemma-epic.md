# Epic: Semantic Scout MCP Server

**Goal**: MCP server that answers fuzzy natural language queries about code by recursively exploring LSP, using FunctionGemma to rate relevance at each step.

## The Problem

When I (Claude) want to understand "what breaks if I change LLMKind", I need to:
1. Find the symbol (maybe ambiguous)
2. Get references (could be 50+)
3. For each: is it relevant? A pattern match? Just an import?
4. Follow interesting paths deeper
5. Know when to stop
6. Synthesize findings

That's 100+ LSP calls and judgment at each step. Tedious, error-prone, context-heavy.

## The Solution

```
┌────────────────────────────────────────────────────────────────────┐
│                                                                    │
│   "LLMKind - adding fourth variant"                               │
│   tags: [exhaustive, pattern-match, type-family]                  │
│                              │                                     │
│                              ▼                                     │
│                    ┌──────────────────┐                           │
│                    │  Semantic Scout  │                           │
│                    │   MCP Server     │                           │
│                    └────────┬─────────┘                           │
│                             │                                      │
│         ┌───────────────────┼───────────────────┐                 │
│         ▼                   ▼                   ▼                  │
│   ┌──────────┐       ┌──────────┐       ┌──────────┐             │
│   │   LSP    │◄─────►│  Gemma   │◄─────►│  Logic   │             │
│   │  Client  │       │ (rubric) │       │(expand?) │             │
│   └──────────┘       └──────────┘       └──────────┘             │
│         │                   │                   │                  │
│         │    "rate these    │   "score > 8,    │                  │
│         │     5 subnodes"   │    expand it"    │                  │
│         │                   │                   │                  │
│         └───────────────────┴───────────────────┘                 │
│                              │                                     │
│                              ▼                                     │
│   ┌────────────────────────────────────────────────────────────┐ │
│   │ "LLMKind @ Types.hs:47. Adding variant breaks:             │ │
│   │  • Edges.hs:89 - NodeHandler type family (must add branch) │ │
│   │  • Interpret.hs:234 - exhaustive pattern match             │ │
│   │  Safe to ignore: 35 imports/signatures"                    │ │
│   └────────────────────────────────────────────────────────────┘ │
│                                                                    │
└────────────────────────────────────────────────────────────────────┘
```

## Key Insight: Separate Judgment from Decision

**FunctionGemma** = semantic judgment (rates each link with a rubric)
**Deterministic logic** = expansion decision (score threshold + budget)

This means:
- Gemma stays narrow/fast (just emit rubric, no reasoning)
- Expansion strategy is tunable without retraining
- Fully debuggable (why was X expanded? check the score)

---

## The Rubric DSL

### What the caller provides

```typescript
interface Query {
  text: string;           // Natural language, fuzzy is fine
  tags: string[];         // Interest signals (4-5 tags)
  tag_hints?: Record<string, string>;  // Optional descriptions
  budget?: number;        // Max nodes to visit (default: 20)
}
```

### What Gemma emits per sublink

```typescript
interface Rubric {
  // Integer rankings (1-5)
  relevance: number;      // How on-topic for the query
  risk: number;           // How likely to break/matter if changed
  complexity: number;     // How much context needed to understand
  confidence: number;     // How sure about this rating

  // Tag matches
  tags: string[];         // Which of caller's tags apply
}
```

### Example

**Query:**
```json
{
  "text": "LLMKind - adding a fourth variant",
  "tags": ["exhaustive", "pattern-match", "type-family", "breaks-on-add"],
  "budget": 15
}
```

**Gemma rates a sublink:**
```
Link: Edges.hs:89 - "type NodeHandler :: LLMKind -> Type -> Type"
```

**Rubric output:**
```json
{
  "relevance": 4,
  "risk": 5,
  "complexity": 2,
  "confidence": 5,
  "tags": ["type-family", "exhaustive", "breaks-on-add"]
}
```

---

## Expansion Logic (Pure, Testable)

```rust
fn should_expand(
    rubric: &Rubric,
    query_tags: &[String],
    depth: u32,
    breadth: u32,
    remaining_budget: u32,
) -> bool {
    if remaining_budget == 0 { return false; }

    let tag_matches = rubric.tags.iter()
        .filter(|t| query_tags.contains(t))
        .count();

    let score =
        (rubric.relevance as f32) * 2.0
      + (rubric.risk as f32) * 1.5
      + (tag_matches as f32) * 3.0
      + (rubric.confidence as f32) * 0.5
      - (depth as f32) * 2.0
      - (rubric.complexity as f32) * 0.5
      - if breadth > 5 { 3.0 } else { 0.0 };

    score > THRESHOLD  // Tunable, start with 8.0
}
```

**Coefficient rationale:**
- `relevance * 2.0` - Most important signal
- `risk * 1.5` - High risk = must check even if tangential
- `tag_matches * 3.0` - Caller knows what they want
- `confidence * 0.5` - Slight boost for certain ratings
- `depth * 2.0` - Penalize deep dives
- `complexity * 0.5` - Slight penalty for rabbit holes
- `breadth > 5 ? 3.0` - Don't explore everything at wide nodes

---

## Exploration Loop

```
1. PARSE
   - Extract text, tags from query
   - Set budget (default 20)

2. SEED
   - LSP: workspaceSymbol(query.text)
   - Get initial candidate nodes

3. LOOP (while frontier not empty && budget > 0)

   a. POP node from frontier

   b. FETCH subnodes via LSP:
      - findReferences(node)
      - incomingCalls(node)
      - outgoingCalls(node)
      - (dedupe)

   c. RATE: Gemma rates each subnode → Rubric[]
      (batch call for efficiency)

   d. FILTER: for each (subnode, rubric):
      if should_expand(rubric, tags, depth, breadth, budget):
          frontier.push(subnode)
          budget -= 1
      visited.push((subnode, rubric))

   e. depth += 1

4. SYNTHESIZE
   - Gemma: given query + visited nodes with rubrics
   - Produce markdown summary + structured pointers
```

---

## MCP Interface

### Single tool, simple input

```typescript
{
  name: "scout",
  description: "Explore codebase to answer a question. Returns summary + pointers.",
  inputSchema: {
    type: "object",
    properties: {
      query: {
        type: "string",
        description: "What you want to know (natural language)"
      },
      tags: {
        type: "array",
        items: { type: "string" },
        description: "Interest signals (e.g., 'exhaustive', 'pattern-match')"
      },
      budget: {
        type: "integer",
        default: 20,
        description: "Max nodes to explore"
      }
    },
    required: ["query"]
  }
}
```

### Response shape

```typescript
interface ScoutResponse {
  // Main answer
  summary: string;  // Markdown, 1-3 paragraphs

  // Actionable pointers
  pointers: Array<{
    location: string;      // "Edges.hs:89"
    what: string;          // "NodeHandler type family"
    action?: string;       // "add branch for new variant"
    risk: "high" | "medium" | "low";
  }>;

  // Exploration metadata
  meta: {
    nodes_visited: number;
    budget_remaining: number;
    interpretation: string;  // "Matched LLMKind data type at Types.hs:47"
  };
}
```

---

## FunctionGemma Prompts

### Rating prompt (called per-batch)

```
Rate each link for relevance to the query.

Query: "{query_text}"
Interest tags: {tags}

Links to rate:
1. {location_1} - {hover_info_1}
2. {location_2} - {hover_info_2}
...

For each link, output JSON:
{"relevance": 1-5, "risk": 1-5, "complexity": 1-5, "confidence": 1-5, "tags": [...]}

Only include tags from the provided list that actually apply.
```

### Synthesis prompt (called once at end)

```
Summarize findings for: "{query_text}"

Visited nodes (with rubrics):
{visited_nodes_formatted}

Write:
1. One-paragraph summary answering the query
2. Bullet list of high-risk locations with actions
3. What was safely ignored and why
```

---

## Implementation Sketch

### Directory structure

```
semantic-scout/
├── Cargo.toml
├── src/
│   ├── main.rs           # MCP server entry
│   ├── mcp.rs            # JSON-RPC handling
│   ├── gemma.rs          # FunctionGemma HTTP client
│   ├── lsp.rs            # LSP client (HLS)
│   ├── rubric.rs         # Rubric types + expansion logic
│   ├── explore.rs        # Main exploration loop
│   └── prompts.rs        # Gemma prompt templates
└── prompts/
    ├── rate.txt          # Rating prompt template
    └── synthesize.txt    # Synthesis prompt template
```

### Core types

```rust
// rubric.rs
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Rubric {
    pub relevance: u8,    // 1-5
    pub risk: u8,         // 1-5
    pub complexity: u8,   // 1-5
    pub confidence: u8,   // 1-5
    pub tags: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct Query {
    pub text: String,
    pub tags: Vec<String>,
    pub budget: u32,
}

#[derive(Debug, Clone)]
pub struct VisitedNode {
    pub location: String,
    pub hover: String,
    pub rubric: Rubric,
    pub depth: u32,
}
```

### Main loop

```rust
// explore.rs
pub async fn explore(
    query: &Query,
    gemma: &GemmaClient,
    lsp: &LspClient,
) -> ScoutResponse {
    let mut visited: Vec<VisitedNode> = vec![];
    let mut frontier: VecDeque<LspLocation> = VecDeque::new();
    let mut budget = query.budget;
    let mut depth = 0;

    // Seed from workspace symbol search
    let roots = lsp.workspace_symbol(&query.text).await?;
    frontier.extend(roots);

    while let Some(node) = frontier.pop_front() {
        if budget == 0 { break; }

        // Get subnodes
        let subnodes = lsp.get_subnodes(&node).await?;
        let breadth = subnodes.len() as u32;

        // Batch rate with Gemma
        let rubrics = gemma.rate_batch(&subnodes, query).await?;

        for (subnode, rubric) in subnodes.into_iter().zip(rubrics) {
            // Record visit
            visited.push(VisitedNode {
                location: subnode.to_string(),
                hover: lsp.hover(&subnode).await?,
                rubric: rubric.clone(),
                depth,
            });

            // Maybe expand
            if should_expand(&rubric, &query.tags, depth, breadth, budget) {
                frontier.push_back(subnode);
                budget -= 1;
            }
        }

        depth += 1;
    }

    // Synthesize
    gemma.synthesize(query, &visited).await
}
```

---

## Iteration Path

### V0: Proof of concept
- [ ] Hardcoded LSP (just workspaceSymbol + findReferences)
- [ ] Mock Gemma (always returns relevance=3, risk=3)
- [ ] Fixed expansion (depth < 2 && breadth < 10)
- [ ] No synthesis (just return visited list)

### V1: Real Gemma
- [ ] HTTP client to localhost FunctionGemma
- [ ] Rating prompt
- [ ] Synthesis prompt
- [ ] Tunable coefficients via config

### V2: Full LSP
- [ ] incomingCalls / outgoingCalls
- [ ] hover for context
- [ ] documentSymbol for file-level queries

### V3: Polish
- [ ] Streaming response (emit pointers as found)
- [ ] Caching (don't re-rate same node)
- [ ] Confidence intervals on rubric
- [ ] Query history / refinement

---

## Why FunctionGemma

FunctionGemma is hyperoptimized for structured output:
- Tiny (runs on phone)
- Fast (< 100ms per rating batch)
- Reliable (trained specifically for function calling / JSON output)
- Cheap (local, no API costs)

It's not doing creative reasoning - it's doing structured classification. That's exactly what it's good at.

The "intelligence" is in:
1. The rubric design (what dimensions matter)
2. The coefficient tuning (how to weight them)
3. The tag vocabulary (what the caller can express)

All of which are iterable without retraining.

---

## Success Criteria

- [ ] Can answer "what breaks if I change X" in < 5 seconds
- [ ] Visits < 30 nodes for typical query (budget efficient)
- [ ] Correctly identifies exhaustive pattern matches
- [ ] Correctly skips imports / re-exports
- [ ] Summary is actionable (locations + actions)
- [ ] Works with HLS on Haskell codebases
- [ ] Extensible to other LSP servers (rust-analyzer, etc.)

---

## Open Questions

1. **Batch size for Gemma**: Rate 5 at a time? 10? All at once?
2. **LSP connection**: Persistent or spawn per query?
3. **Tag vocabulary**: Fixed set or freeform?
4. **Coefficient tuning**: Manual or learned from feedback?
5. **Caching**: Per-session or persistent?

---

## Related Work Items

- `01-graphnode-marker.md` - GraphNode type (if we want to define this as a Tidepool graph)
- `05-mcp-reify.md` - MCP tool generation from graph entries
- Plan: `~/.claude/plans/tranquil-stargazing-scott.md` - Full DSL context
