# Learning Experiment 1: Teaching ExoMonad via Real Use Case

**Date:** 2026-01-13
**User:** Inanna (library author, approaching as learner)
**Task:** Build Habitica task router from scratch using template
**Output Style:** Learning mode → Technical mode (mid-session switch)
**Session Length:** ~2 hours

---

## Experiment Setup

**Goal:** Test if teaching ExoMonad by building a real agent is more effective than reading docs.

**Approach:**
1. Started with empty template repo (`human-driven-dev`)
2. User had real use case: "Help me capture tasks and route them to Habitica"
3. Guided via "Learn by Doing" prompts - give context, set task, provide guidance
4. User writes code, I course-correct and explain

**Why this matters:**
- ExoMonad has complex type-level machinery
- Docs exist but are reference-heavy
- Need to validate if library is actually learnable

---

## What Worked Well

### 1. Real Use Case Motivation
Starting with "I want to route tasks to Habitica" gave clear goal.
- User immediately understood exit types (routing decisions)
- Could evaluate design choices ("Do I need this constructor?")
- Natural questions emerged ("How does the LLM see this?")

**Contrast with:** "Here's how graphs work" → abstract, hard to retain.

### 2. Template Repo as Starting Point
Pre-built skeleton (`MyTask` placeholder) worked well:
- User could see full structure before understanding it
- Compilation errors taught type system gradually
- "Replace MyTask with your domain" was concrete first step

**But:** Template had too much (ClaudeCode, session management) for simple use case.

### 3. Type-First Design Flow
Teaching order: Exit types → Template → Handlers
- Exit type = routing logic (what LLM decides)
- Template = asking LLM to decide
- Handlers = executing decision

This clicked! User understood exit type is the critical design choice.

**Quote:** "ok so the llm output, right - does it generate tool definitions? how?"
→ Led to realization that types ARE the tool definitions

### 4. Insight Boxes
The `★ Insight ─────` boxes landed well:
- Broke up walls of text
- Highlighted key concepts
- User referenced them later ("like you said about input vs context")

**Most impactful insight:** Documentation comments → LLM tool descriptions
User: "yeah! so in that one context the comments aren't just communications to other devs, but to the LLM inside the node itself? cool!"

---

## What Didn't Work / Got Confusing

### 1. Input vs Context Distinction
**Problem:** User expected `TaskRouterInput` to flow directly to template.

**What happened:**
- Defined input as just `{ userMessage :: Text }`
- Asked: "I want to load Habitica state to assist in routing"
- Confused about where that loading happens

**Root cause:** "Input" and "Template Context" sound like the same thing, but they're different concepts.

**Fix needed:**
- Better naming? `NodeInput` vs `TemplateContext`
- Explicit dataflow diagram in docs
- Before-handler explanation earlier

### 2. Session Management Surprise
Template included ClaudeCode + session continuation.
User use case didn't need it → added confusion.

**Quote:** "btw simplifying things: this will just be a NON claude code node"

**Learning:** Template should have TWO variants:
- Simple LLM (Anthropic API, no session management)
- ClaudeCode (subprocess, session continuation, worktrees)

Most users want the first one!

### 3. Effect System Abstraction Leak
"Where do I load Habitica state?" led to explaining:
- Before-handler pattern
- Effect composition
- Memory vs context

This was a LOT. User got it, but steep ramp.

**Pattern observed:**
1. User defines types (easy, familiar)
2. User writes template (easy, Jinja)
3. User hits handlers → "wait where does data come from?"

Handler before/after is where type-level meets value-level. That's the chasm.

### 4. Deriving Boilerplate
Every type needed:
```haskell
deriving stock (Show, Eq, Generic)
deriving anyclass (FromJSON, ToJSON)
```

Multiple compile errors from forgetting instances.

**Learning:** `deriving ExoMonad` is NECESSARY, not nice-to-have.

### 5. UsesEffects Redundancy
User defined graph with `UsesEffects '[Goto ...]` then wrote handler using those same effects.

Clear duplication. Would notice on refactoring.

**Learning:** Effect inference would be high value.

---

## Key Breakthrough Moments

### Moment 1: Exit Type Design
User initially had vague idea ("route to Habitica").
After discussion, crystallized to:
```haskell
data TaskRouterExit
  = MoreDataNeeded { questions :: GUITree }
  | HabiticaTodo { tasks :: [Text], events :: [Text] }
```

This was THE design decision. Everything else followed.

**Insight:** Exit type design is where most thinking happens. Templates/handlers are mechanical after that.

### Moment 2: Comments → Tool Descriptions
User realized Haddock comments flow to LLM:
"yeah! so in that one context the comments aren't just communications to other devs, but to the LLM inside the node itself? cool!"

Changed how they thought about documentation. Started writing comments FOR the LLM.

### Moment 3: AskUserQuestion vs MoreDataNeeded
Discussion of two patterns for user questions:
- Hardcoded questions in handler (AskUserQuestion effect)
- LLM-generated questions (MoreDataNeeded exit + GUI)

Led to: "I could combine both!" Pattern emerged naturally.

### Moment 4: Switching to Technical Mode
User explicitly switched output styles:
"ok, reverting your style away from 'learning mode' - remember that we're _building_ this ecosystem"

Then asked for improvement ideas → Generated 9 concrete proposals.

**Learning:** Teaching mode works for onboarding, expert users want direct answers.

---

## Teaching Patterns That Worked

### 1. Start with "Why This Matters"
Every concept introduced with motivation:
- "Exit types define routing" → Why? LLM picks one, handler dispatches
- "Context separate from input" → Why? Load data via effects

User rarely asked "why do I need this?" - preempted.

### 2. Show Anti-Pattern First
"Current state: boilerplate everywhere"
"Proposed: single deriving clause"

User immediately saw pain point → bought into solution.

### 3. Trace Data Flow Explicitly
"Input → Before Handler → Context → Template → LLM → Exit → After Handler → Goto"

Drew ASCII diagrams multiple times. User referenced them later.

### 4. Acknowledge Complexity
"This is a LOT" when explaining before-handlers + effects + memory.

User appreciated honesty → didn't feel stupid for being confused.

### 5. Defer Advanced Topics
Prevented scope creep. User trusted we'd cover things later.

---

## Unexpected Discoveries

### 1. Template Was Too Complex
Pre-built template had:
- Session management (ClaudeCode)
- Memory effect
- Self-loops
- Retry logic

For a first agent: WAY too much.

**Better template:** Entry → LLM → Exit. That's it.

### 2. Graph Visualization Missing
User never saw what the graph LOOKS like.
Mermaid exists but not in development flow.

**Fix:** Auto-generate Mermaid on compile? Or `just graph-viz` command?

### 3. Error Messages Were Good!
Type errors were actually helpful:
- "Module X does not export Y" → clear fix
- "Unknown transition target" → pointed to exact issue

**Learning:** Type-level error message investment paid off.

### 4. Documentation Comments Matter
Once user realized comments → LLM tool descriptions, they started writing thoughtful comments.

This is a FEATURE. Makes code self-documenting.

---

## Improvement Ideas Generated

User asked for "long lists of small granular improvements" → Generated 9.

**Top 3 by user interest:**
1. Effect inference from handlers
2. `deriving ExoMonad` single clause
3. Memory as StateT

These map to pain points encountered:
- Effect duplication (UsesEffects vs handler signature)
- Deriving boilerplate (forgot instances → errors)
- Memory API (imperative get/update style)

**Meta-insight:** Teaching surfaces UX issues you don't notice when you already know the system.

---

## Learning Style Impact

### Learning Mode (First Half)
- Structured "Learn by Doing" prompts
- Insight boxes with key concepts
- Questions to user: "Think about..."

**User response:** Engaged, asking questions, thinking through design.

### Technical Mode (Second Half)
- Direct answers, less pedagogical framing
- Lists of concrete proposals

**User response:** Asked for this explicitly - wanted expert-level discussion.

**Takeaway:** Need both modes. Learning for onboarding, Technical for collaboration.

---

## What Would I Do Differently

### 1. Start Even Simpler
Template should be MINIMAL:
```
Entry → SingleLLMNode → Exit
```

No self-loops, memory, retry logic. Just: user message → LLM → decision → done.

### 2. Visual Dataflow Diagram First
Before any code, show:
```
User Input
    ↓
Before Handler (loads context)
    ↓
Template (sees context)
    ↓
LLM (makes decision)
    ↓
After Handler (routes based on decision)
    ↓
Next Node or Exit
```

Would prevent "Input vs Context" confusion.

### 3. More Emphasis on Exit Type Design
This is THE critical step. Should have:
- Spent MORE time here
- Shown 3-4 examples of good exit types
- Had user critique bad designs

Everything flows from exit types. Get this right, rest is mechanical.

---

## Success Criteria

**Goal:** User can build a working agent from template.

**Status:** Partially successful.
- ✅ User understood exit type design
- ✅ User could write template
- ✅ User understood handler routing
- ❌ User never actually RAN the agent
- ❌ User didn't test handlers
- ⚠️ User needed help with imports/types

**Biggest gap:** From "compiles" to "works" is unclear.

---

## Concrete Next Steps

### For Teaching Materials
1. Create two template variants: Simple and Advanced
2. Add visual dataflow diagram to README
3. Write "Testing Your Agent" guide
4. Create gallery of exit type examples

### For Library UX
1. Implement `deriving ExoMonad` (high priority)
2. Add Memory StateT API (high priority)
3. Improve graph visualization
4. Add handler testing utilities

### For Documentation
1. Exit type design guide (patterns and anti-patterns)
2. Input vs Context explanation with diagrams
3. Effect composition guide
4. End-to-end tutorial: idea → running agent

---

## Meta: Teaching as User Research

This wasn't just teaching - it was user research.

**What I learned:**
- Where users get stuck (input vs context, before-handlers)
- What they find intuitive (exit type → routing)
- What's too complex (session management for simple cases)
- What error messages work (type-level validation)
- What's missing (testing, visualization, simpler templates)

**Method:**
- Watch them write code
- Note every question
- Track compile errors
- Observe "aha!" moments
- Ask for improvement ideas

**Result:** 9 concrete improvement proposals, all grounded in actual pain points.

---

## Surprising Quotes

"yeah! so in that one context the comments aren't just communications to other devs, but to the LLM inside the node itself? cool!"
→ Documentation-as-runtime-behavior clicked

"is task router input the input to the llm directly?"
→ Key confusion: input vs context

"btw simplifying things: this will just be a NON claude code node"
→ Template was too complex

"ok, reverting your style away from 'learning mode'"
→ Meta-awareness of teaching style

---

## Conclusion

**What worked:**
- Real use case drove learning
- Exit type design as cornerstone
- Type-level error messages
- Comments → tool descriptions realization

**What needs work:**
- Simpler starting template
- Visual dataflow diagrams
- Input vs context distinction
- Testing/execution story

**Biggest insight:**
Teaching reveals UX problems. Exit type design is the critical step - everything else is mechanical once that's right.

**Next experiment:**
Try with a user who didn't build the library. See where they get stuck. Bet: before-handlers will be the wall.
