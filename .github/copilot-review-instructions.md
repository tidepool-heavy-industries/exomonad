# Copilot Code Review Instructions

## Language-Specific Guidance

### Haskell (tidepool-core/, tidepool-dm/, tidepool-tidying/)

**Effect System (effectful)**
- Effect constraints should be ordered correctly - effects are interpreted outermost-first
- Agent code must NOT use `IOE` directly (IO-blind architecture)
- Check that `DispatchOf` is set to `'Dynamic` for new effects
- Effect handlers should be in runner code, not agent code

**Tool Definitions**
- Every `Tool` instance needs `deriveJSONSchema` for the input type
- Tool execution should only use effects available in `ToolEffects`
- Look for missing `Proxy @` in `ToolList` definitions

**Delta Fields**
- Mutation types should use deltas (`+2`), not absolute values
- Every delta type needs a `because :: Text` field for explainability
- Check `applyTurnOutput` correctly handles all delta fields

**Templates**
- `typedTemplateFile` requires the context type to match template variables
- Template includes should exist in the templates/ directory
- Jinja syntax should be valid (LLMs know Jinja well)

**Common Mistakes**
- Using `unsafeCoerce` or `Dynamic` (should use `OneOf` pattern instead)
- Missing `INLINE` pragmas (not needed with effectful, flag if present)
- Partial functions (`head`, `tail`, `!!`) without guards

### TypeScript (deploy/)

- Protocol types must match Haskell `Serializable.hs` exactly
- WASM loader uses GHC-specific JSFFI - don't modify without care
- Durable Object state must be serializable

## Review Focus

1. **Type Safety** - This codebase prioritizes compile-time guarantees
2. **Effect Ordering** - Wrong order causes runtime issues
3. **Schema Derivation** - LLM tools need valid JSON schemas
4. **IO-Blindness** - Agents must not escape their effect sandbox

## Less Important

- Style/formatting (hlint handles this)
- Documentation coverage (intentionally minimal)
- Test coverage suggestions (tests are for critical paths only)
