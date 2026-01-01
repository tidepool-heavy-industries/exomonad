---
applyTo: "**/*.hs"
---

# Haskell Review Instructions

This is an effectful-based codebase with specific patterns. Focus on:

## Critical Issues

**Effect System**
- Effect constraints must be ordered correctly (outermost runs first)
- Agent code must NOT use `IOE` - this is IO-blind architecture
- New effects need `type instance DispatchOf MyEffect = 'Dynamic`
- Effect handlers belong in runner code, not agent code

**Type Safety**
- No `unsafeCoerce` or `Dynamic` - use `OneOf` pattern instead
- No partial functions (`head`, `tail`, `!!`) without guards
- Tool input types need `deriveJSONSchema`

**Delta Pattern**
- Mutations use deltas (`stressDelta = +2`), not absolute values
- Every delta type must have `because :: Text` field

## Less Important

- INLINE pragmas (effectful doesn't need them)
- Missing documentation (intentionally minimal)
- Stylistic issues (hlint handles this)

## Common Patterns

```haskell
-- Effect definition pattern
data MyEffect :: Effect where
  DoThing :: Arg -> MyEffect m Result

type instance DispatchOf MyEffect = 'Dynamic

-- Constraint pattern
myFunction
  :: (State s :> es, Emit e :> es)
  => Eff es ()

-- Tool list pattern
type MyTools = TCons (Proxy @Tool1) $ TCons (Proxy @Tool2) $ TNil
```
