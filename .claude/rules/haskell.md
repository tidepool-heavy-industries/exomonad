---
paths:
  - "**/*.hs"
---

# Applied Haskell

## Core Philosophy

**Parse, don't validate.** Transform unstructured input into structured types at boundaries. Interior code operates on already-parsed types and never re-checks.

**Make illegal states unrepresentable.** Structure types so invalid states cannot be constructed. Sum types with data per branch, newtypes for validated data, smart constructors with opaque exports.

## Type Design

- Newtype domain concepts aggressively (`UserId`, `Email`, `SourcePath` vs `DestPath`)
- Sum types carry data specific to each state - pattern matching provides proof
- Export type but not constructor; validation lives in `mk-` smart constructor
- Constructive safety (invariant in type structure) over extrinsic (module discipline)

## Applicative Over Monad

Static dependencies = Applicative (parallelizable, all errors accumulate).
Dynamic dependencies = Monad (sequential, short-circuit).

```haskell
-- Applicative: all validations run
validateUser form = User
  <$> validateName form.name
  <*> validateEmail form.email

-- Monad: short-circuit on first error
validateUser form = do
  name <- validateName form.name
  email <- validateEmail form.email
  pure $ User name email
```

## Module Organization

- Organize by domain feature, not code type
- Split at ~300-500 lines or when import cycles emerge
- `.Internal` modules for escape hatches (testing, debugging)
- Types colocate with their operations

## Error Handling

Recoverable errors as data; programmer errors as exceptions.

```haskell
data ProcessingError
  = InputError FilePath ValidationError
  | TransformError Stage TransformError
```

Error wrapping adds context as errors propagate up.

## Naming

- `mk-` smart constructor, `un-` unwrapper, `run-` effect runner
- Types are nouns describing the space (`Document`, `Request`)
- Constructors are states/variants (`Pending`, `Validated`)

## Complexity Budget

Advanced features (GADTs, type families, DataKinds) earn their place in DSL/library internals to simplify user code. Bread-and-butter techniques first.
