# Type Error Philosophy

Use when: Writing compile-time errors in TH or type families that LLMs will encounter.

## Core Insight

**Type errors are prompts for LLMs.** When an LLM reads compiler output, the error message becomes its primary source of information for fixing the issue. Well-structured errors enable autonomous fixes; vague errors cause hallucination and wasted iterations.

## Design for Machine Parsing

LLMs excel at pattern matching. Use consistent, predictable structure:

```
<function>: <what failed>

This happens when:
  1. <cause A>
  2. <cause B>

Fix for cause 1:
  <exact code>

Fix for cause 2:
  <exact code>
```

### Key Structural Elements

1. **Function name prefix**: `deriveJSONSchema:` - tells LLM which function failed
2. **Specific identifiers**: Include the exact field name, type name, module name
3. **Numbered causes**: LLM can systematically check each
4. **Literal fix syntax**: Show copy-pasteable code, not descriptions

## Principles

### 1. Explain the Mechanism

LLMs need *why* to avoid similar errors. Include the causal chain:

```
getDoc uses DeclDoc which looks up the field selector function;
without FieldSelectors, that function doesn't exist
```

This lets the LLM understand the dependency: `getDoc` → `DeclDoc` → field selector → `FieldSelectors` extension.

### 2. Enumerate All Causes

When the same symptom has multiple causes, list all with distinct fixes:

```haskell
schemaFieldError typeName fieldName fieldType = unlines
  [ "deriveJSONSchema: Can't get docs for field '" ++ nameBase fieldName ++ "' in '" ++ nameBase typeName ++ "'"
  , ""
  , "This happens when either:"
  , ""
  , "  1. The field is missing a Haddock comment (must use -- ^ format after field)"
  , ""
  , "  2. The type's module doesn't have {-# LANGUAGE FieldSelectors #-}"
  , "     (getDoc uses DeclDoc which looks up the field selector function;"
  , "     without FieldSelectors, that function doesn't exist)"
  , ""
  , "Fix option 1 - Add documentation:"
  , ""
  , "  " ++ nameBase fieldName ++ " :: " ++ prettyType fieldType
  , "    -- ^ " ++ typeHint fieldType
  , ""
  , "Fix option 2 - Enable FieldSelectors in the module defining '" ++ nameBase typeName ++ "':"
  , ""
  , "  {-# LANGUAGE FieldSelectors #-}"
  ]
```

### 3. Include Type-Specific Hints

When types determine valid values, generate contextual hints:

```haskell
typeHint :: Type -> String
typeHint (ConT n)
  | nameBase n == "Int"     = "<meaning> (<min>-<max>)"
  | nameBase n == "Text"    = "<what this text represents>"
  | nameBase n == "Bool"    = "<when true vs false>"
typeHint (AppT (ConT n) _)
  | nameBase n == "Maybe"   = "<when present vs absent>"
```

### 4. Reference Source Locations

Include module names so LLMs know which file to edit:

```
Fix option 2 - Enable FieldSelectors in the module defining 'MyType':
```

## Implementation Mechanics

### Option 1: Template Haskell `fail`

**When to use**: TH splices that validate input before generating code.

**Mechanism**: `fail` in the `Q` monad aborts compilation with your message.

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH

deriveFieldSchema :: Name -> Name -> Int -> VarBangType -> Q Exp
deriveFieldSchema typeName _conName _fieldIdx (fieldName, _, fieldType) = do
  mDoc <- getDoc (DeclDoc fieldName)
  case mDoc of
    Just doc -> pure (T.pack doc)
    Nothing  -> fail $ schemaFieldError typeName fieldName fieldType

-- Helper to build multi-line error
schemaFieldError :: Name -> Name -> Type -> String
schemaFieldError typeName fieldName fieldType = unlines
  [ "deriveJSONSchema: Can't get docs for '" ++ nameBase fieldName ++ "'"
  , ""
  , "Fix: Add Haddock comment"
  , "  " ++ nameBase fieldName ++ " :: " ++ pprint fieldType
  , "    -- ^ description here"
  ]
```

**Available TH introspection for error messages**:
- `nameBase :: Name -> String` - unqualified name
- `nameModule :: Name -> Maybe String` - module name
- `pprint :: Ppr a => a -> String` - pretty print types
- `reify :: Name -> Q Info` - get type/constructor info
- `getDoc :: DocLoc -> Q (Maybe String)` - get Haddock docs

### Option 2: Type Family `TypeError`

**When to use**: Type-level validation where invalid types should be rejected.

**Required extensions**:
```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}  -- often needed for TypeError
```

**Imports**:
```haskell
import GHC.TypeLits (TypeError, ErrorMessage(..))
```

**ErrorMessage constructors**:
```haskell
data ErrorMessage
  = Text Symbol                    -- Literal text
  | ShowType t                     -- Pretty-print a type
  | ErrorMessage :<>: ErrorMessage -- Horizontal concat
  | ErrorMessage :$$: ErrorMessage -- Vertical concat (newline)
```

**Basic pattern** - error on specific case:
```haskell
type family ValidateNotUnit (t :: Type) :: Constraint where
  ValidateNotUnit () = TypeError
    ('Text "Cannot use () as output type"
     ':$$: 'Text "Use a newtype wrapper instead")
  ValidateNotUnit _ = ()
```

**Conditional error** - check a boolean:
```haskell
type family CheckFlag (flag :: Bool) (t :: Type) :: Constraint where
  CheckFlag 'True t = TypeError
    ('Text "Invalid: " ':<>: 'ShowType t)
  CheckFlag 'False _ = ()
```

**Multi-line with formatting**:
```haskell
type family RejectOneOf (t :: Type) :: Constraint where
  RejectOneOf t = TypeError
    ('Text "Schema error for type: " ':<>: 'ShowType t
     ':$$: 'Text ""
     ':$$: 'Text "Anthropic's structured output does not support 'oneOf'."
     ':$$: 'Text ""
     ':$$: 'Text "Fix options:"
     ':$$: 'Text "  1. Use tagged record: data Choice = Choice { tag :: Tag }"
     ':$$: 'Text "  2. Use Maybe fields: data Out = Out { a :: Maybe A }"
     ':$$: 'Text "  3. Use enum for simple string choices")
```

### Option 3: Constraint Aliases with TypeError

**When to use**: Reusable validation that can be applied as a constraint.

```haskell
-- Define as a constraint alias
type ValidStructuredOutput t = CheckNotMarkedOneOf (IsMarkedOneOf t) t

-- Use in function signatures
myFunction :: ValidStructuredOutput output => Proxy output -> IO ()

-- Or in instance contexts
instance ValidStructuredOutput t => MyClass t where ...
```

### Option 4: Type Family That Returns TypeError

**When to use**: Complex validation logic that builds up an error.

```haskell
type family ValidateNode (node :: Type) :: Constraint where
  ValidateNode node = ValidateNodeImpl node (GetNeeds node) (GetSchema node)

type family ValidateNodeImpl (node :: Type) (needs :: [Type]) (schema :: Maybe Type) :: Constraint where
  ValidateNodeImpl node '[] 'Nothing = TypeError
    ('Text "Node " ':<>: 'ShowType node ':<>: 'Text " has no inputs or outputs"
     ':$$: 'Text "Add :@ Needs '[...] or :@ Schema ...")
  ValidateNodeImpl _ _ _ = ()
```

### Option 5: Closed Type Family with Exhaustive Cases

**When to use**: Validate against a known set of valid options.

```haskell
type family ValidateSchemaType (t :: Type) :: Constraint where
  ValidateSchemaType Int = ()
  ValidateSchemaType Text = ()
  ValidateSchemaType Bool = ()
  ValidateSchemaType [a] = ValidateSchemaType a  -- recurse
  ValidateSchemaType (Maybe a) = ValidateSchemaType a
  ValidateSchemaType other = TypeError
    ('Text "Unsupported schema type: " ':<>: 'ShowType other
     ':$$: 'Text ""
     ':$$: 'Text "Supported types: Int, Text, Bool, [a], Maybe a")
```

### Combining TH and TypeError

TH can generate type-level code that triggers TypeError at use sites:

```haskell
-- TH generates a type family instance
deriveUsesOneOf :: Name -> Q [Dec]
deriveUsesOneOf typeName = do
  -- Generates: type instance IsMarkedOneOf TypeName = 'True
  let markedInst = TySynInstD (TySynEqn Nothing
        (AppT (ConT ''IsMarkedOneOf) (ConT typeName))
        (PromotedT 'True))
  pure [markedInst]

-- Later, when used in structured output context, TypeError fires
type family CheckNotMarkedOneOf (isMarked :: Bool) (t :: Type) :: Constraint where
  CheckNotMarkedOneOf 'True t = TypeError ('Text "oneOf not supported: " ':<>: 'ShowType t)
  CheckNotMarkedOneOf 'False _ = ()
```

### Debugging TypeError

**Problem**: TypeError fires unexpectedly or not at all.

**Debug technique**: Replace TypeError with a marker constraint temporarily:
```haskell
-- Instead of TypeError, use this to see when it would fire
class DebugWouldError (msg :: ErrorMessage)

type family CheckSomething (t :: Type) :: Constraint where
  CheckSomething Bad = DebugWouldError ('Text "Would error here")
  CheckSomething _ = ()
```

Then check if `DebugWouldError` appears in error messages - it shows the type family is being evaluated.

## Anti-Patterns (What Makes LLMs Fail)

| Anti-Pattern | Problem | LLM Behavior |
|--------------|---------|--------------|
| "Invalid type" | No specifics | Hallucinates random fix |
| "Staging error" | Jargon without explanation | Searches docs, may find wrong info |
| Missing field name | Can't locate the issue | Edits wrong field |
| "Add documentation" | No syntax shown | Wrong comment format |
| Single cause assumed | User has different issue | Fix doesn't work, confusion |

## Checklist for New Errors

- [ ] Starts with function/context name
- [ ] Names the specific type/field/module that failed
- [ ] Explains *why* this fails (the mechanism)
- [ ] Lists all possible causes if ambiguous
- [ ] Shows exact fix syntax for each cause
- [ ] Uses consistent formatting (numbered lists, indented code)
