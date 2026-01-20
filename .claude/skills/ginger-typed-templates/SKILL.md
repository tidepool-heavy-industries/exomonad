---
name: ginger-typed-templates
description: Use when working with ginger/Jinja templates that use compile-time type validation, especially when dealing with Maybe fields or conditional access patterns.
---

# Ginger Typed Templates

The vendored ginger library (`haskell/vendor/ginger/`) provides compile-time template validation via Template Haskell.

## Typed Template Basics

```haskell
-- Phase 1: Define context type (must be in separate module for TH staging)
data MyContext = MyContext
  { user :: Maybe UserInfo
  , items :: [Item]
  } deriving (Generic)

instance ToGVal (...) MyContext where ...

-- Phase 2: Compile template (TH validates field access)
myTemplate :: TypedTemplate MyContext SourcePos
myTemplate = $(typedTemplateFile ''MyContext "templates/my.jinja")
```

## Maybe Fields and Narrowing

**The typed validator supports narrowing via `is defined` checks.**

### Problem: Direct Maybe Access Fails

```jinja
{# FAILS: validator sees user :: Maybe UserInfo, can't find .name on Maybe #}
{% if user %}
  Hello, {{ user.name }}!
{% endif %}
```

Error:
```
error: field 'name' not found
  --> templates/my.jinja:3:12
   |
 3 |   Hello, {{ user.name }}!
   |            ^
   = in access: user.name
```

### Solution: Use `is defined` for Narrowing

```jinja
{# WORKS: validator narrows user to UserInfo inside the block #}
{% if user is defined %}
  Hello, {{ user.name }}!
{% endif %}
```

The validator tracks `is defined` guards and narrows the type within the true branch.

## Narrowing Rules

From `haskell/vendor/ginger/src/Text/Ginger/TH/Extract.hs`:

| Condition | True Branch Narrowing | False Branch Narrowing |
|-----------|----------------------|------------------------|
| `x is defined` | `x` narrowed | nothing |
| `x is undefined` | nothing | `x` narrowed |
| `a and b` | both narrowed | nothing (conservative) |
| `a or b` | nothing (conservative) | both narrowed |
| `not expr` | swapped | swapped |

## When to Use Each Pattern

| Field Type | Check | Purpose |
|------------|-------|---------|
| `Maybe a` | `{% if field is defined %}` | Narrow to inner type for field access |
| `[a]` | `{% if field %}` | Check non-empty list (truthiness) |
| `Bool` | `{% if field %}` | Check boolean value |
| `Maybe a` (inner truthiness) | `{% if field %}` | Check if Just and truthy value |

## Nested Access After Narrowing

Once narrowed, nested fields work normally:

```jinja
{% if user is defined %}
  {# user is narrowed to UserInfo, so user.profile is accessible #}
  {% if user.profile is defined %}
    {# user.profile is narrowed, so user.profile.avatar works #}
    <img src="{{ user.profile.avatar }}">
  {% endif %}
{% endif %}
```

## Common Patterns

### Optional Section with List

```jinja
{% if items is defined %}
  {% if items %}  {# Check non-empty #}
    <ul>
    {% for item in items %}
      <li>{{ item.name }}</li>
    {% endfor %}
    </ul>
  {% endif %}
{% endif %}
```

### Optional Field in Table

```jinja
| Field | Value |
|-------|-------|
{% if owner is defined %}| Owner | {{ owner }} |{% endif %}
| Status | {{ status }} |
```

## Critical: ToGVal Keys Must Match Field Names

**The TH validates against Haskell record field names, NOT ToGVal dictionary keys.**

```haskell
-- Haskell field (type_ because 'type' is reserved)
data BeadContext = BeadContext
  { type_ :: Text
  , ...
  }

-- ToGVal MUST use same name as field for TH validation
instance ToGVal (...) BeadContext where
  toGVal bc = dict
    [ "type_" ~> bc.type_  -- ✓ Matches field name
    -- NOT: "type" ~> bc.type_  -- ✗ TH will fail on {{ bead.type }}
    ]

-- Template uses the field name
{{ bead.type_ }}  -- ✓ Matches both field and ToGVal key
```

**Why?** TH inspects the Haskell record structure at compile time. Runtime uses ToGVal. They must agree on names.

## Debugging Tips

1. **Check narrowing context**: Validator errors show the access path and position
2. **Add `is defined` incrementally**: Start with outermost Maybe, work inward
3. **Lists don't need narrowing**: `[a]` fields are always defined (may be empty)
4. **Ternary also narrows**: `{{ user.name if user is defined else "Anonymous" }}`
5. **ToGVal keys must match field names**: Reserved word workarounds (like `type_`) propagate to templates

## Source Files

- `haskell/vendor/ginger/src/Text/Ginger/TH/Extract.hs` - Narrowing logic
- `haskell/vendor/ginger/src/Text/Ginger/TH/Types.hs` - `NarrowedPath` types
- `haskell/dsl/core/src/Tidepool/Graph/Template.hs` - `typedTemplateFile` export
