# CLI Parser Research: optparse-generic vs Alternatives

> **STATUS: RESEARCH / REFERENCE**
> This document represents early research. Production implementations may differ.
> See `haskell/dsl/core/CLAUDE.md` for current graph input handling.

Research task for evaluating CLI parser derivation approaches for exomonad graph inputs.

## Executive Summary

**Recommendation: Use optparse-applicative (manual) for production CLIs.**

optparse-generic is excellent for rapid prototyping but has critical limitations:
- Cannot handle nested records (like `GraphInput` with `ImageSource`)
- Limited control over help text and short flags
- No customization of subcommand names

For exomonad's graph input types, a hybrid approach works best:
1. Define a "flat" CLI type that maps to the nested domain type
2. Use optparse-generic for the flat type
3. Convert to domain type in `main`

## optparse-generic Findings

### What Works Well

| Feature | Status | Example |
|---------|--------|---------|
| Sum types → subcommands | Works | `data Cmd = A {...} \| B {...}` → `a ...` / `b ...` |
| Record fields → flags | Works | `{ port :: Int }` → `--port INT` |
| Maybe fields → optional | Works | `Maybe Text` → `[--field TEXT]` |
| Bool fields → switches | Works | `Bool` → `--flag` (no argument) |
| Lists → repeatable | Works | `[Int]` → `--item 1 --item 2` |
| Defaults via `<!>` | Works | `Int <!> "8080"` |
| Help text via `<?>` | Works | `Int <?> "Port number"` |

### What Doesn't Work

| Limitation | Error | Workaround |
|------------|-------|------------|
| Nested records | `No instance for ParseFields Inner` | Flatten the type |
| Nested Maybe/List | Same error | Flatten |
| Recursive types | Same error | Not possible |
| Custom subcommand names | Hardcoded lowercase | Use optparse-applicative |
| Short flags (-p) | Not supported | Use `shortNameModifier` |

### Tested Code

```haskell
-- This works:
data Command
  = TextInput { content :: Text }
  | PhotoInput { caption :: Maybe Text, imageUrl :: Text }
  deriving (Show, Generic)

instance ParseRecord Command

-- Generates:
-- Usage: cmd (textinput | photoinput)
-- textinput --content TEXT
-- photoinput [--caption TEXT] --imageUrl TEXT
```

```haskell
-- This FAILS (nested record):
data PhotoInput = PhotoInput
  { caption :: Maybe Text
  , image :: ImageSource  -- ImageSource is a record!
  }
-- Error: No instance for 'ParseFields ImageSource'
```

## Comparison: optparse-generic vs optparse-applicative

| Aspect | optparse-generic | optparse-applicative |
|--------|------------------|---------------------|
| Setup time | Minutes | Hours |
| Type safety | Automatic from Generic | Manual but verified |
| Flexibility | Limited | Full control |
| Short flags | Via modifier | Built-in |
| Nested types | No | Yes (compose parsers) |
| Subcommands | Auto from sum types | Manual with `subparser` |
| Shell completion | Inherited | Full support |
| Maintenance | Types drive CLI | CLI may drift from types |

### When to Use Each

**optparse-generic**:
- Internal tools, scripts
- Prototyping CLI interface
- Simple flat types
- When types naturally match CLI structure

**optparse-applicative**:
- Production user-facing CLIs
- Complex nested types
- Need short flags, custom help
- Shell completion important

## Alternatives Evaluated

### cmdargs

- Uses annotations/deriving (similar philosophy to optparse-generic)
- More concise than optparse-applicative
- Less ecosystem adoption
- HLint uses it successfully

### optparse-simple

- Thin wrapper over optparse-applicative
- Reduces boilerplate for common patterns
- Good for apps with subcommands
- Not a replacement for complex cases

### butcher

- Monadic interface (vs Applicative)
- More flexible binding
- "More evil" (uses unsafePerformIO internally)
- Less widely used

## Recommendation for ExoMonad

### For GraphInput CLI

The `GraphInput` type has nested `ImageSource`, so optparse-generic won't work directly.

**Option 1: Flat CLI type (Recommended)**

```haskell
-- CLI-specific flat type
data GraphInputCLI
  = TextInputCLI { text :: Text }
  | PhotoUrlCLI { caption :: Maybe Text, url :: Text }
  | PhotoFileCLI { caption :: Maybe Text, file :: FilePath }
  deriving (Show, Generic)

instance ParseRecord GraphInputCLI

-- Convert to domain type
toGraphInput :: GraphInputCLI -> IO GraphInput
toGraphInput (TextInputCLI t) = pure $ TextInput t
toGraphInput (PhotoUrlCLI c u) = pure $ PhotoInput c (UrlImage u)
toGraphInput (PhotoFileCLI c f) = do
  contents <- B.readFile f
  pure $ PhotoInput c (Base64Image "image/jpeg" (encodeBase64 contents))
```

**Option 2: Manual optparse-applicative**

```haskell
graphInputParser :: Parser GraphInput
graphInputParser = subparser
  ( command "text" (info textParser (progDesc "Text input"))
 <> command "photo" (info photoParser (progDesc "Photo input"))
  )
  where
    textParser = TextInput <$> strOption (long "content" <> metavar "TEXT")
    photoParser = PhotoInput
      <$> optional (strOption (long "caption"))
      <*> imageSourceParser

    imageSourceParser = urlParser <|> base64Parser
    urlParser = UrlImage <$> strOption (long "url" <> metavar "URL")
    base64Parser = Base64Image
      <$> strOption (long "media-type" <> value "image/jpeg")
      <*> strOption (long "data" <> metavar "BASE64")
```

### For Future CLIs

Add to CLAUDE.md:

```markdown
## CLI Patterns

For simple flat types, use optparse-generic:
- Derive Generic
- instance ParseRecord MyType
- Use `getRecord "description"` in main

For nested types or production CLIs, use optparse-applicative:
- Define Parser manually with combinators
- Compose with <$>, <*>, <|>
- Use subparser for subcommands
```

## Test Artifacts

Test code is in `/tmp/optparse-test/`:
- `Main.hs` - Working sum type examples
- `NestedTest.hs` - Demonstrates nested record failure
- `optparse-test.cabal` - Build configuration

## Sources

- [optparse-generic Hackage](https://hackage.haskell.org/package/optparse-generic)
- [optparse-applicative Hackage](https://hackage.haskell.org/package/optparse-applicative)
- [Gabriel Gonzalez's blog post on optparse-generic](https://www.haskellforall.com/2016/02/auto-generate-command-line-interface.html)
- [HaskellWiki CLI parsers comparison](https://wiki.haskell.org/Command_line_option_parsers)
- [cmdargs GitHub](https://github.com/ndmitchell/cmdargs)
