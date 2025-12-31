# MERMAID + TEMPLATE TREE GENERATION

Create a module that generates documentation from graph definitions and jinja templates.

## Primary Deliverables

1. **Tidepool.Graph.Mermaid** module in tidepool-core:
   - Function: graphToMermaid :: (Generic (graph AsGraph), ...) => Proxy graph -> Text
   - Generates mermaid flowchart from graph structure
   - Entry nodes: special shape, Exits: special shape, Logic nodes: rectangles
   - Edges derived from UsesEffects constraints (Goto targets)

2. **Tidepool.Template.DependencyTree** module:
   - Parse jinja templates for {% include %} and {% extends %} directives
   - Generate mermaid diagram of template dependencies
   - Function: templateTreeToMermaid :: FilePath -> IO Text

3. **Test suite** with known-good outputs:
   - TestGraph -> expected mermaid string (hardcoded, verified)
   - Sample template tree -> expected mermaid string
   - Tests fail if output changes (golden tests)

## Technical Notes

- Use freer-effects, NOT effectful (migration complete)
- Graph structure available via GHC.Generics on AsGraph mode
- Look at tidepool-core/src/Tidepool/Graph/Types.hs for type families
- Templates are in templates/ directories

## Holistic Improvements (do these as you go!)

- Make types correct-by-construction where possible
- Update CLAUDE.md files if you add new modules/capabilities
- Fix any stale documentation you encounter
- Add type signatures everywhere
- If you see opportunities to improve existing code, do it

Build must pass: cabal build all && cabal test all
File a PR when done.
