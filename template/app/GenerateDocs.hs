-- | Generate rich documentation from the graph type and templates.
--
-- This generates documentation optimized for LLM consumption, including:
-- - Graph structure (Mermaid diagram)
-- - Node-by-node documentation with types
--
-- Run with: cabal run generate-template-docs
module Main where

import Data.Proxy (Proxy(..))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import System.Directory (createDirectoryIfMissing, doesFileExist)

import Data.Aeson.Encode.Pretty (encodePretty)
import Tidepool.Graph.Mermaid (graphToMermaid, toSequenceDiagram, ExecutionPath(..), defaultConfig)
import Tidepool.Graph.Reify (GraphInfo(..), NodeInfo(..), RuntimeNodeKind(..), makeGraphInfo)
import Tidepool.Template.DependencyTree (templateTreeToMermaid)
import Tidepool.Schema (schemaToValue, JSONSchema(..), SchemaType(..))
import qualified Data.Map.Strict as Map
import Data.Typeable (TypeRep)

import Template.Graph (SimpleGraph, inputSchema, outputSchema, resultSchema)

main :: IO ()
main = do
  createDirectoryIfMissing True "docs"

  -- Get reified graph info for node documentation
  let graphInfo = makeGraphInfo (Proxy @SimpleGraph)

  -- Generate graph diagram
  let graphDiagram = graphToMermaid (Proxy @SimpleGraph)

  -- Generate template documentation
  hasTemplates <- doesFileExist "templates/process.jinja"
  templateDiagram <- if hasTemplates
    then templateTreeToMermaid "templates/process.jinja"
    else pure ""

  let content = T.unlines $
        [ "# Agent Documentation"
        , ""
        , "Generated automatically from types and templates."
        , ""
        , "## Graph Overview"
        , ""
        , "```mermaid"
        , graphDiagram
        , "```"
        , ""
        , "## Node Reference"
        , ""
        ]
        ++ nodeDocumentation graphInfo
        ++ [ ""
           , "## Data Flow"
           , ""
           , "| From | To | Type |"
           , "|------|-----|------|"
           ]
        ++ edgeDocumentation graphInfo
        ++ if T.null templateDiagram then [] else
        [ ""
        , "## Templates"
        , ""
        , "See [`templates/process.jinja`](../templates/process.jinja) for the main prompt template."
        , ""
        , "### Dependencies"
        , ""
        , "```mermaid"
        , templateDiagram
        , "```"
        ]
        ++ typeDefinitionsSection
        ++ jsonSchemaSection
        ++ contextMappingSection
        ++ executionTraceSection graphInfo
        ++ validationRulesSection
        ++ availableEffectsSection
        ++ handlerReferenceSection

  TIO.writeFile "docs/graph.md" content
  putStrLn "Generated docs/graph.md"

-- | Generate documentation for each node.
nodeDocumentation :: GraphInfo -> [T.Text]
nodeDocumentation info = concatMap nodeDoc info.giNodes
  where
    nodeDoc :: NodeInfo -> [T.Text]
    nodeDoc node =
      [ "### `" <> node.niName <> "` (" <> kindName node.niKind <> ")"
      , ""
      ]
      ++ inputDoc node
      ++ outputDoc node
      ++ gotoDoc node
      ++ [""]

    kindName RuntimeLLM = "LLM Node"
    kindName RuntimeLogic = "Logic Node"

    inputDoc node = case node.niInput of
      Nothing -> []
      Just inputType -> ["**Input**: " <> formatType inputType]

    outputDoc node = case node.niSchema of
      Nothing -> []
      Just schema -> ["**Outputs**: " <> formatType schema]

    gotoDoc node
      | null node.niGotoTargets && not node.niHasGotoExit = []
      | otherwise =
          [ "**Transitions**: " <> T.intercalate ", " (
              map (\(name, ty) -> "`" <> name <> "` (" <> formatType ty <> ")") node.niGotoTargets
              ++ if node.niHasGotoExit then ["Exit"] else []
            )
          ]

-- | Generate edge documentation table rows.
edgeDocumentation :: GraphInfo -> [T.Text]
edgeDocumentation info =
  entryEdges ++ schemaEdges ++ gotoEdges ++ exitEdges
  where
    entryEdges = case info.giEntryType of
      Nothing -> []
      Just ty ->
        [ "| Entry | " <> node.niName <> " | " <> formatType ty <> " |"
        | node <- info.giNodes
        , node.niInput == Just ty
        ]

    schemaEdges =
      [ "| " <> producer.niName <> " | " <> consumer.niName <> " | " <> formatType ty <> " |"
      | producer <- info.giNodes
      , Just ty <- [producer.niSchema]
      , consumer <- info.giNodes
      , consumer.niInput == Just ty
      ]

    gotoEdges =
      [ "| " <> node.niName <> " | " <> targetName <> " | " <> formatType ty <> " |"
      | node <- info.giNodes
      , (targetName, ty) <- node.niGotoTargets
      ]

    exitEdges = case info.giExitType of
      Nothing ->
        [ "| " <> node.niName <> " | Exit | (via Goto) |"
        | node <- info.giNodes
        , node.niHasGotoExit
        ]
      Just ty ->
        [ "| " <> node.niName <> " | Exit | " <> formatType ty <> " |"
        | node <- info.giNodes
        , node.niHasGotoExit
        ]

-- | Format a TypeRep for display (strip module prefixes).
formatType :: TypeRep -> T.Text
formatType tr = simplifyType $ T.pack $ show tr
  where
    simplifyType t = T.pack $ go $ T.unpack t
    go s = case break (== '.') s of
      (_, []) -> s
      (_, '.':rest) ->
        case rest of
          (c:_) | c >= 'A' && c <= 'Z' -> go rest
          _ -> s
      _ -> s

-- | Type Definitions section showing all graph types with their fields.
typeDefinitionsSection :: [T.Text]
typeDefinitionsSection =
  [ ""
  , "## Type Definitions"
  , ""
  ]
  ++ renderTypeFromSchema "Input" "What enters the graph" inputSchema
  ++ [""]
  ++ renderTypeFromSchema "Output" "LLM structured output" outputSchema
  ++ [""]
  ++ renderTypeFromSchema "Result" "What exits the graph" resultSchema

-- | Render a type definition table from a JSONSchema.
renderTypeFromSchema :: T.Text -> T.Text -> JSONSchema -> [T.Text]
renderTypeFromSchema typeName desc schema =
  [ "### " <> typeName
  , ""
  , "_" <> desc <> "_"
  , ""
  , "| Field | Type | Description |"
  , "|-------|------|-------------|"
  ]
  ++ map renderField (Map.toList schema.schemaProperties)
  where
    renderField (fieldName, fieldSchema) =
      "| " <> fieldName
      <> " | " <> schemaTypeToText fieldSchema.schemaType
      <> " | " <> fromMaybe "" fieldSchema.schemaDescription <> " |"

    schemaTypeToText TString = "String"
    schemaTypeToText TNumber = "Number"
    schemaTypeToText TInteger = "Integer"
    schemaTypeToText TBoolean = "Boolean"
    schemaTypeToText TObject = "Object"
    schemaTypeToText TArray = "Array"
    schemaTypeToText TNull = "Null"

-- | JSON Schema section showing the structured output format.
jsonSchemaSection :: [T.Text]
jsonSchemaSection =
  [ ""
  , "## JSON Schema (LLM Output)"
  , ""
  , "The LLM node expects JSON matching this schema:"
  , ""
  , "```json"
  , TL.toStrict $ TLE.decodeUtf8 $ encodePretty $ schemaToValue outputSchema
  , "```"
  ]

-- | Context to template mapping section.
--
-- NOTE: This table is template-specific. Update if ProcessContext or its
-- ToGVal instance changes.
contextMappingSection :: [T.Text]
contextMappingSection =
  [ ""
  , "## Context to Template Mapping"
  , ""
  , "The LLM handler builds a `ProcessContext` which is rendered into the template:"
  , ""
  , "| Context Field | Template Variable | Source |"
  , "|---------------|-------------------|--------|"
  , "| `input` | `{{ input }}` | `Input.inputText` |"
  , "| `history` | `{{ history }}` | `ChatHistory` effect (formatted messages) |"
  , ""
  , "See [`src/Template/Context.hs`](../src/Template/Context.hs) for the full context definition."
  ]

-- | Example execution trace using Mermaid sequence diagram.
--
-- NOTE: The walkthrough steps and ExecutionPath are template-specific.
-- Update if the graph structure changes.
executionTraceSection :: GraphInfo -> [T.Text]
executionTraceSection graphInfo =
  [ ""
  , "## Example Execution Trace"
  , ""
  , "```mermaid"
  , toSequenceDiagram defaultConfig graphInfo examplePath
  , "```"
  , ""
  , "### Walkthrough"
  , ""
  , "1. **Entry** receives `Input { inputText = \"...\" }`"
  , "2. **sgProcess** builds template context, renders prompt, LLM returns `Output`"
  , "3. **sgRoute** wraps output in `Result`, calls `gotoExit`"
  , "4. **Exit** returns final `Result`"
  ]
  where
    examplePath = ExecutionPath ["sgProcess", "sgRoute"]

-- | Static section documenting graph validation rules.
validationRulesSection :: [T.Text]
validationRulesSection =
  [ ""
  , "## Validation Rules"
  , ""
  , "The graph is validated at compile time:"
  , ""
  , "| Rule | What It Checks |"
  , "|------|----------------|"
  , "| Single Entry/Exit | Exactly one `Entry` and one `Exit` field |"
  , "| Valid Gotos | All `Goto` targets exist as node field names |"
  , "| Reachability | Every node is reachable from Entry via data flow or Goto |"
  , "| Exit Coverage | Every Logic node has a path to Exit |"
  , "| Type Safety | Goto payloads satisfy target node's `Needs` |"
  ]

-- | Static section documenting available effects per node type.
availableEffectsSection :: [T.Text]
availableEffectsSection =
  [ ""
  , "## Available Effects"
  , ""
  , "| Node Type | Handler Type | Available Effects |"
  , "|-----------|--------------|-------------------|"
  , "| LLM Node | `LLMHandler needs schema targets es tpl` | Graph-level effects + Goto |"
  , "| Logic Node | `needs -> Eff es (GotoChoice targets)` | Graph-level effects + Goto |"
  , ""
  , "Common effects: `State s`, `Memory s`, `Emit event`, `RequestInput`, `Log`"
  ]

-- | Reference to handler implementations file.
handlerReferenceSection :: [T.Text]
handlerReferenceSection =
  [ ""
  , "## Handler Implementations"
  , ""
  , "See [`src/Template/Handlers.hs`](../src/Template/Handlers.hs) for handler implementations."
  ]
