{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Codebase exploration tool.
module ExoMonad.Guest.Tools.Explorer
  ( -- * Tool types
    ExploreCodebase,
    ContinueExploration,

    -- * Argument types
    ExploreCodebaseArgs (..),
    ContinueExplorationArgs (..),
  )
where

import Control.Monad (forM)
import Control.Monad.Freer (Eff, Member, runM)
import Data.Aeson (FromJSON (..), ToJSON (..), object, toJSON, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString.Lazy qualified as BSL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import ExoMonad.Guest.Effects.Explore qualified as Exp
import ExoMonad.Guest.Effects.LLM qualified as LLM
import ExoMonad.Guest.Graph qualified as Graph
import ExoMonad.Guest.Tool.Class
import GHC.Generics (Generic)

-- ============================================================================
-- ExploreCodebase
-- ============================================================================

data ExploreCodebase

data ExploreCodebaseArgs = ExploreCodebaseArgs
  { ecQuestion :: Text,
    ecSeed :: Maybe Text,
    ecMaxSteps :: Maybe Int,
    ecLanguages :: Maybe [Text],
    ecDepth :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON ExploreCodebaseArgs where
  parseJSON = Aeson.withObject "ExploreCodebaseArgs" $ \v ->
    ExploreCodebaseArgs
      <$> v .: "question"
      <*> v .:? "seed"
      <*> v .:? "max_steps"
      <*> v .:? "languages"
      <*> v .:? "depth"

instance MCPTool ExploreCodebase where
  type ToolArgs ExploreCodebase = ExploreCodebaseArgs
  toolName = "explore_codebase"
  toolDescription = "Iteratively explore the codebase to answer a question"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "required" .= (["question"] :: [Text]),
        "properties"
          .= object
            [ "question"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Question to answer" :: Text)
                  ],
              "seed"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Optional starting point (file path or symbol)" :: Text)
                  ],
              "max_steps"
                .= object
                  [ "type" .= ("integer" :: Text),
                    "description" .= ("Maximum exploration steps (default: 10)" :: Text)
                  ],
              "languages"
                .= object
                  [ "type" .= ("array" :: Text),
                    "items" .= object ["type" .= ("string" :: Text)],
                    "description" .= ("Languages to include (default: ['haskell'])" :: Text)
                  ],
              "depth"
                .= object
                  [ "type" .= ("string" :: Text),
                    "enum" .= (["quick", "medium", "thorough"] :: [Text]),
                    "description" .= ("Exploration depth" :: Text)
                  ]
            ]
      ]
  toolHandler args = do
    -- Run the exploration loop
    result <- runM $ Exp.runExplore $ LLM.runLLM $ do
      let maxSteps = fromMaybe 10 (ecMaxSteps args)
      let question = ecQuestion args
      let seed = ecSeed args

      exploreLoop question seed maxSteps Graph.emptyGraph []

    pure $ successResult (toJSON result)

-- ============================================================================
-- Exploration Logic
-- ============================================================================

data ExploreAction
  = AstGrepSearch Text Text Text -- language pattern path
  | LspReferences Text Exp.Position
  | LspDefinition Text Exp.Position
  | LspHover Text Exp.Position
  | ReadFileRange Text Int Int
  | Conclude Text
  deriving (Show, Eq, Generic)

instance FromJSON ExploreAction where
  parseJSON = Aeson.withObject "ExploreAction" $ \v -> do
    action <- v .: "action" :: Parser Text
    case action of
      "ast_grep" -> do
        args <- v .: "args"
        AstGrepSearch <$> args .: "language" <*> args .: "pattern" <*> args .: "path"
      "lsp_refs" -> do
        args <- v .: "args"
        LspReferences <$> args .: "file" <*> (Exp.Position <$> args .: "line" <*> args .: "col")
      "lsp_def" -> do
        args <- v .: "args"
        LspDefinition <$> args .: "file" <*> (Exp.Position <$> args .: "line" <*> args .: "col")
      "lsp_hover" -> do
        args <- v .: "args"
        LspHover <$> args .: "file" <*> (Exp.Position <$> args .: "line" <*> args .: "col")
      "read_file" -> do
        args <- v .: "args"
        ReadFileRange <$> args .: "file" <*> args .: "start" <*> args .: "end"
      "conclude" -> do
        args <- v .: "args"
        Conclude <$> args .: "summary"
      _ -> fail $ "Unknown action: " ++ T.unpack action

data ExplorationResult = ExplorationResult
  { erSummary :: Text,
    erGraph :: Graph.ExplorationGraph,
    erSteps :: Int,
    erTrace :: [Text]
  }
  deriving (Show, Eq, Generic)

instance ToJSON ExplorationResult where
  toJSON (ExplorationResult s g st t) =
    object
      [ "summary" .= s,
        "graph" .= g,
        "steps_used" .= st,
        "trace" .= t
      ]

exploreLoop ::
  (Member Exp.Explore effs, Member LLM.LLM effs) =>
  Text ->
  Maybe Text ->
  Int ->
  Graph.ExplorationGraph ->
  [Text] ->
  Eff effs ExplorationResult
exploreLoop question seed stepsRemaining graph trace = do
  if stepsRemaining <= 0
    then pure $ ExplorationResult "Max steps reached" graph (10 - stepsRemaining) trace
    else do
      -- Construct prompt
      let systemPrompt =
            T.unlines
              [ "You are a codebase exploration agent. Your goal is to answer the user's question by exploring the code.",
                "Available actions:",
                "- ast_grep(language, pattern, path): Structural search",
                "- lsp_refs(file, line, col): Find references",
                "- lsp_def(file, line, col): Go to definition",
                "- lsp_hover(file, line, col): Get hover info",
                "- read_file(file, start, end): Read file content",
                "- conclude(summary): Finish exploration with a summary answer",
                "",
                "Respond with JSON: {\"action\": \"...\", \"args\": {...}, \"reasoning\": \"...\"}"
              ]

      let userPrompt =
            T.concat
              [ "Question: ",
                question,
                "\n\nCurrent Graph Nodes: ",
                T.pack (show (length (Graph.graphNodes graph))),
                "\n\nTrace:\n",
                T.unlines trace,
                "\n\nRemaining Steps: ",
                T.pack (show stepsRemaining)
              ]

      -- Call LLM
      output <-
        LLM.complete
          "claude-3-haiku-20240307"
          [LLM.ChatMessage "user" userPrompt]
          1024
          (Just systemPrompt)

      -- Parse action
      -- Ideally we should use a more robust parser or structured output
      let content = LLM.lcoContent output
      case Aeson.decode (BSL.fromStrict (TE.encodeUtf8 content)) of -- Hacky: re-encoding to decode
        Nothing -> do
          -- Failed to parse JSON, log it and retry or skip
          let newTrace = trace ++ ["Failed to parse JSON: " <> content]
          exploreLoop question seed (stepsRemaining - 1) graph newTrace
        Just action -> do
          -- Execute action
          (resultSummary, newNodes, newEdges) <- executeAction action

          let newGraph =
                foldr Graph.addNode (foldr Graph.addEdge graph newEdges) newNodes

          let newTrace = trace ++ ["Action: " <> T.pack (show action), "Result: " <> resultSummary]

          case action of
            Conclude summary ->
              pure $ ExplorationResult summary newGraph (10 - stepsRemaining) newTrace
            _ ->
              exploreLoop question seed (stepsRemaining - 1) newGraph newTrace

executeAction ::
  (Member Exp.Explore effs) =>
  ExploreAction ->
  Eff effs (Text, [Graph.Node], [Graph.Edge])
executeAction action = case action of
  AstGrepSearch lang pat path -> do
    locs <- Exp.astGrep lang pat path
    let nodes = map (locToNode "ast_grep") locs
    pure ("Found " <> T.pack (show (length locs)) <> " matches", nodes, [])
  LspReferences file pos -> do
    locs <- Exp.lspReferences file pos
    let nodes = map (locToNode "lsp_ref") locs
    pure ("Found " <> T.pack (show (length locs)) <> " references", nodes, [])
  LspDefinition file pos -> do
    locs <- Exp.lspDefinition file pos
    let nodes = map (locToNode "lsp_def") locs
    pure ("Found " <> T.pack (show (length locs)) <> " definitions", nodes, [])
  LspHover file pos -> do
    maybeHover <- Exp.lspHover file pos
    case maybeHover of
      Nothing -> pure ("No hover info", [], [])
      Just h -> pure ("Hover: " <> h, [], [])
  ReadFileRange file start end -> do
    output <- Exp.readFileRange file start end
    pure ("Read " <> T.pack (show (T.length (Exp.rfroContent output))) <> " chars", [], [])
  Conclude summary ->
    pure ("Concluded: " <> summary, [], [])

locToNode :: Text -> Exp.Location -> Graph.Node
locToNode source loc =
  Graph.Node
    { Graph.nodeId = Exp.locUri loc <> ":" <> T.pack (show (Exp.posLine (Exp.rangeStart (Exp.locRange loc)))),
      Graph.nodeType = Graph.NTOther source,
      Graph.nodeFile = T.unpack (Exp.locUri loc),
      Graph.nodeLine = Exp.posLine (Exp.rangeStart (Exp.locRange loc)),
      Graph.nodeSnippet = Exp.locContext loc
    }

-- ============================================================================
-- ContinueExploration
-- ============================================================================

data ContinueExploration

data ContinueExplorationArgs = ContinueExplorationArgs
  { ceExplorationId :: Text,
    ceFollowup :: Text,
    ceAdditionalSteps :: Maybe Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON ContinueExplorationArgs where
  parseJSON = Aeson.withObject "ContinueExplorationArgs" $ \v ->
    ContinueExplorationArgs
      <$> v .: "exploration_id"
      <*> v .: "followup"
      <*> v .:? "additional_steps"

instance MCPTool ContinueExploration where
  type ToolArgs ContinueExploration = ContinueExplorationArgs
  toolName = "continue_exploration"
  toolDescription = "Resume or extend a previous exploration"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "required" .= (["exploration_id", "followup"] :: [Text]),
        "properties"
          .= object
            [ "exploration_id"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("ID of previous exploration" :: Text)
                  ],
              "followup"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Follow-up question or instruction" :: Text)
                  ],
              "additional_steps"
                .= object
                  [ "type" .= ("integer" :: Text),
                    "description" .= ("Additional steps to execute" :: Text)
                  ]
            ]
      ]
  toolHandler _args = do
    pure $ successResult $ object ["status" .= ("Not implemented yet" :: Text)]
