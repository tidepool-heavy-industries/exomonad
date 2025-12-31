{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests for Mermaid diagram generation.
--
-- Tests:
-- 1. graphToMermaid produces valid Mermaid syntax from graph types
-- 2. Template dependency parsing extracts include/extends directives
module MermaidSpec (spec) where

import Test.Hspec
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Tidepool.Graph.Types (type (:@), Needs, Schema)
import Tidepool.Graph.Generic (GraphMode(..), Entry, Exit, LLMNode, LogicNode)
import Tidepool.Graph.Mermaid (graphToMermaid)
import Tidepool.Template.DependencyTree
  ( parseTemplateIncludes
  , parseTemplateExtends
  , parseAllDependencies
  )

-- ════════════════════════════════════════════════════════════════════════════
-- TEST GRAPH TYPES
-- ════════════════════════════════════════════════════════════════════════════

data InputA
data OutputB

-- | Simple graph for testing Mermaid output: Entry -> llmNode -> Exit
data SimpleTestGraph mode = SimpleTestGraph
  { stgEntry   :: mode :- Entry InputA
  , stgProcess :: mode :- LLMNode :@ Needs '[InputA] :@ Schema OutputB
  , stgExit    :: mode :- Exit OutputB
  }
  deriving Generic

data TypeX
data TypeY
data TypeZ

-- | Branching graph with Logic node for more complex Mermaid output
data BranchingTestGraph mode = BranchingTestGraph
  { btgEntry  :: mode :- Entry TypeX
  , btgRouter :: mode :- LogicNode :@ Needs '[TypeX] :@ Schema TypeY
  , btgLlm    :: mode :- LLMNode :@ Needs '[TypeY] :@ Schema TypeZ
  , btgExit   :: mode :- Exit TypeZ
  }
  deriving Generic

-- ════════════════════════════════════════════════════════════════════════════
-- SPEC
-- ════════════════════════════════════════════════════════════════════════════

spec :: Spec
spec = do
  describe "graphToMermaid" $ do
    it "generates flowchart header" $ do
      let output = graphToMermaid (Proxy @SimpleTestGraph)
      T.pack "flowchart TD" `shouldSatisfy` (`T.isInfixOf` output)

    it "generates entry node for simple graph" $ do
      let output = graphToMermaid (Proxy @SimpleTestGraph)
      -- Entry nodes use double parentheses in Mermaid
      T.pack "entry((start))" `shouldSatisfy` (`T.isInfixOf` output)

    it "generates exit node for simple graph" $ do
      let output = graphToMermaid (Proxy @SimpleTestGraph)
      T.pack "exit__((end))" `shouldSatisfy` (`T.isInfixOf` output)

    it "generates LLM node with correct styling" $ do
      let output = graphToMermaid (Proxy @SimpleTestGraph)
      -- LLM nodes use stadium shape [[...]]
      T.pack "stgProcess[[" `shouldSatisfy` (`T.isInfixOf` output)
      T.pack "LLM" `shouldSatisfy` (`T.isInfixOf` output)

    it "generates edges between nodes" $ do
      let output = graphToMermaid (Proxy @SimpleTestGraph)
      -- Should have edge from entry to process
      T.pack "entry -->" `shouldSatisfy` (`T.isInfixOf` output)

    it "generates Logic node with correct styling for branching graph" $ do
      let output = graphToMermaid (Proxy @BranchingTestGraph)
      -- Logic nodes use rhombus shape {...}
      T.pack "btgRouter{" `shouldSatisfy` (`T.isInfixOf` output)
      T.pack "Logic" `shouldSatisfy` (`T.isInfixOf` output)

  describe "parseTemplateIncludes" $ do
    it "parses double-quoted include directive" $ do
      let template = "{% include \"_shared/context.jinja\" %}"
      parseTemplateIncludes template `shouldBe` ["_shared/context.jinja"]

    it "parses single-quoted include directive" $ do
      let template = "{% include '_shared/context.jinja' %}"
      parseTemplateIncludes template `shouldBe` ["_shared/context.jinja"]

    it "parses multiple include directives" $ do
      let template = T.unlines
            [ "{% include \"header.jinja\" %}"
            , "Content here"
            , "{% include \"footer.jinja\" %}"
            ]
      parseTemplateIncludes template `shouldBe` ["header.jinja", "footer.jinja"]

    it "returns empty list for template without includes" $ do
      let template = "Just some {{ variable }} content"
      parseTemplateIncludes template `shouldBe` []

    it "handles whitespace variations" $ do
      let template = "{%   include   \"spaced.jinja\"   %}"
      parseTemplateIncludes template `shouldBe` ["spaced.jinja"]

  describe "parseTemplateExtends" $ do
    it "parses extends directive" $ do
      let template = "{% extends \"base.jinja\" %}\n{% block content %}...{% endblock %}"
      parseTemplateExtends template `shouldBe` ["base.jinja"]

    it "returns empty list for template without extends" $ do
      let template = "{% include \"something.jinja\" %}"
      parseTemplateExtends template `shouldBe` []

  describe "parseAllDependencies" $ do
    it "parses both includes and extends" $ do
      let template = T.unlines
            [ "{% extends \"base.jinja\" %}"
            , "{% include \"header.jinja\" %}"
            , "Content"
            , "{% include \"footer.jinja\" %}"
            ]
      let (incs, exts) = parseAllDependencies template
      incs `shouldBe` ["header.jinja", "footer.jinja"]
      exts `shouldBe` ["base.jinja"]

    it "returns empty lists for plain content" $ do
      let template = "Hello {{ name }}, welcome!"
      parseAllDependencies template `shouldBe` ([], [])
