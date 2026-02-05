module Main (main) where

import DecisionSpec qualified
import SchemaDerivationSpec qualified
import StructuredOutputSpec qualified
import TUIWireFormatSpec qualified
import Test.Hspec
import ToolRecordTHSpec qualified
import ToolSchemaBugSpec qualified

main :: IO ()
main = hspec $ do
  describe "Tool Schema Bug" $ do
    ToolSchemaBugSpec.spec

  describe "Schema Derivation" $ do
    SchemaDerivationSpec.spec

  describe "StructuredOutput" $ do
    StructuredOutputSpec.spec

  describe "Decision" $ do
    DecisionSpec.spec

  describe "TUI Wire Format" $ do
    TUIWireFormatSpec.spec

  describe "ToolRecord TH" $ do
    ToolRecordTHSpec.spec
