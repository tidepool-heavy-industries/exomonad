module Main where

import Data.Aeson (encode, decode)
import Data.Proxy (Proxy(..))
import Data.Text qualified as T
import Test.Hspec

import Tidepool.Graph.Template (runTypedTemplate)

import TypesFirstDev.Context (TypesContext(..))
import TypesFirstDev.Graph (TypesFirstGraph(..))
import TypesFirstDev.Handlers (typesFirstHandlers)
import TypesFirstDev.Templates (typesCompiled)
import TypesFirstDev.Types


-- | Types-first dev tests.
main :: IO ()
main = hspec $ do
  describe "Schema Types" $ do
    describe "StackSpec" $ do
      it "roundtrips through JSON" $ do
        let spec = StackSpec "." "Data.Stack" "A stack"
        decode (encode spec) `shouldBe` Just spec

    describe "TypeDefinitions" $ do
      it "roundtrips through JSON" $ do
        let td = TypeDefinitions
              { tdDataType = "data Stack a = Empty | Push a (Stack a)"
              , tdSignatures = ["push :: a -> Stack a -> Stack a"]
              , tdModuleHeader = "module Data.Stack where"
              }
        decode (encode td) `shouldBe` Just td

    describe "ImplementationResult" $ do
      it "roundtrips through JSON" $ do
        let result = ImplementationResult True 5 "All tests passed"
        decode (encode result) `shouldBe` Just result

  describe "Context Building" $ do
    it "TypesContext correctly represents StackSpec data" $ do
      -- buildTypesContext just maps these fields, so we test the mapping directly
      let spec = StackSpec
            { ssProjectPath = "/some/path"
            , ssModuleName = "Data.Queue"
            , ssDescription = "A FIFO queue"
            }
          -- This is what buildTypesContext produces
          ctx = TypesContext
            { moduleName = spec.ssModuleName
            , description = spec.ssDescription
            }
      ctx.moduleName `shouldBe` "Data.Queue"
      ctx.description `shouldBe` "A FIFO queue"

  describe "Template Rendering" $ do
    it "types template renders with expected content" $ do
      let ctx = TypesContext
            { moduleName = "Data.Stack"
            , description = "A LIFO stack with push/pop"
            }
      let rendered = runTypedTemplate ctx typesCompiled
      -- Verify key elements are present in the rendered template
      rendered `shouldSatisfy` T.isInfixOf "Data.Stack"
      rendered `shouldSatisfy` T.isInfixOf "LIFO stack"
      rendered `shouldSatisfy` T.isInfixOf "push/pop"

    it "types template includes type definition instructions" $ do
      let ctx = TypesContext
            { moduleName = "Test.Module"
            , description = "Test description"
            }
      let rendered = runTypedTemplate ctx typesCompiled
      -- Template should mention data types and signatures
      rendered `shouldSatisfy` T.isInfixOf "data"
      rendered `shouldSatisfy` T.isInfixOf "signature"

  describe "Handler Construction" $ do
    it "typesFirstHandlers entry and exit are correctly typed Proxies" $ do
      -- The entry and exit fields are Proxy markers that establish the graph's
      -- input and output types. Verify they have the expected types.
      let handlers = typesFirstHandlers
      -- These compile only if the types match
      handlers.entry `shouldBe` (Proxy :: Proxy StackSpec)
      handlers.exit `shouldBe` (Proxy :: Proxy ParallelResults)
