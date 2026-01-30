{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module AmbiguityRecoverySpec (spec) where

import Control.Monad.Freer (Eff)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import ExoMonad.LLM.Tools.TH (Tool (..), deriveToolRecord)
import ExoMonad.Schema (HasJSONSchema (..), deriveHasJSONSchema)
import Language.Haskell.TH (recover)
import Test.Hspec

-- 1. Define the "Bad" types that should trigger the error
data BadArgs = BadArgs { value :: Text }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

deriveHasJSONSchema ''BadArgs

data BadTools es = BadTools
  { badTool :: BadArgs -> Eff es Text }

-- 2. Use TH recover to verify that deriveToolRecord fails
-- If deriveToolRecord fails (calls 'fail'), recover runs the first argument.
-- If deriveToolRecord succeeds (bug), recover runs the second argument.
$(recover 
    [d| ambiguityCheckPassed :: Bool
        ambiguityCheckPassed = True
      |] 
    (do
       _ <- deriveToolRecord ''BadTools [ Tool "badTool" "Should fail" ]
       [d| ambiguityCheckPassed :: Bool
           ambiguityCheckPassed = False
         |]
    )
 )

-- 3. Define a "Good" type with "value" field + others (should pass)
data GoodArgs = GoodArgs { value :: Text, other :: Int }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

deriveHasJSONSchema ''GoodArgs

data GoodTools es = GoodTools
  { goodTool :: GoodArgs -> Eff es Text }

-- This should compile fine
deriveToolRecord ''GoodTools [ Tool "goodTool" "Should pass" ]

spec :: Spec
spec = describe "ToolRecord TH Ambiguity Validation" $ do
  it "detects single-field 'value' record and fails compilation" $ do
    ambiguityCheckPassed `shouldBe` True
    
  it "allows 'value' field in multi-field records" $ do
    -- If GoodTools instance exists, this compiles and passes
    True `shouldBe` True
