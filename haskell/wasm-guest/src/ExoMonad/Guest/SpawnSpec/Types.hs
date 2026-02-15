{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExoMonad.Guest.SpawnSpec.Types
  ( Zone (..),
    ZoneDefaults (..),
    Step (..),
    PlanRef (..),
    SpawnSpec (..),
    Command (..),
    CompileReport (..),
    emptySpec,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), withObject, withText, (.!=), (.:), (.:?))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Generics (Generic)

-- | A zone represents a file-ownership area with default build/verify context.
data Zone = Proto | RustCore | HaskellWasm | RustBinary | Justfile
  deriving (Show, Eq, Ord, Bounded, Enum, Generic)

instance FromJSON Zone where
  parseJSON = withText "Zone" $ \case
    "proto" -> pure Proto
    "rust-core" -> pure RustCore
    "rust_core" -> pure RustCore
    "haskell-wasm" -> pure HaskellWasm
    "haskell_wasm" -> pure HaskellWasm
    "rust-binary" -> pure RustBinary
    "rust_binary" -> pure RustBinary
    "justfile" -> pure Justfile
    other -> fail $ "Unknown zone: " <> show other

instance ToJSON Zone where
  toJSON Proto = "proto"
  toJSON RustCore = "rust-core"
  toJSON HaskellWasm = "haskell-wasm"
  toJSON RustBinary = "rust-binary"
  toJSON Justfile = "justfile"

-- | Shell command (newtype for type safety).
newtype Command = Command {unCommand :: Text}
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Command where
  parseJSON v = Command <$> parseJSON v

instance ToJSON Command where
  toJSON (Command t) = toJSON t

-- | Defaults contributed by a zone.
data ZoneDefaults = ZoneDefaults
  { -- | CLAUDE.md paths, key source files
    zdReadFirst :: Set Text,
    -- | build/test commands
    zdVerify :: Set Command,
    -- | e.g. PROTOC path
    zdEnvVars :: Map Text Text,
    -- | boundary rules
    zdRules :: [Text]
  }
  deriving (Show, Eq, Generic)

instance Semigroup ZoneDefaults where
  a <> b =
    ZoneDefaults
      { zdReadFirst = zdReadFirst a <> zdReadFirst b,
        zdVerify = zdVerify a <> zdVerify b,
        zdEnvVars = zdEnvVars a <> zdEnvVars b,
        zdRules = zdRules a <> zdRules b
      }

instance Monoid ZoneDefaults where
  mempty = ZoneDefaults Set.empty Set.empty Map.empty []

-- | A step in the execution plan.
data Step = Step
  { -- | what to do
    stepAction :: Text,
    -- | which files
    stepFiles :: [Text],
    -- | optional code snippet
    stepSnippet :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON Step where
  parseJSON = withObject "Step" $ \v ->
    Step
      <$> v .: "action"
      <*> v .:? "files" .!= []
      <*> v .:? "snippet"

instance ToJSON Step

-- | Reference to a persistent plan file.
data PlanRef = PlanRef
  { -- | must be under .exomonad/plans/
    planPath :: Text,
    -- | optional heading to extract
    planSection :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON PlanRef where
  parseJSON = withObject "PlanRef" $ \v ->
    PlanRef
      <$> v .: "path"
      <*> v .:? "section"

instance ToJSON PlanRef

-- | The full spawn specification.
data SpawnSpec = SpawnSpec
  { specZones :: Set Zone,
    specSteps :: [Step],
    -- | additional verify beyond zone defaults
    specVerify :: Set Command,
    specDoneCriteria :: [Text],
    specPlan :: Maybe PlanRef,
    specNoCommit :: Bool,
    -- | TL reasoning summary
    specContext :: Maybe Text,
    specKeyDecisions :: [Text],
    specNonGoals :: [Text]
  }
  deriving (Show, Eq, Generic)

instance FromJSON SpawnSpec where
  parseJSON = withObject "SpawnSpec" $ \v ->
    SpawnSpec
      <$> v .:? "zones" .!= Set.empty
      <*> v .:? "steps" .!= []
      <*> v .:? "verify" .!= Set.empty
      <*> v .:? "done_criteria" .!= []
      <*> v .:? "plan" .!= Nothing
      <*> v .:? "no_commit" .!= True
      <*> v .:? "context" .!= Nothing
      <*> v .:? "key_decisions" .!= []
      <*> v .:? "non_goals" .!= []

-- | Empty spec with sensible defaults.
emptySpec :: SpawnSpec
emptySpec = SpawnSpec Set.empty [] Set.empty [] Nothing True Nothing [] []

-- | What the compiler reports.
data CompileReport = CompileReport
  { crZonesApplied :: Set Zone,
    crVerifyCommands :: Set Command,
    crReadFirstPaths :: Set Text,
    crRulesApplied :: [Text]
  }
  deriving (Show, Eq, Generic)

instance ToJSON CompileReport
