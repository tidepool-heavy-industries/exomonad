-- | Issue filing effect for resident-initiated GitHub issues.
--
-- Effect type only - executors live in tidepool-issue-executor.
-- Residents use this to file issues when they encounter failures.
module Tidepool.Effects.Issue
  ( -- * Effect
    Issue(..)
  , fileIssue

    -- * Report Types
  , IssueReport(..)
  , IssueCategory(..)
  ) where

import Data.Text (Text)
import Data.Aeson (Value, ToJSON(..), FromJSON(..), object, (.=), withObject, withText, (.:), (.:?))
import GHC.Generics (Generic)
import Control.Monad.Freer (Eff, Member, send)


-- ════════════════════════════════════════════════════════════════════════════
-- REPORT TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Category of issue being filed.
--
-- Schema is seed—sleeptime evolves fields based on resolution correlation.
data IssueCategory
  = ParseFailure        -- ^ Failed to parse LLM output
  | EffectMissing       -- ^ Needed an effect that doesn't exist
  | EffectFailing       -- ^ Effect exists but fails consistently
  | UnexpectedState     -- ^ State was in unexpected configuration
  | PerformanceDegraded -- ^ Operation took too long
  | IssueOther          -- ^ Other issues
  deriving (Show, Eq, Generic, Enum, Bounded)

instance ToJSON IssueCategory where
  toJSON ParseFailure = "parse_failure"
  toJSON EffectMissing = "effect_missing"
  toJSON EffectFailing = "effect_failing"
  toJSON UnexpectedState = "unexpected_state"
  toJSON PerformanceDegraded = "performance_degraded"
  toJSON IssueOther = "other"

instance FromJSON IssueCategory where
  parseJSON = Data.Aeson.withText "IssueCategory" $ \case
    "parse_failure" -> pure ParseFailure
    "effect_missing" -> pure EffectMissing
    "effect_failing" -> pure EffectFailing
    "unexpected_state" -> pure UnexpectedState
    "performance_degraded" -> pure PerformanceDegraded
    "other" -> pure IssueOther
    t -> fail $ "Unknown issue category: " ++ show t

-- | Structured issue report from a resident.
--
-- Fields shape how residents perceive and articulate their limitations.
-- Schema and filing heuristics co-evolve via sleeptime observation.
data IssueReport = IssueReport
  { irCategory :: IssueCategory
  , irEffect :: Maybe Text       -- ^ Which effect failed
  , irInput :: Maybe Value       -- ^ What input triggered it
  , irError :: Maybe Text        -- ^ Error message if any
  , irFrequency :: Maybe Text    -- ^ How often this occurs
  , irPattern :: Maybe Text      -- ^ When fails vs succeeds
  , irContext :: Maybe Text      -- ^ Freeform resident reasoning
  }
  deriving (Show, Eq, Generic)

instance ToJSON IssueReport where
  toJSON r = object
    [ "category" .= r.irCategory
    , "effect" .= r.irEffect
    , "input" .= r.irInput
    , "error" .= r.irError
    , "frequency" .= r.irFrequency
    , "pattern" .= r.irPattern
    , "context" .= r.irContext
    ]

instance FromJSON IssueReport where
  parseJSON = withObject "IssueReport" $ \v ->
    IssueReport
      <$> v .: "category"
      <*> v .:? "effect"
      <*> v .:? "input"
      <*> v .:? "error"
      <*> v .:? "frequency"
      <*> v .:? "pattern"
      <*> v .:? "context"


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT
-- ════════════════════════════════════════════════════════════════════════════

-- | Issue filing effect for residents.
--
-- Fire-and-forget: resident doesn't block on response.
data Issue r where
  FileIssue :: IssueReport -> Issue ()


-- ════════════════════════════════════════════════════════════════════════════
-- SMART CONSTRUCTORS
-- ════════════════════════════════════════════════════════════════════════════

-- | File an issue report.
fileIssue :: Member Issue effs => IssueReport -> Eff effs ()
fileIssue = send . FileIssue
