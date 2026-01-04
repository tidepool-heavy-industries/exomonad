-- | Issue template rendering - transforms IssueReport to GitHub issue markdown.
--
-- Template structure evolves via sleeptime observation of which issue formats
-- lead to faster resolution. The current template is a seed that establishes
-- baseline categories and fields.
module Tidepool.Issue.Template
  ( -- * Rendering
    renderIssueBody
  , issueTitle

    -- * Category Display
  , categoryTitle
  , categoryEmoji
  ) where

import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Aeson (Value, encode)
import Data.Aeson.Text (encodeToLazyText)
import Data.Text.Lazy qualified as TL

import Tidepool.Effects.Issue (IssueReport(..), IssueCategory(..))


-- ════════════════════════════════════════════════════════════════════════════
-- CATEGORY DISPLAY
-- ════════════════════════════════════════════════════════════════════════════

-- | Human-readable title for issue category.
categoryTitle :: IssueCategory -> Text
categoryTitle = \case
  ParseFailure       -> "Parse Failure"
  EffectMissing      -> "Missing Effect"
  EffectFailing      -> "Effect Failing"
  UnexpectedState    -> "Unexpected State"
  PerformanceDegraded -> "Performance Degraded"
  IssueOther         -> "Other"

-- | Emoji prefix for category (makes issues scannable in GitHub UI).
categoryEmoji :: IssueCategory -> Text
categoryEmoji = \case
  ParseFailure       -> "\128295"  -- wrench
  EffectMissing      -> "\128640"  -- rocket
  EffectFailing      -> "\128165"  -- collision
  UnexpectedState    -> "\128552"  -- confused face
  PerformanceDegraded -> "\128034"  -- turtle
  IssueOther         -> "\128172"  -- speech bubble


-- ════════════════════════════════════════════════════════════════════════════
-- TITLE GENERATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Generate issue title from report.
--
-- Format: "[Category] Brief description"
-- Description derived from effect name if available, otherwise category.
issueTitle :: IssueReport -> Text
issueTitle report =
  let category = categoryTitle report.irCategory
      detail = case report.irEffect of
        Just effect -> effect <> " effect"
        Nothing -> case report.irError of
          Just err -> T.take 50 err  -- Truncate long errors
          Nothing -> "issue"
  in "[" <> category <> "] " <> detail


-- ════════════════════════════════════════════════════════════════════════════
-- BODY RENDERING
-- ════════════════════════════════════════════════════════════════════════════

-- | Render full issue body as GitHub-flavored markdown.
--
-- Structure:
-- 1. Category header with emoji
-- 2. Effect/input/error details (if present)
-- 3. Occurrence section (frequency, pattern)
-- 4. Resident context (freeform reasoning)
-- 5. Footer noting automatic filing
renderIssueBody :: IssueReport -> Text
renderIssueBody report = T.unlines $ catMaybes
  [ Just $ "## " <> categoryEmoji report.irCategory <> " " <> categoryTitle report.irCategory
  , Just ""
  , renderEffect report.irEffect
  , renderInput report.irInput
  , renderError report.irError
  , renderOccurrence report.irFrequency report.irPattern
  , renderContext report.irContext
  , Just ""
  , Just "---"
  , Just "*Filed automatically by tidepool resident*"
  ]


-- | Render effect name if present.
renderEffect :: Maybe Text -> Maybe Text
renderEffect = fmap $ \effect ->
  "**Effect:** `" <> effect <> "`"


-- | Render input JSON if present.
renderInput :: Maybe Value -> Maybe Text
renderInput Nothing = Nothing
renderInput (Just input) = Just $ T.unlines
  [ "**Input:**"
  , "```json"
  , TL.toStrict $ encodeToLazyText input
  , "```"
  ]


-- | Render error message if present.
renderError :: Maybe Text -> Maybe Text
renderError = fmap $ \err ->
  "**Error:** " <> err


-- | Render occurrence section (frequency + pattern) if either present.
renderOccurrence :: Maybe Text -> Maybe Text -> Maybe Text
renderOccurrence Nothing Nothing = Nothing
renderOccurrence freq pat = Just $ T.unlines $ catMaybes
  [ Just ""
  , Just "### Occurrence"
  , fmap (\f -> "- **Frequency:** " <> f) freq
  , fmap (\p -> "- **Pattern:** " <> p) pat
  ]


-- | Render resident context if present.
renderContext :: Maybe Text -> Maybe Text
renderContext Nothing = Nothing
renderContext (Just ctx) = Just $ T.unlines
  [ ""
  , "### Resident Context"
  , ctx
  ]
