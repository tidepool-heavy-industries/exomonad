{-# LANGUAGE RecordWildCards #-}

-- | Persistence layer for development run results.
--
-- Writes run metadata to ~/tidepool-labs/dev-runs/ with both JSON
-- and human-readable markdown summaries.
module TypesFirstDev.DevRuns
  ( -- * Persistence
    persistRunMetadata
  , loadRunMetadata

    -- * Rendering
  , renderSummary

    -- * Paths
  , devRunsBaseDir
  , runDirectory
  ) where

import Data.Aeson (encodeFile, eitherDecodeFileStrict)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time ()
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Text.Printf (printf)

import TypesFirstDev.Stats
import TypesFirstDev.Types (StackSpec(..))


-- | Base directory for all dev runs.
devRunsBaseDir :: FilePath
devRunsBaseDir = "/home/inanna/tidepool-labs/dev-runs"


-- | Get the directory for a specific run.
--
-- Baseline runs go to: baseline/run-<timestamp>/
-- Experiment runs go to: experiments/<name>/run-<timestamp>/
runDirectory :: RunMetadata -> FilePath
runDirectory meta = case meta.rmExperiment of
  Nothing ->
    devRunsBaseDir </> "baseline" </> T.unpack meta.rmRunId
  Just exp ->
    devRunsBaseDir </> "experiments" </> T.unpack exp.epName </> T.unpack meta.rmRunId


-- | Persist run metadata to disk.
--
-- Creates the directory structure and writes:
-- - metadata.json: Full structured data
-- - summary.md: Human-readable summary
persistRunMetadata :: RunMetadata -> IO FilePath
persistRunMetadata meta = do
  let dir = runDirectory meta
  createDirectoryIfMissing True dir

  -- Write JSON metadata
  let metaPath = dir </> "metadata.json"
  encodeFile metaPath meta

  -- Write markdown summary
  let summaryPath = dir </> "summary.md"
      summary = renderSummary meta
  TIO.writeFile summaryPath summary

  putStrLn $ "Run persisted to: " <> dir
  pure dir


-- | Load run metadata from disk.
loadRunMetadata :: FilePath -> IO (Either String RunMetadata)
loadRunMetadata dir = do
  let metaPath = dir </> "metadata.json"
  eitherDecodeFileStrict metaPath


-- | Render a human-readable markdown summary.
renderSummary :: RunMetadata -> Text
renderSummary RunMetadata{..} = T.unlines $
  [ "# Run Summary"
  , ""
  , "## Overview"
  , ""
  , "| Metric | Value |"
  , "|--------|-------|"
  , "| Run ID | " <> rmRunId <> " |"
  , "| Timestamp | " <> T.pack (show rmTimestamp) <> " |"
  , "| Duration | " <> formatDuration rmDurationSeconds <> " |"
  , "| Success | " <> (if rmSuccess then "Yes" else "**No**") <> " |"
  , "| Total Cost | " <> formatCost rmTotalCost <> " |"
  , "| Total Tokens | " <> T.pack (show rmTotalTokens) <> " |"
  , ""
  ] ++
  experimentSection rmExperiment ++
  [ "## Specification"
  , ""
  , "- **Module**: " <> rmSpec.ssModuleName
  , "- **Type**: " <> T.pack (show rmSpec.ssProjectType)
  , "- **Description**: " <> rmSpec.ssDescription
  , ""
  , "### Acceptance Criteria"
  , ""
  ] ++
  map ("- " <>) rmSpec.ssAcceptanceCriteria ++
  [ ""
  , "## Agent Statistics"
  , ""
  ] ++
  renderAgentStatsTable rmAgentStats


-- | Render experiment section if present.
experimentSection :: Maybe ExperimentParams -> [Text]
experimentSection Nothing = []
experimentSection (Just ExperimentParams{..}) =
  [ "## Experiment"
  , ""
  , "| Field | Value |"
  , "|-------|-------|"
  , "| Name | " <> epName <> " |"
  , "| Branch | `" <> epBranch <> "` |"
  , "| Description | " <> epDescription <> " |"
  , ""
  , "### Changes"
  , ""
  ] ++
  map ("- " <>) epChanges ++
  [ ""
  ]


-- | Render agent stats as a markdown table.
renderAgentStatsTable :: Map Text AgentStats -> [Text]
renderAgentStatsTable stats =
  if Map.null stats
  then ["_No agent statistics recorded._", ""]
  else
    [ "| Node | Duration | Tokens (in/out) | Cost | Retries | Status |"
    , "|------|----------|-----------------|------|---------|--------|"
    ] ++
    map renderAgentRow (Map.toList stats) ++
    [ "" ]


-- | Render a single agent stats row.
renderAgentRow :: (Text, AgentStats) -> Text
renderAgentRow (_, AgentStats{..}) = T.intercalate " | " $
  [ "| " <> asNodeName
  , formatDuration asDurationSeconds
  , T.pack (show asInputTokens) <> " / " <> T.pack (show asOutputTokens)
  , formatCost asCost
  , T.pack (show asRetries)
  , statusCell asSuccess asErrorMessage <> " |"
  ]


-- | Render status cell with optional error.
statusCell :: Bool -> Maybe Text -> Text
statusCell True _ = "Pass"
statusCell False Nothing = "**Fail**"
statusCell False (Just err) = "**Fail**: " <> T.take 50 err


-- | Format duration in human-readable form.
formatDuration :: Double -> Text
formatDuration secs
  | secs < 60 = T.pack (printf "%.1fs" secs)
  | secs < 3600 =
      let mins = floor (secs / 60) :: Int
          remainSecs = secs - fromIntegral mins * 60
      in T.pack (printf "%dm %.0fs" mins remainSecs)
  | otherwise =
      let hours = floor (secs / 3600) :: Int
          remainMins = floor ((secs - fromIntegral hours * 3600) / 60) :: Int
      in T.pack (printf "%dh %dm" hours remainMins)


-- | Format cost in USD.
formatCost :: Double -> Text
formatCost cost = T.pack (printf "$%.4f" cost)
