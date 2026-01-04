-- | Issue filing effect executor - GitHub API client.
--
-- Implements Issue effect by POSTing to GitHub Issues API.
-- Fire-and-forget: resident doesn't block on response.
module Tidepool.Issue.Executor
  ( -- * Executor
    runIssue
  , runIssueLogging

    -- * Configuration
  , IssueConfig(..)
  , defaultLabels

    -- * GitHub API (for IO execution)
  , createGitHubIssue
  , createIssueManager
  , CreateIssueRequest(..)
  ) where

import Control.Monad.Freer (Eff, interpret)
import Data.Aeson (ToJSON(..), object, (.=), encode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Client (Manager, Request(..), RequestBody(..), Response(..), parseRequest, httpLbs)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Header (hAuthorization, hContentType, hAccept, hUserAgent)
import Network.HTTP.Types.Status (statusIsSuccessful)
import GHC.Generics (Generic)

import Tidepool.Effects.Issue (Issue(..), IssueReport(..), IssueCategory(..))
import Tidepool.Issue.Template (renderIssueBody, issueTitle)


-- ════════════════════════════════════════════════════════════════════════════
-- CONFIGURATION
-- ════════════════════════════════════════════════════════════════════════════

-- | GitHub API configuration for issue filing.
data IssueConfig = IssueConfig
  { icOwner :: Text       -- ^ Repository owner (user or org)
  , icRepo :: Text        -- ^ Repository name
  , icToken :: Text       -- ^ GitHub personal access token (needs repo scope)
  , icManager :: Manager  -- ^ HTTP manager (for connection reuse)
  }
  deriving (Generic)

-- | Default labels to apply based on issue category.
--
-- These map resident-facing categories to GitHub labels.
-- Sleeptime can evolve this mapping based on resolution correlation.
defaultLabels :: IssueCategory -> [Text]
defaultLabels = \case
  ParseFailure       -> ["bug", "llm-output", "tidepool-resident"]
  EffectMissing      -> ["enhancement", "effect", "tidepool-resident"]
  EffectFailing      -> ["bug", "effect", "tidepool-resident"]
  UnexpectedState    -> ["bug", "state", "tidepool-resident"]
  PerformanceDegraded -> ["performance", "tidepool-resident"]
  IssueOther         -> ["tidepool-resident"]


-- ════════════════════════════════════════════════════════════════════════════
-- EXECUTOR
-- ════════════════════════════════════════════════════════════════════════════

-- | Run Issue effects with a pure handler.
--
-- This is the main interpreter that takes a handler function.
runIssue
  :: (IssueReport -> Eff effs ())  -- ^ Handler for filing issues
  -> Eff (Issue ': effs) a
  -> Eff effs a
runIssue handler = interpret $ \case
  FileIssue report -> handler report


-- | Run Issue effects by logging them (useful for testing/debugging).
--
-- Returns the reports that would have been filed along with the result.
runIssueLogging
  :: Eff (Issue ': effs) a
  -> Eff effs (a, [IssueReport])
runIssueLogging action = do
  -- This is a stub - proper implementation would use State effect
  -- For now, we just run the action and discard the reports
  result <- runIssue (const $ pure ()) action
  pure (result, [])


-- ════════════════════════════════════════════════════════════════════════════
-- GITHUB API
-- ════════════════════════════════════════════════════════════════════════════

-- | GitHub issue creation request body.
data CreateIssueRequest = CreateIssueRequest
  { cirTitle :: Text
  , cirBody :: Text
  , cirLabels :: [Text]
  }
  deriving (Show, Eq, Generic)

instance ToJSON CreateIssueRequest where
  toJSON r = object
    [ "title" .= r.cirTitle
    , "body" .= r.cirBody
    , "labels" .= r.cirLabels
    ]


-- | Create a GitHub issue from an IssueReport.
--
-- Returns the issue URL on success, or error message on failure.
-- This is a raw IO function - wrap it in your effect handler.
createGitHubIssue
  :: IssueConfig
  -> IssueReport
  -> IO (Either Text Text)
createGitHubIssue config report = do
  let url = "https://api.github.com/repos/"
          <> T.unpack config.icOwner <> "/"
          <> T.unpack config.icRepo <> "/issues"

  let reqBody = CreateIssueRequest
        { cirTitle = issueTitle report
        , cirBody = renderIssueBody report
        , cirLabels = defaultLabels report.irCategory
        }

  -- Build request
  case parseRequest url of
    Nothing -> pure $ Left $ "Invalid GitHub API URL: " <> T.pack url
    Just baseReq -> do
      let req = baseReq
            { method = "POST"
            , requestHeaders =
                [ (hAuthorization, "Bearer " <> encodeUtf8 config.icToken)
                , (hContentType, "application/json")
                , (hAccept, "application/vnd.github+json")
                , (hUserAgent, "tidepool-issue-executor")
                , ("X-GitHub-Api-Version", "2022-11-28")
                ]
            , requestBody = RequestBodyLBS $ encode reqBody
            }

      -- Fire request
      resp <- httpLbs req config.icManager
      if statusIsSuccessful (responseStatus resp)
        then pure $ Right $ "Issue created successfully"
        else pure $ Left $ "GitHub API error: " <> T.pack (show $ responseStatus resp)

  where
    encodeUtf8 :: Text -> ByteString
    encodeUtf8 = TE.encodeUtf8


-- | Create a fresh TLS manager for issue filing.
--
-- Call once at startup and reuse the manager.
createIssueManager :: IO Manager
createIssueManager = newTlsManager
