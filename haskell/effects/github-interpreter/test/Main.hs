{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Tests for GitHub interpreter JSON parsing.
--
-- These tests verify that we correctly parse the JSON output from gh CLI.
module Main where

import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Test.Hspec

import Tidepool.Effects.GitHub


main :: IO ()
main = hspec $ do
  describe "Issue JSON parsing" $ do
    it "parses a minimal issue" $ do
      let json = LBS.pack $ unlines
            [ "{"
            , "  \"number\": 123,"
            , "  \"title\": \"Test issue\","
            , "  \"body\": \"Issue body\","
            , "  \"author\": { \"login\": \"testuser\", \"name\": \"Test User\" },"
            , "  \"labels\": [],"
            , "  \"state\": \"OPEN\","
            , "  \"url\": \"https://github.com/owner/repo/issues/123\""
            , "}"
            ]
      case eitherDecode json :: Either String Issue of
        Left err -> expectationFailure $ "Failed to parse: " <> err
        Right issue -> do
          issue.issueNumber `shouldBe` 123
          issue.issueTitle `shouldBe` "Test issue"
          issue.issueAuthor.authorLogin `shouldBe` "testuser"
          issue.issueState `shouldBe` IssueOpen

    it "parses issue with labels" $ do
      let json = LBS.pack $ unlines
            [ "{"
            , "  \"number\": 456,"
            , "  \"title\": \"Bug report\","
            , "  \"body\": \"Something broke\","
            , "  \"author\": { \"login\": \"reporter\" },"
            , "  \"labels\": ["
            , "    { \"name\": \"bug\", \"color\": \"d73a4a\" },"
            , "    { \"name\": \"priority:high\", \"color\": \"ff0000\" }"
            , "  ],"
            , "  \"state\": \"CLOSED\","
            , "  \"url\": \"https://github.com/owner/repo/issues/456\""
            , "}"
            ]
      case eitherDecode json :: Either String Issue of
        Left err -> expectationFailure $ "Failed to parse: " <> err
        Right issue -> do
          issue.issueLabels `shouldBe` ["bug", "priority:high"]
          issue.issueState `shouldBe` IssueClosed

    it "parses issue with comments" $ do
      let json = LBS.pack $ unlines
            [ "{"
            , "  \"number\": 789,"
            , "  \"title\": \"With comments\","
            , "  \"body\": \"Body\","
            , "  \"author\": { \"login\": \"author\" },"
            , "  \"labels\": [],"
            , "  \"state\": \"OPEN\","
            , "  \"url\": \"https://github.com/owner/repo/issues/789\","
            , "  \"comments\": ["
            , "    {"
            , "      \"author\": { \"login\": \"commenter\" },"
            , "      \"body\": \"First comment\","
            , "      \"createdAt\": \"2024-01-15T10:30:00Z\""
            , "    }"
            , "  ]"
            , "}"
            ]
      case eitherDecode json :: Either String Issue of
        Left err -> expectationFailure $ "Failed to parse: " <> err
        Right issue -> do
          length issue.issueComments `shouldBe` 1
          (head issue.issueComments).commentAuthor.authorLogin `shouldBe` "commenter"

  describe "PullRequest JSON parsing" $ do
    it "parses a minimal PR" $ do
      let json = LBS.pack $ unlines
            [ "{"
            , "  \"number\": 100,"
            , "  \"title\": \"Add feature\","
            , "  \"body\": \"PR body\","
            , "  \"author\": { \"login\": \"contributor\" },"
            , "  \"labels\": [],"
            , "  \"state\": \"OPEN\","
            , "  \"url\": \"https://github.com/owner/repo/pull/100\","
            , "  \"headRefName\": \"feature-branch\","
            , "  \"baseRefName\": \"main\""
            , "}"
            ]
      case eitherDecode json :: Either String PullRequest of
        Left err -> expectationFailure $ "Failed to parse: " <> err
        Right pr -> do
          pr.prNumber `shouldBe` 100
          pr.prHeadRefName `shouldBe` "feature-branch"
          pr.prBaseRefName `shouldBe` "main"
          pr.prState `shouldBe` PROpen

    it "parses PR with reviews" $ do
      let json = LBS.pack $ unlines
            [ "{"
            , "  \"number\": 200,"
            , "  \"title\": \"Reviewed PR\","
            , "  \"body\": \"Body\","
            , "  \"author\": { \"login\": \"author\" },"
            , "  \"labels\": [],"
            , "  \"state\": \"MERGED\","
            , "  \"url\": \"https://github.com/owner/repo/pull/200\","
            , "  \"headRefName\": \"fix\","
            , "  \"baseRefName\": \"main\","
            , "  \"reviews\": ["
            , "    {"
            , "      \"author\": { \"login\": \"reviewer\" },"
            , "      \"body\": \"LGTM\","
            , "      \"state\": \"APPROVED\""
            , "    }"
            , "  ]"
            , "}"
            ]
      case eitherDecode json :: Either String PullRequest of
        Left err -> expectationFailure $ "Failed to parse: " <> err
        Right pr -> do
          pr.prState `shouldBe` PRMerged
          length pr.prReviews `shouldBe` 1
          (head pr.prReviews).reviewState `shouldBe` ReviewApproved

  describe "PRCreateSpec JSON parsing" $ do
    it "parses a full spec" $ do
      let json = LBS.pack $ unlines
            [ "{"
            , "  \"prcsRepo\": \"owner/repo\","
            , "  \"prcsHead\": \"feature\","
            , "  \"prcsBase\": \"main\","
            , "  \"prcsTitle\": \"Fix bug\","
            , "  \"prcsBody\": \"Fixes the issue\""
            , "}"
            ]
      case eitherDecode json :: Either String PRCreateSpec of
        Left err -> expectationFailure $ "Failed to parse: " <> err
        Right spec -> do
          spec.prcsRepo `shouldBe` Repo "owner/repo"
          spec.prcsHead `shouldBe` "feature"
          spec.prcsBase `shouldBe` "main"
          spec.prcsTitle `shouldBe` "Fix bug"
          spec.prcsBody `shouldBe` "Fixes the issue"

  describe "PRUrl JSON parsing" $ do
    it "parses a PR URL" $ do
      let json = "\"https://github.com/owner/repo/pull/1\""
      case eitherDecode json :: Either String PRUrl of
        Left err -> expectationFailure $ "Failed to parse: " <> err
        Right (PRUrl url) -> url `shouldBe` "https://github.com/owner/repo/pull/1"

  describe "Author JSON parsing" $ do
    it "parses author with name" $ do
      let json = "{ \"login\": \"user\", \"name\": \"Full Name\" }"
      case eitherDecode json :: Either String Author of
        Left err -> expectationFailure $ "Failed to parse: " <> err
        Right author -> do
          author.authorLogin `shouldBe` "user"
          author.authorName `shouldBe` Just "Full Name"

    it "parses author without name" $ do
      let json = "{ \"login\": \"bot\" }"
      case eitherDecode json :: Either String Author of
        Left err -> expectationFailure $ "Failed to parse: " <> err
        Right author -> do
          author.authorLogin `shouldBe` "bot"
          author.authorName `shouldBe` Nothing

  describe "ReviewComment JSON parsing" $ do
    it "parses GitHub API format" $ do
      let json = LBS.pack $ unlines
            [ "{"
            , "  \"user\": { \"login\": \"github-actions\" },"
            , "  \"body\": \"Consider using type X instead of type Y\","
            , "  \"path\": \"src/Main.hs\","
            , "  \"line\": 42,"
            , "  \"state\": \"COMMENTED\","
            , "  \"created_at\": \"2024-01-15T10:30:00Z\""
            , "}"
            ]
      case eitherDecode json :: Either String ReviewComment of
        Left err -> expectationFailure $ "Failed to parse: " <> err
        Right comment -> do
          comment.rcAuthor `shouldBe` "github-actions"
          comment.rcBody `shouldBe` "Consider using type X instead of type Y"
          comment.rcPath `shouldBe` Just "src/Main.hs"
          comment.rcLine `shouldBe` Just 42
          comment.rcState `shouldBe` ReviewCommented

    it "parses without optional fields" $ do
      let json = LBS.pack $ unlines
            [ "{"
            , "  \"user\": { \"login\": \"copilot\" },"
            , "  \"body\": \"Suggestion\","
            , "  \"state\": \"CHANGES_REQUESTED\","
            , "  \"created_at\": \"2024-01-15T11:00:00Z\""
            , "}"
            ]
      case eitherDecode json :: Either String ReviewComment of
        Left err -> expectationFailure $ "Failed to parse: " <> err
        Right comment -> do
          comment.rcAuthor `shouldBe` "copilot"
          comment.rcPath `shouldBe` Nothing
          comment.rcLine `shouldBe` Nothing
          comment.rcState `shouldBe` ReviewChangesRequested

    it "uses default state when missing" $ do
      let json = LBS.pack $ unlines
            [ "{"
            , "  \"user\": { \"login\": \"reviewer\" },"
            , "  \"body\": \"LGTM\","
            , "  \"created_at\": \"2024-01-15T12:00:00Z\""
            , "}"
            ]
      case eitherDecode json :: Either String ReviewComment of
        Left err -> expectationFailure $ "Failed to parse: " <> err
        Right comment -> do
          comment.rcState `shouldBe` ReviewCommented  -- Default when missing
