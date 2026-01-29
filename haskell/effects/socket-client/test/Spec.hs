{-# LANGUAGE OverloadedStrings #-}

-- | Serialization roundtrip tests for the socket client protocol types.
--
-- These tests verify that:
-- 1. ServiceRequest encodes to JSON that Rust can decode
-- 2. ServiceResponse decodes from JSON that Rust produces
-- 3. The wire format field names match exactly across both languages
--
-- No network or GitHub API calls are made.
module Main where

import Data.Aeson (Value(..), decode, encode, object, (.=))
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import ExoMonad.Effects.SocketClient

main :: IO ()
main = hspec $ do
  -- =========================================================================
  -- ServiceRequest encoding (Haskell → JSON → Rust)
  -- =========================================================================
  describe "ServiceRequest encoding" $ do
    it "encodes GitHubGetIssue with type tag and include_comments" $ do
      let req = GitHubGetIssue $ GitHubGetIssueReq "octocat" "hello-world" 42 True
          json = decodeToObject req
      lookupField json "type" `shouldBe` Just (String "GitHubGetIssue")
      lookupField json "owner" `shouldBe` Just (String "octocat")
      lookupField json "repo" `shouldBe` Just (String "hello-world")
      lookupField json "number" `shouldBe` Just (Number 42)
      lookupField json "include_comments" `shouldBe` Just (Bool True)

    it "encodes GitHubGetIssue with include_comments=false" $ do
      let req = GitHubGetIssue $ GitHubGetIssueReq "o" "r" 1 False
          json = decodeToObject req
      lookupField json "include_comments" `shouldBe` Just (Bool False)

    it "encodes GitHubGetPR with type tag and include_details" $ do
      let req = GitHubGetPR $ GitHubGetPRReq "octocat" "hello-world" 99 True
          json = decodeToObject req
      lookupField json "type" `shouldBe` Just (String "GitHubGetPR")
      lookupField json "owner" `shouldBe` Just (String "octocat")
      lookupField json "number" `shouldBe` Just (Number 99)
      lookupField json "include_details" `shouldBe` Just (Bool True)

    it "encodes GitHubCreateIssue with all fields" $ do
      let req = GitHubCreateIssue $ GitHubCreateIssueReq "octocat" "repo" "Bug" "Details" ["bug", "critical"]
          json = decodeToObject req
      lookupField json "type" `shouldBe` Just (String "GitHubCreateIssue")
      lookupField json "title" `shouldBe` Just (String "Bug")
      lookupField json "body" `shouldBe` Just (String "Details")

    it "encodes GitHubCreatePR with head and base" $ do
      let req = GitHubCreatePR $ GitHubCreatePRReq "octocat" "repo" "Add feature" "Body" "feature" "main"
          json = decodeToObject req
      lookupField json "type" `shouldBe` Just (String "GitHubCreatePR")
      lookupField json "head" `shouldBe` Just (String "feature")
      lookupField json "base" `shouldBe` Just (String "main")

    it "encodes GitHubListIssues with labels" $ do
      let req = GitHubListIssues $ GitHubListIssuesReq "o" "r" (Just "open") ["bug"]
          json = decodeToObject req
      lookupField json "type" `shouldBe` Just (String "GitHubListIssues")
      lookupField json "state" `shouldBe` Just (String "open")

    it "encodes GitHubCheckAuth" $ do
      let req = GitHubCheckAuth
          json = decodeToObject req
      lookupField json "type" `shouldBe` Just (String "GitHubCheckAuth")

  -- =========================================================================
  -- ServiceResponse decoding (Rust → JSON → Haskell)
  -- =========================================================================
  describe "ServiceResponse decoding" $ do
    it "decodes GitHubIssueResponse with all fields" $ do
      let json = object
            [ "type" .= ("GitHubIssueResponse" :: Text)
            , "number" .= (42 :: Int)
            , "title" .= ("Fix the bug" :: Text)
            , "body" .= ("It's broken" :: Text)
            , "state" .= ("open" :: Text)
            , "labels" .= (["bug", "critical"] :: [Text])
            , "url" .= ("https://github.com/octocat/repo/issues/42" :: Text)
            , "author" .= ("octocat" :: Text)
            , "comments" .=
                [ object
                    [ "author" .= ("reviewer" :: Text)
                    , "body" .= ("Looks good" :: Text)
                    , "created_at" .= ("2024-01-15T10:00:00Z" :: Text)
                    , "replies" .= ([] :: [Value])
                    ]
                ]
            ]
      case decode (encode json) :: Maybe ServiceResponse of
        Nothing -> expectationFailure "Failed to decode GitHubIssueResponse"
        Just (GitHubIssueResponse n t b s ls u a cs) -> do
          n `shouldBe` 42
          t `shouldBe` "Fix the bug"
          b `shouldBe` "It's broken"
          s `shouldBe` "open"
          ls `shouldBe` ["bug", "critical"]
          u `shouldBe` "https://github.com/octocat/repo/issues/42"
          a `shouldBe` "octocat"
          length cs `shouldBe` 1
        Just other -> expectationFailure $ "Wrong variant: " <> show other

    it "decodes GitHubIssueResponse with missing optional comments" $ do
      let json = object
            [ "type" .= ("GitHubIssueResponse" :: Text)
            , "number" .= (1 :: Int)
            , "title" .= ("t" :: Text)
            , "body" .= ("b" :: Text)
            , "state" .= ("open" :: Text)
            , "labels" .= ([] :: [Text])
            , "url" .= ("u" :: Text)
            , "author" .= ("a" :: Text)
            ]
      case decode (encode json) :: Maybe ServiceResponse of
        Nothing -> expectationFailure "Failed to decode GitHubIssueResponse without comments"
        Just (GitHubIssueResponse _ _ _ _ _ _ _ cs) ->
          cs `shouldBe` []
        Just other -> expectationFailure $ "Wrong variant: " <> show other

    it "decodes GitHubPRResponse with all fields" $ do
      let json = object
            [ "type" .= ("GitHubPRResponse" :: Text)
            , "number" .= (99 :: Int)
            , "title" .= ("Add feature" :: Text)
            , "body" .= ("This adds X" :: Text)
            , "author" .= ("octocat" :: Text)
            , "url" .= ("https://github.com/octocat/repo/pull/99" :: Text)
            , "state" .= ("open" :: Text)
            , "head_ref_name" .= ("feature-branch" :: Text)
            , "base_ref_name" .= ("main" :: Text)
            , "created_at" .= ("2024-01-15T10:00:00Z" :: Text)
            , "merged_at" .= ("2024-01-16T12:00:00Z" :: Text)
            , "labels" .= (["enhancement"] :: [Text])
            , "comments" .=
                [ object
                    [ "author" .= ("reviewer" :: Text)
                    , "body" .= ("LGTM" :: Text)
                    , "created_at" .= ("2024-01-15T11:00:00Z" :: Text)
                    , "replies" .= ([] :: [Value])
                    ]
                ]
            , "reviews" .=
                [ object
                    [ "author" .= ("reviewer" :: Text)
                    , "body" .= ("Approved" :: Text)
                    , "path" .= ("src/main.rs" :: Text)
                    , "line" .= (42 :: Int)
                    , "state" .= ("APPROVED" :: Text)
                    , "created_at" .= ("2024-01-15T12:00:00Z" :: Text)
                    ]
                ]
            ]
      case decode (encode json) :: Maybe ServiceResponse of
        Nothing -> expectationFailure "Failed to decode GitHubPRResponse"
        Just (GitHubPRResponse n t b a u s h ba c ma ls cs rs) -> do
          n `shouldBe` 99
          t `shouldBe` "Add feature"
          b `shouldBe` "This adds X"
          a `shouldBe` "octocat"
          s `shouldBe` "open"
          h `shouldBe` "feature-branch"
          ba `shouldBe` "main"
          c `shouldBe` "2024-01-15T10:00:00Z"
          ma `shouldBe` Just "2024-01-16T12:00:00Z"
          ls `shouldBe` ["enhancement"]
          length cs `shouldBe` 1
          length rs `shouldBe` 1
        Just other -> expectationFailure $ "Wrong variant: " <> show other

    it "decodes GitHubPRResponse with missing optional fields" $ do
      let json = object
            [ "type" .= ("GitHubPRResponse" :: Text)
            , "number" .= (1 :: Int)
            , "title" .= ("t" :: Text)
            , "body" .= ("b" :: Text)
            , "author" .= ("a" :: Text)
            , "url" .= ("u" :: Text)
            , "state" .= ("open" :: Text)
            , "head_ref_name" .= ("h" :: Text)
            , "base_ref_name" .= ("main" :: Text)
            , "created_at" .= ("2024-01-01T00:00:00Z" :: Text)
            , "labels" .= ([] :: [Text])
            ]
      case decode (encode json) :: Maybe ServiceResponse of
        Nothing -> expectationFailure "Failed to decode GitHubPRResponse without optionals"
        Just (GitHubPRResponse _ _ _ _ _ _ _ _ _ ma ls cs rs) -> do
          ma `shouldBe` Nothing
          ls `shouldBe` []
          cs `shouldBe` []
          rs `shouldBe` []
        Just other -> expectationFailure $ "Wrong variant: " <> show other

    it "decodes GitHubAuthResponse" $ do
      let json = object
            [ "type" .= ("GitHubAuthResponse" :: Text)
            , "authenticated" .= True
            , "user" .= ("octocat" :: Text)
            ]
      case decode (encode json) :: Maybe ServiceResponse of
        Nothing -> expectationFailure "Failed to decode GitHubAuthResponse"
        Just (GitHubAuthResponse auth user) -> do
          auth `shouldBe` True
          user `shouldBe` Just "octocat"
        Just other -> expectationFailure $ "Wrong variant: " <> show other

    it "decodes GitHubIssuesResponse" $ do
      let json = object
            [ "type" .= ("GitHubIssuesResponse" :: Text)
            , "issues" .=
                [ object [ "number" .= (1 :: Int), "title" .= ("Bug" :: Text), "state" .= ("open" :: Text) ]
                , object [ "number" .= (2 :: Int), "title" .= ("Feature" :: Text), "state" .= ("closed" :: Text) ]
                ]
            ]
      case decode (encode json) :: Maybe ServiceResponse of
        Nothing -> expectationFailure "Failed to decode GitHubIssuesResponse"
        Just (GitHubIssuesResponse issues) ->
          length issues `shouldBe` 2
        Just other -> expectationFailure $ "Wrong variant: " <> show other

    it "decodes GitHubReviewsResponse" $ do
      let json = object
            [ "type" .= ("GitHubReviewsResponse" :: Text)
            , "reviews" .=
                [ object
                    [ "author" .= ("reviewer" :: Text)
                    , "body" .= ("Changes needed" :: Text)
                    , "path" .= ("lib.rs" :: Text)
                    , "state" .= ("CHANGES_REQUESTED" :: Text)
                    , "created_at" .= ("2024-01-15T10:00:00Z" :: Text)
                    ]
                ]
            ]
      case decode (encode json) :: Maybe ServiceResponse of
        Nothing -> expectationFailure "Failed to decode GitHubReviewsResponse"
        Just (GitHubReviewsResponse reviews) ->
          length reviews `shouldBe` 1
        Just other -> expectationFailure $ "Wrong variant: " <> show other

    it "decodes GitHubDiscussionResponse" $ do
      let json = object
            [ "type" .= ("GitHubDiscussionResponse" :: Text)
            , "number" .= (10 :: Int)
            , "title" .= ("RFC" :: Text)
            , "body" .= ("Proposal" :: Text)
            , "author" .= ("octocat" :: Text)
            , "url" .= ("https://github.com/octocat/repo/discussions/10" :: Text)
            , "comments" .=
                [ object
                    [ "author" .= ("commenter" :: Text)
                    , "body" .= ("Great idea" :: Text)
                    , "created_at" .= ("2024-01-15T10:00:00Z" :: Text)
                    , "replies" .= ([] :: [Value])
                    ]
                ]
            ]
      case decode (encode json) :: Maybe ServiceResponse of
        Nothing -> expectationFailure "Failed to decode GitHubDiscussionResponse"
        Just (GitHubDiscussionResponse n t b a u cs) -> do
          n `shouldBe` 10
          t `shouldBe` "RFC"
          a `shouldBe` "octocat"
          length cs `shouldBe` 1
        Just other -> expectationFailure $ "Wrong variant: " <> show other

    it "decodes ErrorResponse" $ do
      let json = object
            [ "type" .= ("ErrorResponse" :: Text)
            , "code" .= (404 :: Int)
            , "message" .= ("Not found" :: Text)
            ]
      case decode (encode json) :: Maybe ServiceResponse of
        Nothing -> expectationFailure "Failed to decode ErrorResponse"
        Just (ErrorResponse code msg) -> do
          code `shouldBe` 404
          msg `shouldBe` "Not found"
        Just other -> expectationFailure $ "Wrong variant: " <> show other

    it "decodes OtelAckResponse" $ do
      let json = object [ "type" .= ("OtelAckResponse" :: Text) ]
      case decode (encode json) :: Maybe ServiceResponse of
        Nothing -> expectationFailure "Failed to decode OtelAckResponse"
        Just OtelAckResponse -> pure ()
        Just other -> expectationFailure $ "Wrong variant: " <> show other

  -- =========================================================================
  -- Cross-language wire format contract tests
  -- =========================================================================
  describe "Cross-language wire format" $ do
    it "GitHubGetIssue request uses snake_case include_comments" $ do
      let req = GitHubGetIssue "o" "r" 1 True
          bs = encode req
          jsonStr = LBS8.unpack bs
      jsonStr `shouldContain` "\"include_comments\""

    it "GitHubGetPR request uses snake_case include_details" $ do
      let req = GitHubGetPR "o" "r" 1 True
          bs = encode req
          jsonStr = LBS8.unpack bs
      jsonStr `shouldContain` "\"include_details\""

    it "ServiceRequest roundtrip preserves type tag" $ do
      let requests :: [(Text, LBS8.ByteString)]
          requests =
            [ ("GitHubGetIssue", encode $ GitHubGetIssue $ GitHubGetIssueReq "o" "r" 1 False)
            , ("GitHubCreateIssue", encode $ GitHubCreateIssue $ GitHubCreateIssueReq "o" "r" "t" "b" [])
            , ("GitHubGetPR", encode $ GitHubGetPR $ GitHubGetPRReq "o" "r" 1 False)
            , ("GitHubCreatePR", encode $ GitHubCreatePR $ GitHubCreatePRReq "o" "r" "t" "b" "h" "main")
            , ("GitHubListIssues", encode $ GitHubListIssues $ GitHubListIssuesReq "o" "r" Nothing [])
            , ("GitHubCheckAuth", encode GitHubCheckAuth)
            ]
      mapM_ (\(expected, bs) ->
        case decode bs :: Maybe Value of
          Nothing -> expectationFailure $ "Failed to decode " <> T.unpack expected
          Just (Object o) ->
            case KM.lookup "type" o of
              Just (String t) -> t `shouldBe` expected
              _ -> expectationFailure $ "Missing type tag in " <> T.unpack expected
          _ -> expectationFailure $ "Not an object: " <> T.unpack expected
        ) requests

-- | Helper: encode a ServiceRequest and decode as a raw JSON object.
decodeToObject :: ServiceRequest -> Maybe Value
decodeToObject = decode . encode

-- | Helper: look up a field in a decoded JSON Value.
lookupField :: Maybe Value -> Text -> Maybe Value
lookupField (Just (Object o)) key = KM.lookup (fromText key) o
lookupField _ _ = Nothing
