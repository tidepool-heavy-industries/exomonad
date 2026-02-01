{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module ToolsSpec (spec) where

import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KM
import Data.Char (isAsciiLower)
import Data.List (nub)
import Data.Text qualified as T
import Test.Hspec

import ExoMonad.Guest.Tool.Class (DispatchTools (..), ToolDefinition (..), toMCPFormat)
import ExoMonad.Guest.Tools.Agent (CleanupAgents, ListAgents, SpawnAgents)
import ExoMonad.Guest.Tools.File (ReadFile, WriteFile)
import ExoMonad.Guest.Tools.Git (GitBranch, GitLog, GitStatus)
import ExoMonad.Guest.Tools.GitHub (GitHubGetIssue, GitHubListIssues, GitHubListPRs)

-- | DevTools type list (same as Dev.Tools but defined locally for tests)
type DevTools =
  '[ GitBranch,
     GitStatus,
     GitLog,
     ReadFile,
     WriteFile,
     GitHubGetIssue
   ]

-- | TLTools type list (same as TL.Tools but defined locally for tests)
type TLTools =
  DevTools
    :++ '[ SpawnAgents,
           CleanupAgents,
           ListAgents,
           GitHubListIssues,
           GitHubListPRs
         ]

-- Type-level list append (matches Class.hs)
type family (:++) (xs :: [k]) (ys :: [k]) :: [k] where
  '[] :++ ys = ys
  (x ': xs) :++ ys = x ': (xs :++ ys)

spec :: Spec
spec = do
  describe "DevTools" $ do
    let devTools = toolDefs @DevTools

    it "exports exactly 6 tools" $
      length devTools `shouldBe` 6

    it "has no duplicate tool names" $ do
      let names = map tdName devTools
      nub names `shouldBe` names

    it "all names are lowercase_snake_case" $
      all (T.all isSnakeCaseChar . tdName) devTools `shouldBe` True

    it "all descriptions are non-empty" $
      not (any (T.null . tdDescription) devTools) `shouldBe` True

    it "contains git_branch" $
      "git_branch" `elem` map tdName devTools `shouldBe` True

    it "contains read_file" $
      "read_file" `elem` map tdName devTools `shouldBe` True

    it "does NOT contain spawn_agents" $
      "spawn_agents" `elem` map tdName devTools `shouldBe` False

  describe "TLTools" $ do
    let tlTools = toolDefs @TLTools

    it "exports exactly 11 tools" $
      length tlTools `shouldBe` 11

    it "has no duplicate tool names" $ do
      let names = map tdName tlTools
      nub names `shouldBe` names

    it "contains all DevTools" $ do
      let devNames = map tdName (toolDefs @DevTools)
      let tlNames = map tdName tlTools
      all (`elem` tlNames) devNames `shouldBe` True

    it "contains spawn_agents" $
      "spawn_agents" `elem` map tdName tlTools `shouldBe` True

    it "contains cleanup_agents" $
      "cleanup_agents" `elem` map tdName tlTools `shouldBe` True

    it "contains list_agents" $
      "list_agents" `elem` map tdName tlTools `shouldBe` True

    it "contains github_list_issues" $
      "github_list_issues" `elem` map tdName tlTools `shouldBe` True

    it "contains github_list_prs" $
      "github_list_prs" `elem` map tdName tlTools `shouldBe` True

  describe "toMCPFormat" $ do
    let tlTools = toolDefs @TLTools

    it "produces object with name, description, inputSchema" $ do
      case tlTools of
        (tool : _) -> do
          let json = toMCPFormat tool
          case json of
            Object obj -> do
              KM.member "name" obj `shouldBe` True
              KM.member "description" obj `shouldBe` True
              KM.member "inputSchema" obj `shouldBe` True
            _ -> expectationFailure "Expected JSON object"
        [] -> expectationFailure "tlTools is empty"

    it "name field matches tdName" $ do
      case tlTools of
        (tool : _) -> do
          let json = toMCPFormat tool
          case json of
            Object obj ->
              KM.lookup "name" obj `shouldBe` Just (String (tdName tool))
            _ -> expectationFailure "Expected JSON object"
        [] -> expectationFailure "tlTools is empty"

  describe "schema structure" $ do
    let tlTools = toolDefs @TLTools

    it "all schemas have type: object" $ do
      mapM_
        ( \tool ->
            case tdInputSchema tool of
              Object obj ->
                KM.lookup "type" obj `shouldBe` Just (String "object")
              _ ->
                expectationFailure $
                  "Schema not object: " ++ T.unpack (tdName tool)
        )
        tlTools

    it "all schemas have properties field" $ do
      mapM_
        ( \tool ->
            case tdInputSchema tool of
              Object obj ->
                KM.member "properties" obj `shouldBe` True
              _ ->
                expectationFailure "Schema not object"
        )
        tlTools

  describe "required fields" $ do
    let tlTools = toolDefs @TLTools

    it "read_file requires path" $ do
      let Just tool = findTool "read_file" tlTools
      hasRequired "path" (tdInputSchema tool) `shouldBe` True

    it "github_list_issues requires owner and repo" $ do
      let Just tool = findTool "github_list_issues" tlTools
      hasRequired "owner" (tdInputSchema tool) `shouldBe` True
      hasRequired "repo" (tdInputSchema tool) `shouldBe` True

    it "github_get_issue requires owner, repo, and number" $ do
      let Just tool = findTool "github_get_issue" tlTools
      hasRequired "owner" (tdInputSchema tool) `shouldBe` True
      hasRequired "repo" (tdInputSchema tool) `shouldBe` True
      hasRequired "number" (tdInputSchema tool) `shouldBe` True

    it "spawn_agents requires issues, owner, and repo" $ do
      let Just tool = findTool "spawn_agents" tlTools
      hasRequired "issues" (tdInputSchema tool) `shouldBe` True
      hasRequired "owner" (tdInputSchema tool) `shouldBe` True
      hasRequired "repo" (tdInputSchema tool) `shouldBe` True

    it "cleanup_agents requires issues" $ do
      let Just tool = findTool "cleanup_agents" tlTools
      hasRequired "issues" (tdInputSchema tool) `shouldBe` True

-- Helper: Check if a character is valid for snake_case
isSnakeCaseChar :: Char -> Bool
isSnakeCaseChar c = c == '_' || isAsciiLower c

-- Helper: Find a tool by name
findTool :: T.Text -> [ToolDefinition] -> Maybe ToolDefinition
findTool name tools = case filter ((== name) . tdName) tools of
  [t] -> Just t
  _ -> Nothing

-- Helper: Check if a field is in the required array
hasRequired :: T.Text -> Value -> Bool
hasRequired field (Object obj) =
  case KM.lookup "required" obj of
    Just (Array arr) -> String field `elem` arr
    _ -> False
hasRequired _ _ = False
