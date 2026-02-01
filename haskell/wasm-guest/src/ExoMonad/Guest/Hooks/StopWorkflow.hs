{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExoMonad.Guest.Hooks.StopWorkflow
  ( handleSubagentStop,
    stopWorkflowHandler,
  )
where

import Control.Monad (when)
import Control.Monad.Freer
import Control.Monad.Writer (Writer)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import ExoMonad.Guest.Effects.Cabal
import ExoMonad.Guest.Effects.FileSystem (runFileSystem) -- For completeness if needed later
import ExoMonad.Guest.Effects.Git
import ExoMonad.Guest.Effects.GitHub
import ExoMonad.Guest.Effects.WorkflowState
import ExoMonad.Guest.Types
import Extism.PDK (input, output)
import Foreign.C.Types (CInt (..))
import Text.Ginger
import Text.Ginger.Html (htmlSource)
import Text.Ginger.TH.QuasiQuote (jinja)

-- | Configuration
maxAttempts :: Int
maxAttempts = 10

renderTpl :: Text -> HashMap.HashMap Text (GVal (Run SourcePos (Writer Text) Text)) -> Text
renderTpl tplSource context =
  case parseGinger (const $ return Nothing) Nothing (T.unpack tplSource) of
    Left err -> T.pack $ "Template error: " ++ show err
    Right tpl -> 
      let lookupFn key = HashMap.lookupDefault (toGVal ()) key context
          ctx = makeContextText lookupFn
      in runGinger ctx tpl

fixBuildErrorsTpl :: Text
fixBuildErrorsTpl = [jinja|
The build is failing with {{ errors | length }} error(s):

{% for error in errors %}
## {{ error.file }}:{{ error.line }}
{{ error.message }}

{% endfor %}

Please fix these build errors before continuing.
|]

fixTestFailuresTpl :: Text
fixTestFailuresTpl = [jinja|
{{ failures | length }} test(s) failed:

{% for failure in failures %}
- {{ failure.name }}: {{ failure.message }}
{% endfor %}

Please fix these test failures.
|]

writeTestsTpl :: Text
writeTestsTpl = [jinja|
You've modified the following files but haven't added tests:

{% for file in changed_files %}
- {{ file }}
{% endfor %}

Please write tests covering the new functionality.
|]

createPRTpl :: Text
createPRTpl = [jinja|
All tests pass. Please create a pull request:

```bash
gh pr create --title "{{ suggested_title }}" --body "{{ suggested_body }}"
```

Summary of changes:
{{ change_summary }}
|]

-- | Helpers
continue :: Text -> HookOutput
continue msg =
  HookOutput
    { continue_ = False,
      stopReason = Nothing,
      suppressOutput = Nothing,
      systemMessage = Just msg,
      hookSpecificOutput = Nothing
    }

allow :: Text -> HookOutput
allow msg =
  HookOutput
    { continue_ = True,
      stopReason = Just msg,
      suppressOutput = Nothing,
      systemMessage = Nothing,
      hookSpecificOutput = Nothing
    }

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x

stopWorkflowHandler :: IO CInt
stopWorkflowHandler = do
  inp <- input @ByteString
  case Aeson.eitherDecodeStrict inp of
    Left err -> do
      let errResp = Aeson.object ["error" Aeson..= ("Parse error: " ++ err)]
      output (BSL.toStrict $ Aeson.encode errResp)
      pure 1
    Right (hookInput :: HookInput) -> do
      if hiHookEventName hookInput == "SubagentStop"
        then do
          result <- runM 
                  $ runWorkflowState 
                  $ runGitHub 
                  $ runGit 
                  $ runCabal 
                  $ handleSubagentStop hookInput
          output (BSL.toStrict $ Aeson.encode result)
          pure 0
        else do
          let resp = allowResponse Nothing
          output (BSL.toStrict $ Aeson.encode resp)
          pure 0

-- | Main logic
handleSubagentStop ::
  ( Member Cabal effs,
    Member Git effs,
    Member GitHub effs,
    Member WorkflowState effs
  ) =>
  HookInput ->
  Eff effs HookOutput
handleSubagentStop input = do
  let agentId = hiSessionId input
  
  -- 1. Loop Guard
  attempts <- either (const 0) id <$> getAttempts agentId
  when (attempts > maxAttempts) $
    pure $ allow "Max attempts reached, stopping"

  -- Define context variables
  let workingDir = "." 
  let containerId = agentId -- Use session ID as container ID

  -- 2. Check Build
  buildRes <- cabalBuild workingDir containerId
  case buildRes of
    Left err -> pure $ allow ("System error checking build: " <> err)
    Right (BuildFailed errors) -> do
      _ <- incrementAttempts agentId
      let ctx = HashMap.fromList [("errors" :: Text, toGVal errors)]
      pure $ continue $ renderTpl fixBuildErrorsTpl ctx
    Right BuildSuccess -> do
      -- 3. Check Tests
      testRes <- cabalTest workingDir containerId
      case testRes of
        Left err -> pure $ allow ("System error checking tests: " <> err)
        Right (TestsFailed failures) -> do
          _ <- incrementAttempts agentId
          let ctx = HashMap.fromList [("failures" :: Text, toGVal failures)]
          pure $ continue $ renderTpl fixTestFailuresTpl ctx
        Right NoTests -> do
           -- Check if files changed
           diffRes <- gitDiff workingDir containerId
           case diffRes of
             Left err -> pure $ allow ("System error checking git: " <> err)
             Right changedFiles -> do
               let sourceChanged = any (\f -> ".hs" `T.isSuffixOf` f) changedFiles
               if sourceChanged
                 then do
                   let ctx = HashMap.fromList [("changed_files" :: Text, toGVal changedFiles)]
                   pure $ continue $ renderTpl writeTestsTpl ctx
                 else 
                   checkPR workingDir containerId input
        Right (TestsPassed _) -> do
           checkPR workingDir containerId input

checkPR :: 
  ( Member GitHub effs,
    Member Git effs
  ) => 
  Text -> Text -> HookInput -> Eff effs HookOutput
checkPR workingDir containerId input = do
  -- 4. Check PR
  branchRes <- gitBranch workingDir containerId
  case branchRes of
    Left err -> pure $ allow ("System error checking branch: " <> err)
    Right branch -> do
      -- Get Remote URL to parse Owner/Repo
      remoteRes <- gitRemoteUrl workingDir containerId "origin"
      let repo = case remoteRes of
                   Right url -> case parseRepoFromUrl url of
                                  Just r -> r
                                  Nothing -> Repo "owner" "repo" -- Fallback
                   Left _ -> Repo "owner" "repo" -- Fallback

      prsRes <- ghListPRs repo (Just $ PRFilter (Just "open") (Just 1))
      
      case prsRes of
        Left err -> pure $ allow ("System error listing PRs: " <> err)
        Right prs -> do
          let myPr = headMaybe $ filter (\p -> prHeadRef p == branch) prs
          case myPr of
            Nothing -> do
              let ctx = HashMap.fromList 
                    [ ("suggested_title" :: Text, toGVal ("Fixes for " <> branch))
                    , ("suggested_body" :: Text, toGVal "Implemented changes.")
                    , ("change_summary" :: Text, toGVal "TODO: Git log summary")
                    ]
              pure $ continue $ renderTpl createPRTpl ctx
            Just pr -> do
               pure $ allow "PR ready for review"

parseRepoFromUrl :: Text -> Maybe Repo
parseRepoFromUrl url = 
  let parts = T.splitOn "/" (T.replace ".git" "" url)
  in case reverse parts of
       (name : owner : _) -> Just $ Repo (parseOwner owner) name
       _ -> Nothing
  where
    parseOwner o = if ":" `T.isInfixOf` o then last (T.splitOn ":" o) else o