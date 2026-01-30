{-# LANGUAGE DeriveAnyClass #-}

-- | Hook policy types and evaluation logic.
module ExoMonad.Control.Hook.Policy
  ( HookDecision (..),
    HookRule (..),
    HookPolicy (..),
    evaluatePolicy,
    defaultPolicy,
    loadHookPolicy,
  )
where

import Data.Aeson (FromJSON, ToJSON, Value, eitherDecode)
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

-- | Decision for a PreToolUse hook.
data HookDecision
  = -- | Allow with optional reason and modified input
    PolicyAllow (Maybe Text) (Maybe Value)
  | -- | Deny with required reason
    PolicyDeny Text
  | -- | Ask user with optional reason
    PolicyAsk (Maybe Text)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | A single rule mapping a tool pattern to a decision.
data HookRule = HookRule
  { -- | Tool name or pattern (currently literal match)
    toolPattern :: Text,
    -- | Decision to apply if tool matches
    decision :: HookDecision
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | A complete hook policy.
data HookPolicy = HookPolicy
  { -- | List of rules (first match wins)
    rules :: [HookRule],
    -- | Decision if no rules match
    defaultDecision :: HookDecision
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Default policy: allow everything.
defaultPolicy :: HookPolicy
defaultPolicy =
  HookPolicy
    { rules = [],
      defaultDecision = PolicyAllow (Just "Allowed by default policy") Nothing
    }

-- | Evaluate a policy for a given tool name.
evaluatePolicy :: HookPolicy -> Text -> HookDecision
evaluatePolicy policy toolName =
  case filter (matchRule toolName) policy.rules of
    (rule : _) -> rule.decision
    [] -> policy.defaultDecision

-- | Load policy from file or return default.
loadHookPolicy :: FilePath -> IO HookPolicy
loadHookPolicy projectDir = do
  let policyPath = projectDir </> ".exomonad" </> "hook-policy.json"
  exists <- doesFileExist policyPath
  if not exists
    then pure defaultPolicy
    else do
      content <- BL.readFile policyPath
      case eitherDecode content of
        Left err -> do
          TIO.putStrLn $ "Error loading hook policy: " <> T.pack err
          pure defaultPolicy
        Right policy -> pure policy

-- | Check if a tool name matches a rule's pattern.
-- Currently only supports literal matches or "*".
matchRule :: Text -> HookRule -> Bool
matchRule toolName rule =
  if rule.toolPattern == "*"
    then True
    else toolName == rule.toolPattern
