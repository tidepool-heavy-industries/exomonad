{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module SecureHooks
  ( securePreToolUse,
  )
where

import Control.Monad (void)
import Control.Monad.Freer (Eff, sendM)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as BSL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Effects.Log qualified as Log
import ExoMonad.Effects.Log (LogEmitEvent)
import ExoMonad.Guest.Tool.SuspendEffect (suspendEffect_)
import ExoMonad.Guest.Types (HookInput (..), HookOutput, allowResponse, denyResponse)
import ExoMonad.Types (HookEffects)
import System.Environment (lookupEnv)

-- | Canonicalize path: split on "/", filter out "." and resolve ".."
canonicalizePath :: Text -> Text
canonicalizePath path = T.intercalate "/" $ resolve [] (T.splitOn "/" path)
  where
    resolve acc [] = reverse acc
    resolve acc ("." : rest) = resolve acc rest
    resolve [] (".." : rest) = ".." : resolve [] rest
    resolve (_ : acc) (".." : rest) = resolve acc rest
    resolve acc ("" : rest) = resolve acc rest
    resolve acc (x : rest) = resolve (x : acc) rest

checkScope :: Text -> Text -> Bool
checkScope cwd path =
  let fullPath = if "/" `T.isPrefixOf` path then path else cwd <> "/" <> path
      canonPath = canonicalizePath fullPath
      canonCwd = canonicalizePath cwd
  in canonCwd `T.isPrefixOf` canonPath

emitEvent :: Text -> Text -> HookInput -> Eff HookEffects ()
emitEvent eventType toolName hookInput = do
  let args = fromMaybe (Aeson.Object mempty) (hiToolInput hookInput)
      payload = BSL.toStrict $ Aeson.encode $ Aeson.object
        [ "tool" Aeson..= toolName,
          "args" Aeson..= args
        ]
  void $ suspendEffect_ @LogEmitEvent (Log.EmitEventRequest
    { Log.emitEventRequestEventType = TL.fromStrict eventType,
      Log.emitEventRequestPayload = payload,
      Log.emitEventRequestTimestamp = 0
    })

securePreToolUse :: HookInput -> Eff HookEffects HookOutput
securePreToolUse hookInput = do
  envVal <- sendM $ lookupEnv "EXOMONAD_SECURE_MODE"
  allowedPathsEnv <- sendM $ lookupEnv "EXOMONAD_SECURE_ALLOWED_PATHS"
  let allowedPaths = case allowedPathsEnv of
        Nothing -> []
        Just paths -> T.splitOn ":" (T.pack paths)
  case envVal of
    Nothing -> pure (allowResponse Nothing)
    Just _ -> do
      case hiCwd hookInput of
        Nothing -> pure (allowResponse Nothing)
        Just cwd -> do
          let tool = fromMaybe "" (hiToolName hookInput)
              args = fromMaybe (Aeson.Object mempty) (hiToolInput hookInput)
          
          if tool `elem` ["Read", "Write", "Edit", "Glob", "Grep"]
            then do
              let mPath = case args of
                    Aeson.Object obj -> 
                      -- Some tools use "file_path", some use "path"
                      case KeyMap.lookup "file_path" obj of
                        Just (Aeson.String p) -> Just p
                        _ -> case KeyMap.lookup "path" obj of
                          Just (Aeson.String p) -> Just p
                          _ -> Nothing
                    _ -> Nothing
              case mPath of
                Nothing -> pure (allowResponse Nothing)
                Just path -> do
                  if checkScope cwd path
                    then do
                      emitEvent "security.access.allowed" tool hookInput
                      pure (allowResponse Nothing)
                    else if tool `elem` ["Read", "Glob", "Grep"] && path `elem` allowedPaths
                      then do
                        emitEvent "security.access.allowed" (tool <> " (allowed_read_path)") hookInput
                        pure (allowResponse Nothing)
                      else do
                        emitEvent "security.access.denied" tool hookInput
                        pure (denyResponse ("Access denied: Path escapes current working directory (" <> cwd <> ")"))
            else if tool == "Bash"
              then do
                emitEvent "security.access.warning" tool hookInput
                pure (allowResponse Nothing)
              else pure (allowResponse Nothing)
