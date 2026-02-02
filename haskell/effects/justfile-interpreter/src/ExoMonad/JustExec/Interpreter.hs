{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExoMonad.JustExec.Interpreter
  ( runJustExecIO,
  )
where

import Control.Exception (SomeException, try)
import Polysemy (Sem, Member, interpret, embed)
import Polysemy.Embed (Embed)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import ExoMonad.Effects.JustExec (ExecResult (..), JustExec (..))
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

-- | Run JustExec effects using docker-ctl.
--
-- Takes the path to the docker-ctl executable and an optional container name.
-- If container is Nothing, runs with --local.
runJustExecIO :: (Member (Embed IO) r) => FilePath -> Maybe Text -> Sem (JustExec ': r) a -> Sem r a
runJustExecIO dockerCtlPath container = interpret $ \case
  ExecRecipe recipe args ->
    embed $ runDockerCtl dockerCtlPath container recipe args

runDockerCtl :: FilePath -> Maybe Text -> Text -> [Text] -> IO ExecResult
runDockerCtl cmd container recipe args = do
  let ctlArgs = case container of
        Just c -> ["exec", T.unpack c, "--"]
        Nothing -> ["exec", "--local", "--"]
      justCmd = ["just", T.unpack recipe] ++ map T.unpack args
      allArgs = ctlArgs ++ justCmd

  -- docker-ctl outputs JSON to stdout
  result <- try $ readProcessWithExitCode cmd allArgs ""
  case result of
    Left (e :: SomeException) ->
      pure $
        ExecResult
          { exitCode = 1,
            stdout = "",
            stderr = T.pack $ show e
          }
    Right (sysExitCode, out, err) ->
      case sysExitCode of
        ExitFailure code ->
          -- docker-ctl failed to run (e.g. usage error), so it might not have printed JSON.
          pure $
            ExecResult
              { exitCode = code,
                stdout = T.pack out,
                stderr = T.pack err
              }
        ExitSuccess ->
          -- docker-ctl ran successfully. The output should be JSON.
          -- Use robust UTF-8 encoding for parsing
          let jsonBytes = LBS.fromStrict $ TE.encodeUtf8 $ T.pack out
           in case eitherDecode jsonBytes of
                Left parseErr ->
                  pure $
                    ExecResult
                      { exitCode = -1,
                        stdout = "",
                        stderr = "Failed to parse docker-ctl output: " <> T.pack parseErr <> "\nRaw output: " <> T.pack out
                      }
                Right (res :: ExecResult) ->
                  pure res
