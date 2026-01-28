{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExoMonad.JustExec.Interpreter
  ( runJustExecIO
  )
where

import Control.Exception (try, SomeException)
import Control.Monad.Freer (Eff, LastMember, interpret, sendM)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

import ExoMonad.Effects.JustExec (JustExec(..), ExecResult(..))

-- | Run JustExec effects using docker-ctl.
--
-- Takes the path to the docker-ctl executable and an optional container name.
-- If container is Nothing, runs with --local.
runJustExecIO :: LastMember IO effs => FilePath -> Maybe Text -> Eff (JustExec ': effs) a -> Eff effs a
runJustExecIO dockerCtlPath container = interpret $ \case
  RunRecipe recipe args ->
    sendM $ runDockerCtl dockerCtlPath container recipe args

runDockerCtl :: FilePath -> Maybe Text -> Text -> [Text] -> IO ExecResult
runDockerCtl cmd container recipe args = do
  let ctlArgs = case container of
        Just c  -> ["exec", T.unpack c, "--"]
        Nothing -> ["exec", "--local", "--"]
      justCmd = ["just", T.unpack recipe] ++ map T.unpack args
      allArgs = ctlArgs ++ justCmd

  -- docker-ctl outputs JSON to stdout
  result <- try $ readProcessWithExitCode cmd allArgs ""
  case result of
    Left (e :: SomeException) ->
      pure $ ExecResult
        { exitCode = 1
        , stdout = ""
        , stderr = T.pack $ show e
        }
    Right (sysExitCode, out, err) ->
      case sysExitCode of
        ExitFailure code ->
            -- docker-ctl failed to run (e.g. usage error), so it might not have printed JSON.
            pure $ ExecResult
              { exitCode = code
              , stdout = T.pack out
              , stderr = T.pack err
              }
        ExitSuccess ->
            -- docker-ctl ran successfully. The output should be JSON.
            case eitherDecode (LBS.pack out) of
                Left parseErr ->
                    pure $ ExecResult
                        { exitCode = -1
                        , stdout = ""
                        , stderr = "Failed to parse docker-ctl output: " <> T.pack parseErr <> "\nRaw output: " <> T.pack out
                        }
                Right (res :: ExecResult) ->
                    pure res
