-- | Justfile effect interpreter - just CLI client.
--
-- Implements Justfile effect by calling the just CLI tool.
module ExoMonad.Justfile.Interpreter
  ( -- * Interpreter
    runJustfileIO
  ) where

import Control.Exception (try, SomeException)
import Control.Monad.Freer (Eff, LastMember, interpret, sendM)
import Data.Text (Text)
import Data.Text qualified as T
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

import ExoMonad.Effects.Justfile
  ( Justfile(..)
  , JustResult(..)
  )

-- | Run Justfile effects using the just CLI.
--
-- This interpreter shells out to the just command for each operation.
runJustfileIO :: LastMember IO effs => Eff (Justfile ': effs) a -> Eff effs a
runJustfileIO = interpret $ \case
  RunRecipe recipe args ->
    sendM $ runJustCommand recipe args

-- | Run a just command and return result.
runJustCommand :: Text -> [Text] -> IO JustResult
runJustCommand recipe args = do
  let allArgs = T.unpack recipe : map T.unpack args
  result <- try $ readProcessWithExitCode "just" allArgs ""
  case result of
    Left (e :: SomeException) ->
      pure $ JustResult
        { stdout = ""
        , stderr = T.pack $ show e
        , exitCode = 1
        }
    Right (exitCode, stdout, stderr) ->
      pure $ JustResult
        { stdout = T.pack stdout
        , stderr = T.pack stderr
        , exitCode = case exitCode of
            ExitSuccess -> 0
            ExitFailure code -> code
        }
