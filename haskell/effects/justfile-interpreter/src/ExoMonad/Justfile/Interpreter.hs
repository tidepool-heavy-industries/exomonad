-- | Justfile effect interpreter - just CLI client.
--
-- Implements Justfile effect by calling the just CLI tool.
module ExoMonad.Justfile.Interpreter
  ( -- * Interpreter
    runJustfileIO,
  )
where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Effects.Justfile
  ( JustResult (..),
    Justfile (..),
  )
import Polysemy (Member, Sem, embed, interpret)
import Polysemy.Embed (Embed)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

-- | Run Justfile effects using the just CLI.
--
-- This interpreter shells out to the just command for each operation.
runJustfileIO :: (Member (Embed IO) r) => Sem (Justfile ': r) a -> Sem r a
runJustfileIO = interpret $ \case
  RunRecipe recipe args ->
    embed $ runJustCommand recipe args

-- | Run a just command and return result.
runJustCommand :: Text -> [Text] -> IO JustResult
runJustCommand recipe args = do
  let allArgs = T.unpack recipe : map T.unpack args
  result <- try $ readProcessWithExitCode "just" allArgs ""
  case result of
    Left (e :: SomeException) ->
      pure $
        JustResult
          { stdout = "",
            stderr = T.pack $ show e,
            exitCode = 1
          }
    Right (exitCode, stdout, stderr) ->
      pure $
        JustResult
          { stdout = T.pack stdout,
            stderr = T.pack stderr,
            exitCode = case exitCode of
              ExitSuccess -> 0
              ExitFailure code -> code
          }
