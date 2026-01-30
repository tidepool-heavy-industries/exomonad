-- | Justfile execution effect
--
-- Provides access to run recipes from a Justfile via the @just@ CLI.
module ExoMonad.Effects.Justfile
  ( -- * Effect
    Justfile (..),
    runRecipe,

    -- * Types
    JustResult (..),

    -- * Runner (stub)
    runJustfileStub,
  )
where

import Control.Monad.Freer (Eff, Member, interpret, send)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text qualified
import ExoMonad.Effect (Log, logInfo)
import GHC.Generics (Generic)

-- | Result of running a just recipe.
data JustResult = JustResult
  { stdout :: Text,
    stderr :: Text,
    exitCode :: Int
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Justfile effect.
data Justfile r where
  RunRecipe ::
    Text ->
    [Text] ->
    -- | Run a recipe with arguments.
    Justfile JustResult

-- | Run a just recipe.
runRecipe :: (Member Justfile effs) => Text -> [Text] -> Eff effs JustResult
runRecipe recipe args = send (RunRecipe recipe args)

-- | Stub runner that logs calls and returns success with empty output.
runJustfileStub :: (Member Log effs) => Eff (Justfile ': effs) a -> Eff effs a
runJustfileStub = interpret $ \case
  RunRecipe recipe args -> do
    logInfo $ "[Justfile:stub] RunRecipe called: " <> recipe <> " " <> Data.Text.unwords args
    pure $ JustResult "" "" 0
