-- | Justfile execution effect
--
-- Provides access to run recipes from a Justfile via the @just@ CLI.
module ExoMonad.Effects.Justfile
  ( -- * Effect
    Justfile(..)
  , runRecipe

    -- * Types
  , JustResult(..)

    -- * Runner (stub)
  , runJustfileStub
  ) where

import Data.Text (Text)
import qualified Data.Text
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Control.Monad.Freer (Eff, Member, send, interpret)

import ExoMonad.Effect (Log, logInfo)

-- | Result of running a just recipe.
data JustResult = JustResult
  { stdout   :: Text
  , stderr   :: Text
  , exitCode :: Int
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Justfile effect.
data Justfile r where
  RunRecipe :: Text -> [Text] -> Justfile JustResult
  -- ^ Run a recipe with arguments.

-- | Run a just recipe.
runRecipe :: Member Justfile effs => Text -> [Text] -> Eff effs JustResult
runRecipe recipe args = send (RunRecipe recipe args)

-- | Stub runner that logs calls and returns success with empty output.
runJustfileStub :: Member Log effs => Eff (Justfile ': effs) a -> Eff effs a
runJustfileStub = interpret $ \case
  RunRecipe recipe args -> do
    logInfo $ "[Justfile:stub] RunRecipe called: " <> recipe <> " " <> Data.Text.unwords args
    pure $ JustResult "" "" 0
