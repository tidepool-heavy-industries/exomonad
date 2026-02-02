{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

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

import Polysemy (Sem, Member, interpret, makeSem)
import Data.Kind (Type)
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
data Justfile m a where
  RunRecipe ::
    Text ->
    [Text] ->
    -- | Run a recipe with arguments.
    Justfile m JustResult

makeSem ''Justfile

-- | Stub runner that logs calls and returns success with empty output.
runJustfileStub :: (Member Log effs) => Sem (Justfile ': effs) a -> Sem effs a
runJustfileStub = interpret $ \case
  RunRecipe recipe args -> do
    logInfo $ "[Justfile:stub] RunRecipe called: " <> recipe <> " " <> Data.Text.unwords args
    pure $ JustResult "" "" 0

