{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Common combinators for MCP tool handlers to reduce boilerplate.
module ExoMonad.Control.Combinators
  ( -- * GitHub Combinators
    withGitHubRepo,
    getRepo,

    -- * Effect Combinators
    withEffect,
  )
where

import Control.Monad.Freer (Eff, Member)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Effects.Env (Env, getEnv)
import ExoMonad.Effects.GitHub (GitHub, GitHubError, Repo (..))

-- | Get repository from args or environment.
getRepo :: (Member Env es) => Maybe Text -> Eff es Repo
getRepo mRepo = do
  case mRepo of
    Just r -> pure $ Repo r
    Nothing -> do
      mEnvRepo <- getEnv "GITHUB_REPO"
      pure $ Repo $ fromMaybe "exomonad-ai/exomonad" mEnvRepo

-- | A combinator that handles the getRepo + GitHub effect call + Either unwrapping.
withGitHubRepo ::
  (Member GitHub es, Member Env es) =>
  -- | optional repo override
  Maybe Text ->
  -- | GitHub operation
  (Repo -> Eff es (Either GitHubError a)) ->
  -- | success mapper
  (a -> result) ->
  -- | error mapper
  (Text -> result) ->
  Eff es result
withGitHubRepo mRepo op onSuccess onError = do
  repo <- getRepo mRepo
  res <- op repo
  pure $ case res of
    Right a -> onSuccess a
    Left err -> onError (T.pack $ show err)

-- | Generalize the pattern beyond GitHub.
-- This is essentially 'either' lifted into 'Eff'.
withEffect ::
  -- | effectful operation
  Eff es (Either e a) ->
  -- | on success
  (a -> Eff es result) ->
  -- | on error
  (e -> Eff es result) ->
  Eff es result
withEffect op onSuccess onError = do
  res <- op
  case res of
    Right a -> onSuccess a
    Left e -> onError e
