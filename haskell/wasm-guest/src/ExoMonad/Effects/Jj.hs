{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module ExoMonad.Effects.Jj
  ( -- * Effect Types
    JjBookmarkCreate,
    JjGitPush,
    JjGitFetch,
    JjLog,
    JjNew,
    JjStatus,

    -- * Smart Constructors
    bookmarkCreate,
    gitPush,
    gitFetch,
    jjLog,
    jjNew,
    jjStatus,

    -- * Re-exported proto types
    module Effects.Jj,
  )
where

import Effects.EffectError (EffectError)
import Effects.Jj
import ExoMonad.Effect.Class (Effect (..), runEffect)

data JjBookmarkCreate

instance Effect JjBookmarkCreate where
  type Input JjBookmarkCreate = BookmarkCreateRequest
  type Output JjBookmarkCreate = BookmarkCreateResponse
  effectId = "jj.bookmark_create"

data JjGitPush

instance Effect JjGitPush where
  type Input JjGitPush = GitPushRequest
  type Output JjGitPush = GitPushResponse
  effectId = "jj.git_push"

data JjGitFetch

instance Effect JjGitFetch where
  type Input JjGitFetch = GitFetchRequest
  type Output JjGitFetch = GitFetchResponse
  effectId = "jj.git_fetch"

data JjLog

instance Effect JjLog where
  type Input JjLog = LogRequest
  type Output JjLog = LogResponse
  effectId = "jj.log"

data JjNew

instance Effect JjNew where
  type Input JjNew = NewRequest
  type Output JjNew = NewResponse
  effectId = "jj.new"

data JjStatus

instance Effect JjStatus where
  type Input JjStatus = StatusRequest
  type Output JjStatus = StatusResponse
  effectId = "jj.status"

bookmarkCreate :: BookmarkCreateRequest -> IO (Either EffectError BookmarkCreateResponse)
bookmarkCreate = runEffect @JjBookmarkCreate

gitPush :: GitPushRequest -> IO (Either EffectError GitPushResponse)
gitPush = runEffect @JjGitPush

gitFetch :: GitFetchRequest -> IO (Either EffectError GitFetchResponse)
gitFetch = runEffect @JjGitFetch

jjLog :: LogRequest -> IO (Either EffectError LogResponse)
jjLog = runEffect @JjLog

jjNew :: NewRequest -> IO (Either EffectError NewResponse)
jjNew = runEffect @JjNew

jjStatus :: StatusRequest -> IO (Either EffectError StatusResponse)
jjStatus = runEffect @JjStatus
