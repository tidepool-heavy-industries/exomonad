{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Handler stubs for V3 TDD protocol.
--
-- Handlers use effect constraints (Member Effect es) to declare only
-- the effects they need. GHC validates at invocation time.
module TypesFirstDev.Handlers
  ( maxRetryAttempts
  ) where

-- | Maximum retry attempts for Impl node before giving up.
maxRetryAttempts :: Int
maxRetryAttempts = 5
