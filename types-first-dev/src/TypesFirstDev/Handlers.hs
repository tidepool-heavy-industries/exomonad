{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Handlers for the types-first development workflow graph.
--
-- DEPRECATED: This module contained the old sequential handler implementations.
-- All actual execution now uses Handlers/Hybrid which works with the new actor runtime.
-- This stub module is kept only for API compatibility.
module TypesFirstDev.Handlers
  ( typesFirstHandlers
    -- * Effect Stack
  , DevEffects
  , TDDEffects
    -- * Logging
  , logToFile
    -- * Build Validation (for graph handlers)
  , runAgentWithBuildValidation
  , runStubsAgentWithBuildValidation
    -- * TDD Workflow (sequential with validation loop)
  , typesFirstHandlersTDD
  , maxFixAttempts
    -- * Session-Aware Execution
  , runAgentWithBuildValidationV2
  , handleSessionEnd
    -- * Crosstalk (Parallel Agent Communication)
  , runAgentWithCrosstalk
  ) where

import Data.Text (Text)
import Control.Monad.Freer (Eff)

-- Minimal stub type for compatibility
type DevEffects = '[]
type TDDEffects = '[]

-- Stub implementations - these are not actually used
typesFirstHandlers :: a
typesFirstHandlers = error "typesFirstHandlers: Deprecated module. Use Handlers.Hybrid instead."

logToFile :: FilePath -> String -> IO ()
logToFile _path _msg = pure ()

runAgentWithBuildValidation :: a
runAgentWithBuildValidation = error "Deprecated. Use Handlers.Hybrid instead."

runStubsAgentWithBuildValidation :: a
runStubsAgentWithBuildValidation = error "Deprecated. Use Handlers.Hybrid instead."

typesFirstHandlersTDD :: a
typesFirstHandlersTDD = error "Deprecated. Use Handlers.Hybrid instead."

maxFixAttempts :: Int
maxFixAttempts = 3

runAgentWithBuildValidationV2 :: a
runAgentWithBuildValidationV2 = error "Deprecated. Use Handlers.Hybrid instead."

handleSessionEnd :: a
handleSessionEnd = error "Deprecated. Use Handlers.Hybrid instead."

runAgentWithCrosstalk :: a
runAgentWithCrosstalk = error "Deprecated. Use Handlers.Hybrid instead."
