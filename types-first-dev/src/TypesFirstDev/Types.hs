-- | TDD Protocol Types
--
-- Re-exports all types from submodules.
-- Field prefix convention: lowercase acronym of type name (e.g., Spec -> s)
--
-- Design principles:
-- 1. LLM outputs describe WHAT was done, not the code itself
-- 2. Types derive StructuredOutput for unified schema+parsing
-- 3. Memory types enable threaded conversation context
module TypesFirstDev.Types
  ( -- * Core Entry Types
    module TypesFirstDev.Types.Core
    -- * Shared Workflow Types
  , module TypesFirstDev.Types.Shared
    -- * Cross-Node Payloads
  , module TypesFirstDev.Types.Payloads
    -- * Node-Private Memory
  , module TypesFirstDev.Types.Memory
    -- * Node Input/Output Types
  , module TypesFirstDev.Types.Nodes
  ) where

import TypesFirstDev.Types.Core
import TypesFirstDev.Types.Shared
import TypesFirstDev.Types.Payloads
import TypesFirstDev.Types.Memory
import TypesFirstDev.Types.Nodes
