-- | V3 Protocol Types
--
-- Re-exports all V3 types from submodules.
-- Field prefix convention: lowercase acronym of type name (e.g., Spec -> s)
--
-- Design principles:
-- 1. LLM outputs describe WHAT was done, not the code itself
-- 2. Types derive StructuredOutput for unified schema+parsing
-- 3. Memory types enable threaded conversation context
module TypesFirstDev.V3.Types
  ( -- * Core Entry Types
    module TypesFirstDev.V3.Types.Core
    -- * Shared Workflow Types
  , module TypesFirstDev.V3.Types.Shared
    -- * Cross-Node Payloads
  , module TypesFirstDev.V3.Types.Payloads
    -- * Node-Private Memory
  , module TypesFirstDev.V3.Types.Memory
    -- * Node Input/Output Types
  , module TypesFirstDev.V3.Types.Nodes
  ) where

import TypesFirstDev.V3.Types.Core
import TypesFirstDev.V3.Types.Shared
import TypesFirstDev.V3.Types.Payloads
import TypesFirstDev.V3.Types.Memory
import TypesFirstDev.V3.Types.Nodes
