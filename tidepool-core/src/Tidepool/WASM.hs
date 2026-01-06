{-|
Module: Tidepool.WASM
Description: Minimal re-exports for WASM builds

This module provides a minimal surface area for WASM builds, excluding:
- Documentation generators (Mermaid, Docs)
- Example graphs (Graph.Example)
- Unused effect implementations (GitHub, Obsidian, Calendar)
- Dev-only types (Anthropic.Types, Question, Tool, Schema, Delta)

Expected savings: ~500 KB in WASM binary size.

Usage in WASM packages:
@
import Tidepool.WASM
@

Instead of:
@
import Tidepool  -- pulls in everything
@
-}
module Tidepool.WASM
  ( -- * Graph System
    -- ** Core Type-Level Operators
    type (:@)
  , Input
  , UsesEffects
  , Self

    -- ** Generic Graph Modes
  , GraphMode(..)  -- Includes (:-) type operator
  , AsGraph
  , Entry
  , Exit
  , LLMNode
  , LogicNode

    -- ** Goto System
  , module Tidepool.Graph.Goto
  , module Tidepool.Graph.Goto.Internal

    -- ** Graph Reification
  , module Tidepool.Graph.Reify

    -- * Effects
  , module Tidepool.Effect.Metadata
  ) where

-- Graph types (selective re-export to avoid Exit conflict)
import Tidepool.Graph.Types (type (:@), Input, UsesEffects, Self)
import Tidepool.Graph.Generic
  ( GraphMode(..)
  , type (:-)
  , AsGraph
  , Entry
  , Exit
  , LLMNode
  , LogicNode
  )

-- Goto system
import Tidepool.Graph.Goto
import Tidepool.Graph.Goto.Internal

-- Reification
import Tidepool.Graph.Reify

-- Effect metadata
import Tidepool.Effect.Metadata
