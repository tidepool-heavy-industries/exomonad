{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Template Haskell utilities for the Graph DSL.
--
-- Note: The record-based DSL (Tidepool.Graph.Generic) uses GHC.Generics
-- for traversal and doesn't require Template Haskell for handler generation.
--
-- Handler types are computed via the NodeHandler type family in Generic.hs.
module Tidepool.Graph.TH
  (
  ) where

-- This module is kept for backwards compatibility but is no longer used.
-- The record-based DSL doesn't require TH for handler generation.
