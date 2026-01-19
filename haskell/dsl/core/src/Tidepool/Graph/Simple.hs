{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Helper for simple Entry → Logic → Exit graphs.
--
-- Reduces handler boilerplate from 5 lines to 1 line while preserving
-- all graph record metadata (needed for MCP export and validation).
--
-- @
-- -- Before:
-- findCallersHandlers = FindCallersGraph
--   { fcEntry = ()
--   , fcRun = findCallersLogic
--   , fcExit = ()
--   }
--
-- -- After:
-- findCallersHandlers = simpleGraph findCallersLogic
-- @
--
-- The graph type is inferred from context (return type annotation).
module Tidepool.Graph.Simple
  ( SimpleGraphPattern(..)
  ) where

import Control.Monad.Freer (Eff)
import Tidepool.Graph.Generic (AsHandler)
import Tidepool.Graph.Goto (GotoChoice, To)
import Tidepool.Graph.Types (Exit)

-- | Typeclass for graphs with Entry → Logic → Exit pattern.
--
-- Allows constructing handlers from just the logic function,
-- automatically filling Entry/Exit with ().
--
-- @
-- instance SimpleGraphPattern FindCallersGraph FindCallersArgs FindCallersResult where
--   simpleGraph logic = FindCallersGraph
--     { fcEntry = ()
--     , fcRun = logic
--     , fcExit = ()
--     }
-- @
--
-- The graph type, input type, and output type are typically inferred from:
-- 1. Return type annotation on the handler definition
-- 2. Logic function signature
class SimpleGraphPattern graph input output | graph -> input output where
  -- | Construct a simple graph handler from just the logic function.
  --
  -- Entry and Exit handlers are automatically filled with ().
  -- The logic function must return GotoChoice with a 'To Exit output' target.
  simpleGraph :: (input -> Eff es (GotoChoice '[To Exit output]))
              -> graph (AsHandler es)
