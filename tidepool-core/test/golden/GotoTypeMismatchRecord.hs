{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Golden test for Goto payload type mismatch.
--
-- This should produce the improved error message:
--   Graph validation failed: Goto payload type mismatch
--   Node 'router' sends:
--     Goto "handler" String
--   But target 'handler' needs:
--     • Int
module GotoTypeMismatchRecord where

import GHC.Generics (Generic)

import Tidepool.Graph.Types (type (:@), Needs, Schema, UsesEffects)
import Tidepool.Graph.Generic (GraphMode(..), Entry, Exit, LLMNode, LogicNode, ValidGraphRecord)
import Tidepool.Graph.Goto (Goto)

data Input
data Result

-- | Graph with type mismatch: router sends String but handler needs Int
data TypeMismatchGraph mode = TypeMismatchGraph
  { entry   :: mode :- Entry Input
  , router  :: mode :- LogicNode :@ Needs '[Input] :@ UsesEffects '[Goto "handler" String]  -- Sends String
  , handler :: mode :- LLMNode :@ Needs '[Int] :@ Schema Result  -- Needs Int!
  , exit    :: mode :- Exit Result
  }
  deriving Generic

check :: ()
check = validGraph @TypeMismatchGraph

validGraph :: forall g. ValidGraphRecord g => ()
validGraph = ()
