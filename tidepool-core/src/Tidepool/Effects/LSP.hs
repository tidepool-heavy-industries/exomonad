-- | Language Server Protocol effect (native utilities)
--
-- This module re-exports the IO-blind LSP effect from 'Tidepool.Effect.LSP'
-- and provides a stub runner for testing/development.
--
-- = Architecture
--
-- * 'Tidepool.Effect.LSP' - Core types and effect (WASM-safe)
-- * 'Tidepool.Effects.LSP' - This module, native utilities (not in WASM builds)
-- * 'Tidepool.LSP.Executor' - Real interpreter using lsp-client
--
-- = Usage
--
-- For the effect and types, import 'Tidepool.Effect.LSP':
--
-- @
-- import Tidepool.Effect.LSP (LSP, hover, textDocument, position)
-- @
--
-- For the stub runner (testing/development):
--
-- @
-- import Tidepool.Effects.LSP (runLSPStub)
-- @
--
-- For real LSP functionality, use the lsp-executor package:
--
-- @
-- import Tidepool.LSP.Executor (withLSPSession, runLSP)
-- @
--
module Tidepool.Effects.LSP
  ( -- * Re-exports from Tidepool.Effect.LSP
    module Tidepool.Effect.LSP

    -- * Stub Runner
  , runLSPStub
  ) where

import Control.Monad.Freer (Eff, Member, interpret)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Tidepool.Effect (Log, logInfo)
import Tidepool.Effect.LSP


-- ════════════════════════════════════════════════════════════════════════════
-- STUB RUNNER
-- ════════════════════════════════════════════════════════════════════════════

-- | Stub runner that logs operations and returns empty/error results.
--
-- This is a placeholder for testing and development. For real LSP
-- functionality, use 'Tidepool.LSP.Executor.runLSP' from lsp-executor.
runLSPStub :: Member Log effs => Eff (LSP ': effs) a -> Eff effs a
runLSPStub = interpret $ \case
  Diagnostics doc -> do
    logInfo $ "[LSP:stub] Diagnostics called for: " <> doc.tdiUri
    pure []  -- Return empty list instead of erroring

  Hover doc pos -> do
    logInfo $ "[LSP:stub] Hover at " <> doc.tdiUri <> " " <> showPos pos
    pure Nothing

  References doc pos -> do
    logInfo $ "[LSP:stub] References at " <> doc.tdiUri <> " " <> showPos pos
    pure []

  Definition doc pos -> do
    logInfo $ "[LSP:stub] Definition at " <> doc.tdiUri <> " " <> showPos pos
    pure []

  CodeActions doc rng -> do
    logInfo $ "[LSP:stub] CodeActions at " <> doc.tdiUri <> " " <> showRange rng
    pure []

  Rename doc pos newName -> do
    logInfo $ "[LSP:stub] Rename at " <> doc.tdiUri <> " " <> showPos pos <> " to " <> newName
    pure (WorkspaceEdit Map.empty)

  Completion doc pos -> do
    logInfo $ "[LSP:stub] Completion at " <> doc.tdiUri <> " " <> showPos pos
    pure []
  where
    showPos :: Position -> Text
    showPos p = "(" <> showT p.posLine <> ":" <> showT p.posCharacter <> ")"

    showRange :: Range -> Text
    showRange r = showPos r.rangeStart <> "-" <> showPos r.rangeEnd

    showT :: Show a => a -> Text
    showT = T.pack . show
