-- | Bridge between coroutine-based tool handlers and the WASM trampoline.
--
-- Converts @Eff ToolEffects@ computations into @IO (WasmResult MCPCallOutput)@,
-- storing continuations for suspended computations.
module ExoMonad.Guest.Tool.Suspend
  ( runToolEff,
  )
where

import Control.Monad.Freer (Eff, runM)
import Control.Monad.Freer.Coroutine (Status, runC)
import Control.Monad.Freer.Coroutine qualified as C
import Data.Aeson (Value)
import ExoMonad.Guest.Continuations (newContinuationId, storeContinuation)
import ExoMonad.Guest.Tool.Class (EffectRequest, MCPCallOutput, ToolEffects, WasmResult (..))

-- | Run an effectful tool handler, converting to WasmResult.
-- Handles both immediate completion and suspension with continuation storage.
runToolEff :: Eff ToolEffects MCPCallOutput -> IO (WasmResult MCPCallOutput)
runToolEff eff = do
  status <- runM (runC eff)
  statusToWasmResult status

-- | Convert a coroutine Status to a WasmResult, storing continuations as needed.
statusToWasmResult :: Status '[IO] EffectRequest Value MCPCallOutput -> IO (WasmResult MCPCallOutput)
statusToWasmResult (C.Done output) = pure $ Done output
statusToWasmResult (C.Continue req k) = do
  kId <- newContinuationId
  storeContinuation kId $ \val -> runM (k val) >>= statusToWasmResult
  pure $ Suspend kId req
