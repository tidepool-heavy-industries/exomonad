-- | Bridge between coroutine-based tool handlers and the WASM trampoline.
--
-- Converts @Eff ToolEffects@ computations into @IO (WasmResult MCPCallOutput)@,
-- storing continuations for suspended computations.
module ExoMonad.Guest.Tool.Suspend
  ( runToolEff,
    statusToWasmResult,
  )
where

import Control.Monad.Freer (Eff, runM)
import Control.Monad.Freer.Coroutine (Status, runC)
import Control.Monad.Freer.Coroutine qualified as C
import Data.Aeson (ToJSON, Value)
import Data.Aeson qualified as Aeson
import ExoMonad.Guest.Continuations (newContinuationId, storeContinuation)
import ExoMonad.Guest.Effects.AgentControl (runAgentControlSuspend)
import ExoMonad.Guest.Effects.FileSystem (runFileSystemSuspend)
import ExoMonad.Guest.Tool.Class (EffectRequest, MCPCallOutput, ToolEffects, WasmResult (..))

-- | Run an effectful tool handler, converting to WasmResult.
-- Handles both immediate completion and suspension with continuation storage.
runToolEff :: Eff ToolEffects MCPCallOutput -> IO (WasmResult MCPCallOutput)
runToolEff eff = do
  status <- runM (runC (runFileSystemSuspend (runAgentControlSuspend eff)))
  statusToWasmResult status

-- | Convert a coroutine Status to a WasmResult, storing continuations as needed.
statusToWasmResult :: (ToJSON a) => Status '[IO] EffectRequest Value a -> IO (WasmResult a)
statusToWasmResult (C.Done output) = pure $ Done output
statusToWasmResult (C.Continue req k) = do
  kId <- newContinuationId
  storeContinuation kId $ \val -> do
    res <- runM (k val) >>= statusToWasmResult
    pure (Aeson.toJSON res)
  pure $ Suspend kId req
