{-# LANGUAGE OverloadedStrings #-}

module ExoMonad.Guest.Continuations
  ( Continuation
  , newContinuationId
  , storeContinuation
  , retrieveContinuation
  ) where

import Data.Aeson (Value)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Unique (newUnique, hashUnique)
import System.IO.Unsafe (unsafePerformIO)
import ExoMonad.Guest.Tool.Class (MCPCallOutput, WasmResult)

-- | A continuation takes a Value (result from effect) and returns the next step (WasmResult).
type Continuation = Value -> IO (WasmResult MCPCallOutput)

{-# NOINLINE continuationStore #-}
continuationStore :: IORef (Map Text Continuation)
continuationStore = unsafePerformIO (newIORef Map.empty)

newContinuationId :: IO Text
newContinuationId = do
  u <- newUnique
  pure $ "k-" <> T.pack (show (hashUnique u))

storeContinuation :: Text -> Continuation -> IO ()
storeContinuation k cont = do
  modifyIORef' continuationStore (Map.insert k cont)

retrieveContinuation :: Text -> IO (Maybe Continuation)
retrieveContinuation k = do
  m <- readIORef continuationStore
  let cont = Map.lookup k m
  -- One-shot continuation: remove after retrieval
  modifyIORef' continuationStore (Map.delete k)
  pure cont
