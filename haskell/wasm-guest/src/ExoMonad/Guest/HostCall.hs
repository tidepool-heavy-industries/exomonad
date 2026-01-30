{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExoMonad.Guest.HostCall
  ( callHost,
    host_git_get_branch,
  )
where

import Control.Exception (bracket)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Word (Word64)
import Extism.PDK (MemoryOffset (..), alloc, free, load)

foreign import ccall "git_get_branch" host_git_get_branch :: Word64 -> IO Word64

callHost :: (ToJSON req, FromJSON resp) => (Word64 -> IO Word64) -> req -> IO (Either String resp)
callHost rawFn request = do
  -- Encode and allocate
  let reqBs = toStrict (encode request)

  bracket (alloc reqBs) free $ \reqMem -> do
    -- Call host
    respOffset <- rawFn (unMemoryOffset reqMem)

    -- Read response (ensure we free the response memory)
    let respMem = MemoryOffset respOffset
    bracket (pure respMem) free $ \_ -> do
      respBs <- load respMem

      -- Decode
      case eitherDecode (fromStrict respBs) of
        Left err -> pure $ Left ("Decode error: " ++ err)
        Right v -> pure $ Right v
