{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExoMonad.Guest.HostCall
  ( callHost,
    host_git_get_branch,
  )
where

import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Word (Word64)
import Extism.PDK (MemoryOffset (..), alloc, free, load)

foreign import ccall "git_get_branch" host_git_get_branch :: Word64 -> IO Word64

callHost :: (ToJSON req, FromJSON resp) => (Word64 -> IO Word64) -> req -> IO (Either String resp)
callHost rawFn request = do
  -- Encode and allocate
  let reqBs = toStrict (encode request)
  reqMem <- alloc reqBs

  -- Call host
  respOffset <- rawFn (unMemoryOffset reqMem)

  -- Read response
  let respMem = MemoryOffset respOffset
  respBs <- load respMem

  -- Cleanup
  free reqMem
  free respMem

  -- Decode
  case eitherDecode (fromStrict respBs) of
    Left err -> pure $ Left ("Decode error: " ++ err)
    Right v -> pure $ Right v
