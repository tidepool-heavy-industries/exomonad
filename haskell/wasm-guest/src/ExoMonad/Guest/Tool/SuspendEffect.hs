{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Bridge between typed Effect dispatch and coroutine suspension.
--
-- Like 'runEffect', but yields via coroutine 'suspend' instead of direct FFI.
-- The host dispatches the effect async without holding the WASM plugin lock.
module ExoMonad.Guest.Tool.SuspendEffect
  ( suspendEffect,
    suspendEffect_,
  )
where

import Control.Monad.Freer (Eff, Member)
import Data.Aeson (Value, toJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text.Lazy qualified as TL
import Data.Word (Word8)
import Effects.EffectError (EffectError (..))
import Effects.EffectError qualified as EE
import Effects.Envelope (EffectResponse (..), EffectResponseResult (..))
import ExoMonad.Effect.Class (Effect (..))
import ExoMonad.Guest.Tool.Class (EffectRequest (..), SuspendYield, suspend)
import Proto3.Suite.Class (Message, fromByteString, toLazyByteString)

-- | Like runEffect, but yields via coroutine suspend instead of direct FFI.
suspendEffect ::
  forall e effs.
  ( Effect e,
    Message (Input e),
    Message (Output e),
    Member SuspendYield effs
  ) =>
  Input e ->
  Eff effs (Either EffectError (Output e))
suspendEffect reqInput = do
  let reqBytes = BL.toStrict (toLazyByteString reqInput)
      payloadJson = toJSON (BS.unpack reqBytes :: [Word8])
  resultJson <- suspend (EffectRequest (TL.toStrict (effectId @e)) payloadJson)
  pure $ decodeEffectResponse resultJson

-- | Fire-and-forget variant that discards the response payload.
suspendEffect_ ::
  forall e effs.
  ( Effect e,
    Message (Input e),
    Member SuspendYield effs
  ) =>
  Input e ->
  Eff effs (Either EffectError ())
suspendEffect_ reqInput = do
  let reqBytes = BL.toStrict (toLazyByteString reqInput)
      payloadJson = toJSON (BS.unpack reqBytes :: [Word8])
  resultJson <- suspend (EffectRequest (TL.toStrict (effectId @e)) payloadJson)
  pure $ case decodeResponseEnvelope resultJson of
    Left err -> Left err
    Right _ -> Right ()

-- Decode JSON Value → EffectResponse protobuf → typed response
decodeEffectResponse :: forall resp. (Message resp) => Value -> Either EffectError resp
decodeEffectResponse val = do
  envBytes <- decodeResponseEnvelope val
  case fromByteString envBytes of
    Left parseErr ->
      Left $ mkErr "payload_decode_error" (show parseErr)
    Right resp -> Right resp

-- Decode JSON array → ByteString, then decode EffectResponse envelope
decodeResponseEnvelope :: Value -> Either EffectError BS.ByteString
decodeResponseEnvelope val = do
  responseBytes <- case Aeson.fromJSON val :: Aeson.Result [Word8] of
    Aeson.Success bytes -> Right (BS.pack bytes)
    Aeson.Error err -> Left $ mkErr "json_decode_error" err
  case fromByteString responseBytes of
    Left parseErr ->
      Left $ mkErr "response_decode_error" (show parseErr)
    Right (EffectResponse (Just (EffectResponseResultPayload payloadBytes))) ->
      Right payloadBytes
    Right (EffectResponse (Just (EffectResponseResultError effErr))) ->
      Left effErr
    Right (EffectResponse Nothing) ->
      Left $ mkErr "empty_response" "EffectResponse had no result"

mkErr :: String -> String -> EffectError
mkErr code msg =
  EffectError
    { effectErrorKind =
        Just $
          EE.EffectErrorKindCustom $
            EE.Custom
              { EE.customCode = TL.pack code,
                EE.customMessage = TL.pack msg,
                EE.customData = BS.empty
              }
    }
