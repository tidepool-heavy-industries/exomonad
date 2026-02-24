{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Types for coroutine suspension in tools.
module ExoMonad.Guest.Tool.Suspend.Types
  ( SuspendYield,
    EffectRequest (..),
    suspend,
  )
where

import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.Coroutine (Yield, yield)
import Data.Aeson (ToJSON (..), Value, object, (.=))
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Yield type for tool handlers: yield an EffectRequest, receive a Value back.
type SuspendYield = Yield EffectRequest Value

-- | Effect request for suspension.
data EffectRequest = EffectRequest
  { erType :: Text,
    erPayload :: Value
  }
  deriving (Show, Eq, Generic)

instance ToJSON EffectRequest where
  toJSON (EffectRequest t p) = object ["type" .= t, "payload" .= p]

-- | Suspend execution, yielding an effect request to the host.
-- Returns the result value when resumed.
suspend :: (Member SuspendYield effs) => EffectRequest -> Eff effs Value
suspend req = yield req id
