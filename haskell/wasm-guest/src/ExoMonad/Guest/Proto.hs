module ExoMonad.Guest.Proto (fromText, toText) where

import Data.Text (Text)
import Data.Text.Lazy qualified as TL

fromText :: Text -> TL.Text
fromText = TL.fromStrict

toText :: TL.Text -> Text
toText = TL.toStrict
