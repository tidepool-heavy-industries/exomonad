{-# LANGUAGE OverloadedStrings #-}

-- | Wire format compatibility layer for proto3-suite <-> prost interop.
--
-- proto3-suite generates enums with SCREAMING_SNAKE_CASE (e.g., ERROR_CODE_NOT_FOUND).
-- prost with serde generates snake_case (e.g., not_found).
--
-- This module provides conversions for JSON wire format compatibility.
module Exomonad.Compat
  ( toWireJSON
  , fromWireJSON
  , toWireJSONPB
  , fromWireJSONPB
  , protoToWire
  , wireToProto
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Char as Char
import qualified Data.Text as T
import Data.Text (Text)
import Proto3.Suite.JSONPB (FromJSONPB(..), ToJSONPB(..), Options(..), defaultOptions)
import qualified Proto3.Suite.JSONPB as JSONPB

-- | Convert proto3-suite value to JSON with snake_case enums for wire compatibility.
toWireJSON :: ToJSONPB a => a -> LBS.ByteString
toWireJSON = transformEnums protoToWire . JSONPB.encode defaultOptions

-- | Parse JSON with snake_case enums back to proto3-suite value.
fromWireJSON :: FromJSONPB a => LBS.ByteString -> Either String a
fromWireJSON bs = do
  val <- Aeson.eitherDecode bs
  let transformed = transformEnumsValue wireToProtoValue val
  JSONPB.eitherDecode (Aeson.encode transformed)

-- | Like toWireJSON but returns Aeson.Value.
toWireJSONPB :: ToJSONPB a => a -> Aeson.Value
toWireJSONPB a = transformEnumsValue protoToWireValue (JSONPB.toAesonValue a)

-- | Like fromWireJSON but takes Aeson.Value.
fromWireJSONPB :: FromJSONPB a => Aeson.Value -> Either String a
fromWireJSONPB val =
  let transformed = transformEnumsValue wireToProtoValue val
  in JSONPB.eitherDecode (Aeson.encode transformed)

-- | Convert SCREAMING_SNAKE_CASE enum to snake_case for wire.
-- E.g., "ERROR_CODE_NOT_FOUND" -> "not_found"
protoToWire :: Text -> Text
protoToWire t = case T.splitOn "_" t of
  -- Strip common prefixes: ERROR_CODE_, HOOK_TYPE_, AGENT_TYPE_, etc.
  (prefix : rest)
    | prefix `elem` ["ERROR", "HOOK", "AGENT", "STOP", "ROLE", "TOOL"] ->
        case rest of
          ("CODE" : vals) -> T.toLower (T.intercalate "_" vals)
          ("TYPE" : vals) -> T.toLower (T.intercalate "_" vals)
          ("DECISION" : vals) -> T.toLower (T.intercalate "_" vals)
          ("STATUS" : vals) -> T.toLower (T.intercalate "_" vals)
          ("PERMISSION" : vals) -> T.toLower (T.intercalate "_" vals)
          vals -> T.toLower (T.intercalate "_" vals)
  parts -> T.toLower (T.intercalate "_" parts)

-- | Convert snake_case wire enum to SCREAMING_SNAKE_CASE for proto.
-- This is a heuristic and may not work for all enums.
wireToProto :: Text -> Text -> Text
wireToProto prefix val = prefix <> "_" <> T.toUpper val

-- Internal helpers

transformEnums :: (Text -> Text) -> LBS.ByteString -> LBS.ByteString
transformEnums f bs = case Aeson.decode bs of
  Nothing -> bs
  Just val -> Aeson.encode (transformEnumsValue (liftTextTransform f) val)

liftTextTransform :: (Text -> Text) -> Aeson.Value -> Aeson.Value
liftTextTransform f (Aeson.String t) = Aeson.String (f t)
liftTextTransform _ v = v

protoToWireValue :: Aeson.Value -> Aeson.Value
protoToWireValue (Aeson.String t)
  | isScreamingSnake t = Aeson.String (protoToWire t)
protoToWireValue v = v

wireToProtoValue :: Aeson.Value -> Aeson.Value
wireToProtoValue = id  -- Wire format is already what prost produces

transformEnumsValue :: (Aeson.Value -> Aeson.Value) -> Aeson.Value -> Aeson.Value
transformEnumsValue f (Aeson.Object obj) = Aeson.Object (fmap (transformEnumsValue f) obj)
transformEnumsValue f (Aeson.Array arr) = Aeson.Array (fmap (transformEnumsValue f) arr)
transformEnumsValue f v = f v

-- | Check if a text looks like SCREAMING_SNAKE_CASE enum.
isScreamingSnake :: Text -> Bool
isScreamingSnake t =
  not (T.null t)
    && T.all (\c -> Char.isUpper c || c == '_' || Char.isDigit c) t
    && T.any Char.isUpper t
