{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module ExoMonad.Guest.Effects.AstGrep where

import Control.Monad.Freer (Eff, LastMember, Member, interpret, send, sendM)
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Guest.HostCall
import GHC.Generics (Generic)

data AstGrep r where
  Scan :: Text -> Text -> AstGrep (Either Text Text) -- Rule, Scope -> JSON or Error
  Apply :: Text -> Text -> AstGrep (Either Text ())

scan :: (Member AstGrep effs) => Text -> Text -> Eff effs (Either Text Text)
scan rule scope = send (Scan rule scope)

apply :: (Member AstGrep effs) => Text -> Text -> Eff effs (Either Text ())
apply rule scope = send (Apply rule scope)

data ScanInput = ScanInput
  { rule :: Text,
    scope :: Text
  }
  deriving (Show, Generic)

instance ToJSON ScanInput

instance FromJSON ScanInput

newtype ScanOutput = ScanOutput
  { json :: Text
  }
  deriving (Show, Generic)

instance FromJSON ScanOutput

data ApplyInput = ApplyInput
  { rule :: Text,
    scope :: Text
  }
  deriving (Show, Generic)

instance ToJSON ApplyInput

instance FromJSON ApplyInput

runAstGrep :: (LastMember IO effs) => Eff (AstGrep ': effs) a -> Eff effs a
runAstGrep = interpret $ \case
  Scan r s -> do
    let input = ScanInput {rule = r, scope = s}
    res <- sendM $ callHost host_ast_grep_scan input
    case res of
      Left err -> pure $ Left $ T.pack err
      Right (ScanOutput json) -> pure $ Right json
  Apply r s -> do
    let input = ApplyInput {rule = r, scope = s}
    res <- sendM $ callHost host_ast_grep_apply input
    case res of
      Left err -> pure $ Left $ T.pack err
      Right (_ :: Value) -> pure $ Right ()
