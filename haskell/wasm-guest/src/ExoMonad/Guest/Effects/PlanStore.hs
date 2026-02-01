{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module ExoMonad.Guest.Effects.PlanStore where

import Control.Monad.Freer (Eff, LastMember, Member, interpret, send, sendM)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Guest.HostCall
import GHC.Generics (Generic)

data RefactorPlan = RefactorPlan
  { id :: Text,
    rule :: Text,
    scope :: Text,
    language :: Text,
    diff :: Text,
    files :: [Text]
  }
  deriving (Show, Generic)

instance ToJSON RefactorPlan

instance FromJSON RefactorPlan

data NewRefactorPlan = NewRefactorPlan
  { rule :: Text,
    scope :: Text,
    language :: Text,
    diff :: Text,
    files :: [Text]
  }
  deriving (Show, Generic)

instance ToJSON NewRefactorPlan

instance FromJSON NewRefactorPlan

data PlanStore r where
  StorePlan :: NewRefactorPlan -> PlanStore (Either Text Text) -- Returns ID
  GetPlan :: Text -> PlanStore (Either Text (Maybe RefactorPlan))
  DeletePlan :: Text -> PlanStore (Either Text Bool)

storePlan :: (Member PlanStore effs) => NewRefactorPlan -> Eff effs (Either Text Text)
storePlan plan = send (StorePlan plan)

getPlan :: (Member PlanStore effs) => Text -> Eff effs (Either Text (Maybe RefactorPlan))
getPlan id = send (GetPlan id)

deletePlan :: (Member PlanStore effs) => Text -> Eff effs (Either Text Bool)
deletePlan id = send (DeletePlan id)

-- DTOs for Host Calls
data StorePlanInput = StorePlanInput
  { plan :: NewRefactorPlan
  }
  deriving (Show, Generic)

instance ToJSON StorePlanInput

newtype StorePlanOutput = StorePlanOutput
  { id :: Text
  }
  deriving (Show, Generic)

instance FromJSON StorePlanOutput

newtype GetPlanInput = GetPlanInput
  { id :: Text
  }
  deriving (Show, Generic)

instance ToJSON GetPlanInput

newtype GetPlanOutput = GetPlanOutput
  { plan :: Maybe RefactorPlan
  }
  deriving (Show, Generic)

instance FromJSON GetPlanOutput

newtype DeletePlanInput = DeletePlanInput
  { id :: Text
  }
  deriving (Show, Generic)

instance ToJSON DeletePlanInput

newtype DeletePlanOutput = DeletePlanOutput
  { deleted :: Bool
  }
  deriving (Show, Generic)

instance FromJSON DeletePlanOutput

runPlanStore :: (LastMember IO effs) => Eff (PlanStore ': effs) a -> Eff effs a
runPlanStore = interpret $ \case
  StorePlan p -> do
    let input = StorePlanInput p
    res <- sendM $ callHost host_plan_store_save input
    case res of
      Left err -> pure $ Left $ T.pack err
      Right (StorePlanOutput pid) -> pure $ Right pid
  GetPlan pid -> do
    let input = GetPlanInput pid
    res <- sendM $ callHost host_plan_store_get input
    case res of
      Left err -> pure $ Left $ T.pack err
      Right (GetPlanOutput mp) -> pure $ Right mp
  DeletePlan pid -> do
    let input = DeletePlanInput pid
    res <- sendM $ callHost host_plan_store_delete input
    case res of
      Left err -> pure $ Left $ T.pack err
      Right (DeletePlanOutput d) -> pure $ Right d
