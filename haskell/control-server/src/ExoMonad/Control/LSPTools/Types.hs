{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ExoMonad.Control.LSPTools.Types
  ( -- * Find Callers
    FindCallersArgs(..)
  , FindCallersResult(..)
  , CallSite(..)

    -- * Find Callees
  , FindCalleesArgs(..)
  , FindCalleesResult(..)
  , CalleeInfo(..)

    -- * Show Type
  , ShowTypeArgs(..)
  , ShowTypeResult(..)
  , TypeField(..)
  , TypeConstructor(..)

    -- * Show Fields
  , ShowFieldsArgs(..)
  , ShowFieldsResult(..)
  , RecordField(..)

    -- * Show Constructors
  , ShowConstructorsArgs(..)
  , ShowConstructorsResult(..)
  , Constructor(..)
  ) where

import Data.Aeson (ToJSON(..), object, (.=))
import Data.Text (Text)
import GHC.Generics (Generic)
import ExoMonad.Effect.LSP (Position)

-- ════════════════════════════════════════════════════════════════════════════
-- FIND-CALLERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for find_callers tool.
data FindCallersArgs = FindCallersArgs
  { fcaName :: Text              -- ^ Function name to find callers of
  , fcaContextLines :: Maybe Int -- ^ Lines of context (default: 1)
  , fcaMaxResults :: Maybe Int   -- ^ Max results (default: 50)
  }
  deriving stock (Show, Eq, Generic)

-- | A single call site location.
data CallSite = CallSite
  { csFile :: Text
  , csLine :: Int
  , csColumn :: Int
  , csContext :: Text            -- ^ The line containing the call
  , csContextBefore :: [Text]    -- ^ Lines before
  , csContextAfter :: [Text]     -- ^ Lines after
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON CallSite where
  toJSON cs = object
    [ "file" .= csFile cs
    , "line" .= csLine cs
    , "column" .= csColumn cs
    , "context" .= csContext cs
    , "context_before" .= csContextBefore cs
    , "context_after" .= csContextAfter cs
    ]

-- | Result of find_callers tool.
data FindCallersResult = FindCallersResult
  { fcrName :: Text
  , fcrDefinitionFile :: Maybe Text
  , fcrDefinitionLine :: Maybe Int
  , fcrCallSites :: [CallSite]
  , fcrFilteredCount :: Int      -- ^ How many references were filtered out
  , fcrTruncated :: Bool
  , fcrWarning :: Maybe Text     -- ^ Warning if HLS is still indexing
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON FindCallersResult where
  toJSON fcr = object
    [ "name" .= fcrName fcr
    , "definition_file" .= fcrDefinitionFile fcr
    , "definition_line" .= fcrDefinitionLine fcr
    , "call_sites" .= fcrCallSites fcr
    , "filtered_count" .= fcrFilteredCount fcr
    , "truncated" .= fcrTruncated fcr
    , "warning" .= fcrWarning fcr
    ]

-- ════════════════════════════════════════════════════════════════════════════
-- FIND-CALLEES
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for find_callees tool.
data FindCalleesArgs = FindCalleesArgs
  { fceName :: Text              -- ^ Function name to find callees of
  , fceMaxResults :: Maybe Int   -- ^ Max results (default: 50)
  }
  deriving stock (Show, Eq, Generic)

-- | Information about a called function (callee).
data CalleeInfo = CalleeInfo
  { ciName :: Text
  , ciFile :: Text
  , ciLine :: Int
  , ciCallSites :: [Position]    -- ^ Where in the source function it is called
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON CalleeInfo where
  toJSON ci = object
    [ "name" .= ciName ci
    , "file" .= ciFile ci
    , "line" .= ciLine ci
    , "call_sites" .= ciCallSites ci
    ]

-- | Result of find_callees tool.
data FindCalleesResult = FindCalleesResult
  { fceResultName :: Text
  , fceDefinitionFile :: Maybe Text
  , fceDefinitionLine :: Maybe Int
  , fceCallees :: [CalleeInfo]
  , fceTruncated :: Bool
  , fceWarning :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON FindCalleesResult where
  toJSON fce = object
    [ "name" .= fceResultName fce
    , "definition_file" .= fceDefinitionFile fce
    , "definition_line" .= fceDefinitionLine fce
    , "callees" .= fceCallees fce
    , "truncated" .= fceTruncated fce
    , "warning" .= fceWarning fce
    ]

-- ════════════════════════════════════════════════════════════════════════════
-- SHOW-TYPE
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for show_type tool.
data ShowTypeArgs = ShowTypeArgs
  { staTypeName :: Text          -- ^ Type name to inspect
  }
  deriving stock (Show, Eq, Generic)

-- | A field in a type (for records).
data TypeField = TypeField
  { tfName :: Text
  , tfType :: Text
  , tfStrict :: Bool             -- ^ Has ! strictness annotation
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON TypeField where
  toJSON tf = object
    [ "name" .= tfName tf
    , "type" .= tfType tf
    , "strict" .= tfStrict tf
    ]

-- | A constructor in a type.
data TypeConstructor = TypeConstructor
  { tcName :: Text
  , tcFields :: [Text]           -- ^ Field types (positional or record)
  , tcIsRecord :: Bool           -- ^ Uses record syntax
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON TypeConstructor where
  toJSON tc = object
    [ "name" .= tcName tc
    , "fields" .= tcFields tc
    , "is_record" .= tcIsRecord tc
    ]

-- | Result of show_type tool.
data ShowTypeResult = ShowTypeResult
  { strTypeName :: Text
  , strFile :: Maybe Text
  , strLine :: Maybe Int
  , strTypeKind :: Text          -- ^ "record", "sum", "gadt", or "newtype"
  , strFields :: [TypeField]     -- ^ Non-empty for records
  , strConstructors :: [TypeConstructor]
  , strRawDefinition :: Text
  , strWarning :: Maybe Text     -- ^ Warning if HLS is still indexing
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ShowTypeResult where
  toJSON str = object
    [ "type_name" .= strTypeName str
    , "file" .= strFile str
    , "line" .= strLine str
    , "type_kind" .= strTypeKind str
    , "fields" .= strFields str
    , "constructors" .= strConstructors str
    , "raw_definition" .= strRawDefinition str
    , "warning" .= strWarning str
    ]

-- ════════════════════════════════════════════════════════════════════════════
-- SHOW-FIELDS
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for show_fields tool.
data ShowFieldsArgs = ShowFieldsArgs
  { sfaTypeName :: Text          -- ^ Record type name
  }
  deriving stock (Show, Eq, Generic)

-- | A single record field.
data RecordField = RecordField
  { rfName :: Text
  , rfType :: Text
  , rfStrict :: Bool             -- ^ Has ! strictness annotation
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON RecordField where
  toJSON rf = object
    [ "name" .= rfName rf
    , "type" .= rfType rf
    , "strict" .= rfStrict rf
    ]

-- | Result of show_fields tool.
data ShowFieldsResult = ShowFieldsResult
  { sfrTypeName :: Text
  , sfrFile :: Maybe Text
  , sfrLine :: Maybe Int
  , sfrIsRecord :: Bool
  , sfrFields :: [RecordField]
  , sfrRawDefinition :: Text     -- ^ Raw definition text for reference
  , sfrWarning :: Maybe Text     -- ^ Warning if HLS is still indexing
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ShowFieldsResult where
  toJSON sfr = object
    [ "type_name" .= sfrTypeName sfr
    , "file" .= sfrFile sfr
    , "line" .= sfrLine sfr
    , "is_record" .= sfrIsRecord sfr
    , "fields" .= sfrFields sfr
    , "raw_definition" .= sfrRawDefinition sfr
    , "warning" .= sfrWarning sfr
    ]

-- ════════════════════════════════════════════════════════════════════════════
-- SHOW-CONSTRUCTORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for show_constructors tool.
data ShowConstructorsArgs = ShowConstructorsArgs
  { scaTypeName :: Text          -- ^ Type name
  }
  deriving stock (Show, Eq, Generic)

-- | A single constructor.
data Constructor = Constructor
  { conName :: Text
  , conFields :: [Text]          -- ^ Field types (positional or record)
  , conIsRecord :: Bool          -- ^ Uses record syntax
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON Constructor where
  toJSON con = object
    [ "name" .= conName con
    , "fields" .= conFields con
    , "is_record" .= conIsRecord con
    ]

-- | Result of show_constructors tool.
data ShowConstructorsResult = ShowConstructorsResult
  { scrTypeName :: Text
  , scrFile :: Maybe Text
  , scrLine :: Maybe Int
  , scrConstructors :: [Constructor]
  , scrIsGADT :: Bool
  , scrRawDefinition :: Text
  , scrWarning :: Maybe Text     -- ^ Warning if HLS is still indexing
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ShowConstructorsResult where
  toJSON scr = object
    [ "type_name" .= scrTypeName scr
    , "file" .= scrFile scr
    , "line" .= scrLine scr
    , "constructors" .= scrConstructors scr
    , "is_gadt" .= scrIsGADT scr
    , "raw_definition" .= scrRawDefinition scr
    , "warning" .= scrWarning scr
    ]
