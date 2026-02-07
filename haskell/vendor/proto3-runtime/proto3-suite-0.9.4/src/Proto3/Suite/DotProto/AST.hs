-- | Minimal AST types for proto3-suite runtime.
--
-- This is a stripped-down version of the original DotProto.AST that provides
-- only the type definitions needed by Proto3.Suite.Class (the Message typeclass).
-- All Arbitrary, Pretty, and Data instances are removed. Turtle dependency
-- is eliminated. These types exist so that generated dotProto method
-- implementations compile, but they are never evaluated at runtime.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Proto3.Suite.DotProto.AST
  ( -- * Types
    MessageName(..)
  , FieldName(..)
  , PackageName(..)
  , DotProtoIdentifier(..)
  , DotProtoImport(..)
  , DotProtoImportQualifier(..)
  , DotProtoPackageSpec(..)
  , DotProtoOption(..)
  , DotProtoDefinition(..)
  , DotProtoMeta(..)
  , DotProto(..)
  , DotProtoValue(..)
  , DotProtoPrimType(..)
  , Packing(..)
  , Path(..), fakePath
  , DotProtoType(..)
  , DotProtoEnumValue
  , DotProtoEnumPart(..)
  , Streaming(..)
  , DotProtoServicePart(..)
  , RPCMethod(..)
  , DotProtoMessagePart(..)
  , DotProtoField(..)
  , DotProtoReservedField(..)
  ) where

import Data.Int (Int32)
import qualified Data.List.NonEmpty as NE
import Data.String (IsString(..))
import GHC.Generics (Generic)
import Proto3.Wire.Types (FieldNumber(..))

newtype MessageName = MessageName
  { getMessageName :: String }
  deriving (Eq, Generic, IsString, Ord)

instance Show MessageName where
  show = show . getMessageName

newtype FieldName = FieldName
  { getFieldName :: String }
  deriving (Eq, Generic, IsString, Ord)

instance Show FieldName where
  show = show . getFieldName

newtype PackageName = PackageName
  { getPackageName :: String }
  deriving (Eq, Generic, IsString, Ord)

instance Show PackageName where
  show = show . getPackageName

newtype Path = Path
  { components :: NE.NonEmpty String }
  deriving (Eq, Generic, Ord, Show)

fakePath :: Path
fakePath = Path ("fakePath" NE.:| [])

data DotProtoIdentifier
  = Single String
  | Dots   Path
  | Qualified DotProtoIdentifier DotProtoIdentifier
  | Anonymous
  deriving (Eq, Generic, Ord, Show)

data DotProtoImport = DotProtoImport
  { dotProtoImportQualifier :: DotProtoImportQualifier
  , dotProtoImportPath      :: FilePath
  }
  deriving (Eq, Generic, Ord, Show)

data DotProtoImportQualifier
  = DotProtoImportPublic
  | DotProtoImportWeak
  | DotProtoImportDefault
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)

data DotProtoPackageSpec
  = DotProtoPackageSpec DotProtoIdentifier
  | DotProtoNoPackage
  deriving (Eq, Generic, Ord, Show)

data DotProtoOption = DotProtoOption
  { dotProtoOptionIdentifier :: DotProtoIdentifier
  , dotProtoOptionValue      :: DotProtoValue
  } deriving (Eq, Generic, Ord, Show)

data DotProtoDefinition
  = DotProtoMessage String DotProtoIdentifier [DotProtoMessagePart]
  | DotProtoEnum    String DotProtoIdentifier [DotProtoEnumPart]
  | DotProtoService String DotProtoIdentifier [DotProtoServicePart]
  deriving (Eq, Generic, Ord, Show)

data DotProtoMeta = DotProtoMeta
  { metaModulePath :: Path
  } deriving (Eq, Generic, Ord, Show)

data DotProto = DotProto
  { protoImports     :: [DotProtoImport]
  , protoOptions     :: [DotProtoOption]
  , protoPackage     :: DotProtoPackageSpec
  , protoDefinitions :: [DotProtoDefinition]
  , protoMeta        :: DotProtoMeta
  } deriving (Eq, Generic, Ord, Show)

data DotProtoValue
  = Identifier DotProtoIdentifier
  | StringLit  String
  | IntLit     Int
  | FloatLit   Double
  | BoolLit    Bool
  deriving (Eq, Generic, Ord, Show)

data DotProtoPrimType
  = Int32
  | Int64
  | SInt32
  | SInt64
  | UInt32
  | UInt64
  | Fixed32
  | Fixed64
  | SFixed32
  | SFixed64
  | String
  | Bytes
  | Bool
  | Float
  | Double
  | Named DotProtoIdentifier
  deriving (Eq, Generic, Ord, Show)

data Packing
  = PackedField
  | UnpackedField
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)

data DotProtoType
  = Prim           DotProtoPrimType
  | Optional       DotProtoPrimType
  | Repeated       DotProtoPrimType
  | NestedRepeated DotProtoPrimType
  | Map            DotProtoPrimType DotProtoPrimType
  deriving (Eq, Generic, Ord, Show)

type DotProtoEnumValue = Int32

data DotProtoEnumPart
  = DotProtoEnumField DotProtoIdentifier DotProtoEnumValue [DotProtoOption]
  | DotProtoEnumOption DotProtoOption
  | DotProtoEnumReserved   [DotProtoReservedField]
  deriving (Eq, Generic, Ord, Show)

data Streaming
  = Streaming
  | NonStreaming
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)

data DotProtoServicePart
  = DotProtoServiceRPCMethod RPCMethod
  | DotProtoServiceOption DotProtoOption
  deriving (Eq, Generic, Ord, Show)

data RPCMethod = RPCMethod
  { rpcMethodName :: DotProtoIdentifier
  , rpcMethodRequestType :: DotProtoIdentifier
  , rpcMethodRequestStreaming :: Streaming
  , rpcMethodResponseType :: DotProtoIdentifier
  , rpcMethodResponseStreaming :: Streaming
  , rpcMethodOptions :: [DotProtoOption]
  }
  deriving (Eq, Generic, Ord, Show)

data DotProtoMessagePart
  = DotProtoMessageField DotProtoField
  | DotProtoMessageOneOf DotProtoIdentifier [DotProtoField]
  | DotProtoMessageDefinition DotProtoDefinition
  | DotProtoMessageReserved   [DotProtoReservedField]
  | DotProtoMessageOption DotProtoOption
  deriving (Eq, Generic, Ord, Show)

data DotProtoField = DotProtoField
  { dotProtoFieldNumber  :: FieldNumber
  , dotProtoFieldType    :: DotProtoType
  , dotProtoFieldName    :: DotProtoIdentifier
  , dotProtoFieldOptions :: [DotProtoOption]
  , dotProtoFieldComment :: String
  }
  deriving (Eq, Generic, Ord, Show)

data DotProtoReservedField
  = SingleField Int
  | FieldRange  Int Int
  | ReservedIdentifier String
  deriving (Eq, Generic, Ord, Show)
