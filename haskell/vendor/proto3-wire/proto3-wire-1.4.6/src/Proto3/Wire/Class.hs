{-# LANGUAGE DefaultSignatures #-}
{-
  Copyright 2019 Awake Networks

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
-}

-- | This module defines classes which are shared by the encoding and decoding
-- modules.

module Proto3.Wire.Class
  ( ProtoEnum(..)
  ) where

import           Data.Int (Int32)
import qualified Safe

-- | Similar to 'Enum', but allows gaps in the sequence of numeric codes,
-- and uses 'Int32' in order to match the proto3 specification.
--
-- Absent gaps, you can use an automatic derivation of 'Bounded' and 'Enum',
-- then use the default implementations for all methods of this class.  But
-- if gaps are involved, then you must instantiate this class directly and
-- supply the specific numeric codes desired for each constructor.
class ProtoEnum a where
    -- | Default implementation: `Safe.toEnumMay`.
    toProtoEnumMay :: Int32 -> Maybe a
    default toProtoEnumMay :: (Bounded a, Enum a) => Int32 -> Maybe a
    toProtoEnumMay = Safe.toEnumMay . fromIntegral

    -- | Default implementation: 'fromEnum'.
    fromProtoEnum :: a -> Int32
    default fromProtoEnum :: Enum a => a -> Int32
    fromProtoEnum = fromIntegral . fromEnum
