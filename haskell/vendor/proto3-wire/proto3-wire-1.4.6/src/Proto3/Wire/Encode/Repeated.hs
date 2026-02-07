{-
  Copyright 2025 Arista Networks

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

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | Presents right-associative folds as 'Foldable' sequences.
module Proto3.Wire.Encode.Repeated
  ( Repeated(..)
  , nullRepeated
  , ToRepeated(..)
  , mapRepeated
  ) where

import Data.Functor.Identity (Identity(..))
import Data.IntMap.Lazy qualified
import Data.IntSet qualified
import Data.Kind (Type)
import Data.List.NonEmpty qualified
import Data.Map.Lazy qualified
import Data.Sequence qualified
import Data.Set qualified
import Data.Vector qualified
import Data.Vector.Storable qualified
import Data.Vector.Unboxed qualified
import Foreign (Storable)
import GHC.Exts (Constraint, TYPE)
import GHC.Generics (Generic)
import Proto3.Wire.FoldR (FoldR(..), fromFoldR)

-- | Expresses a sequence of values /in reverse order/ for encoding as a repeated field.
type Repeated :: forall er . TYPE er -> Type
data Repeated e = ReverseRepeated
  { countRepeated :: Maybe Int
      -- ^ Optionally predicts the number of elements in the sequence.  Predict
      -- the count only when it is practical to do so accurately and quickly.
      --
      -- A prediction that is too low causes undefined behavior--possibly
      -- a crash.  A length prediction that is too high overallocates
      -- output space, as if the sequence really were that length.
  , reverseRepeated :: FoldR e
      -- ^ A lazy right-associative fold over the /reverse/
      -- of the desired sequence of field values.
      --
      -- Design Note: We could have used a lazy left-associative fold, but
      -- vectors perform such folds using a left-to-right iteration, instead
      -- of the right-to-left iteration that would yield best performance.
      --
      -- Therefore in order to avoid accidental misuse of 'foldl', we ask
      -- for sequence reversal explicitly.  Thanks to vector fusion rules,
      -- it is fast to 'foldr' on the result of reversing a vector.
  }
  deriving stock (Functor, Generic)

deriving stock instance Eq e => Eq (Repeated e)
deriving stock instance Read e => Read (Repeated e)
deriving stock instance Show e => Show (Repeated e)

nullRepeated :: Repeated e -> Bool
nullRepeated c = null (reverseRepeated c)
{-# INLINE nullRepeated #-}

-- | For each container type, specifies the optimal method for reverse iteration.
type ToRepeated :: forall cr . TYPE cr -> forall er . TYPE er -> Constraint
class ToRepeated c e | c -> e
  where
    -- | Converts to a reverse iteration over the elements.
    toRepeated :: c -> Repeated e

instance forall er (e :: TYPE er) .
         ToRepeated (Repeated e) e
  where
    toRepeated = id
    {-# INLINE toRepeated #-}

instance ToRepeated (Identity a) a
  where
    toRepeated x = ReverseRepeated (Just 1) (FoldR (\f z -> f (runIdentity x) z))
    {-# INLINE toRepeated #-}

instance ToRepeated [a] a
  where
    toRepeated xs = ReverseRepeated Nothing (FoldR (\f z -> foldl (flip f) z xs))
      -- Unavoidably reads to the end of the list before presenting the last element.
    {-# INLINE toRepeated #-}

instance ToRepeated (Data.List.NonEmpty.NonEmpty a) a
  where
    toRepeated xs = ReverseRepeated Nothing (FoldR (\f z -> foldl (flip f) z xs))
      -- Unavoidably reads to the end of the list before presenting the last element.
    {-# INLINE toRepeated #-}

instance ToRepeated (Data.Vector.Vector a) a
  where
    toRepeated xs = ReverseRepeated
      (Just (Data.Vector.length xs))
      (fromFoldR (Data.Vector.reverse xs))
      -- Vector fusion should convert this to right-to-left iteration.
    {-# INLINE toRepeated #-}

instance Storable a =>
         ToRepeated (Data.Vector.Storable.Vector a) a
  where
    toRepeated xs = ReverseRepeated
      (Just (Data.Vector.Storable.length xs))
      (FoldR (\f z -> Data.Vector.Storable.foldr f z (Data.Vector.Storable.reverse xs)))
      -- Vector fusion should convert this to right-to-left iteration.
    {-# INLINE toRepeated #-}

instance Data.Vector.Unboxed.Unbox a =>
         ToRepeated (Data.Vector.Unboxed.Vector a) a
  where
    toRepeated xs = ReverseRepeated
      (Just (Data.Vector.Unboxed.length xs))
      (FoldR (\f z -> Data.Vector.Unboxed.foldr f z (Data.Vector.Unboxed.reverse xs)))
      -- Vector fusion should convert this to right-to-left iteration.
    {-# INLINE toRepeated #-}

instance ToRepeated (Data.Sequence.Seq a) a
  where
    toRepeated xs = ReverseRepeated
      (Just (Data.Sequence.length xs))
      (FoldR (\f z -> foldl (flip f) z xs))
      -- Should present the last element without having to read through the whole sequence.
    {-# INLINE toRepeated #-}

instance ToRepeated (Data.Set.Set a) a
  where
    toRepeated xs = ReverseRepeated
      (Just (Data.Set.size xs))
      (FoldR (\f z -> foldl (flip f) z xs))
      -- Should present the last element without having to read through the whole sequence.
    {-# INLINE toRepeated #-}

instance ToRepeated Data.IntSet.IntSet Int
  where
    toRepeated xs = ReverseRepeated Nothing (FoldR (\f z -> Data.IntSet.foldl (flip f) z xs))
      -- Should present the last element without having to read through the whole sequence.
    {-# INLINE toRepeated #-}

instance ToRepeated (Data.Map.Lazy.Map k a) (k, a)
  where
    toRepeated xs = ReverseRepeated
      (Just (Data.Map.Lazy.size xs))
      (FoldR (\f z -> Data.Map.Lazy.foldlWithKey (\a k v -> f (k, v) a) z xs))
      -- Should present the last key-value pair without having to read through the whole map.
    {-# INLINE toRepeated #-}

instance ToRepeated (Data.IntMap.Lazy.IntMap a) (Int, a)
  where
    toRepeated xs = ReverseRepeated
      Nothing
      (FoldR (\f z -> Data.IntMap.Lazy.foldlWithKey (\a k v -> f (k, v) a) z xs))
      -- Should present the last key-value pair without having to read through the whole map.
    {-# INLINE toRepeated #-}

-- | A convenience function that maps a function over a sequence,
-- provided that the relevant types are all lifted.
mapRepeated ::
  forall (c :: Type) (e :: Type) (a :: Type) . ToRepeated c e => (e -> a) -> c -> Repeated a
mapRepeated f xs = fmap f (toRepeated xs)
{-# INLINE mapRepeated #-}
