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
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Presents right-associative folds as 'Foldable' sequences.
module Proto3.Wire.FoldR
  ( FoldR(..)
  , fromFoldR
  ) where

import Data.Foldable qualified
import Data.Kind (Type)
import GHC.Exts (TYPE)
import GHC.Exts qualified
import Text.Read (Read(..))

-- | Presents a right-associative fold as a 'Foldable' sequence.
--
-- Similar to the @FRList@ example in the documentation for "Data.Foldable",
-- but for generality the element type may be unlifted.  For compatibility
-- with the 'Foldable' type class and to avoid limitations on runtime
-- representation polymorphism, the fold supports only lifted results.
newtype FoldR (a :: TYPE r) = FoldR { applyFoldR :: forall (b :: Type) . (a -> b -> b) -> b -> b }
  deriving stock (Functor)

instance Foldable FoldR
  where
    foldr f z xs = applyFoldR xs f z

instance GHC.Exts.IsList (FoldR a)
  where
    type Item (FoldR a) = a
    fromList = fromFoldR
    toList = Data.Foldable.toList

instance Eq a =>
         Eq (FoldR a)
  where
    x == y = GHC.Exts.toList x == GHC.Exts.toList y

instance Ord a =>
         Ord (FoldR a)
  where
    x <= y = GHC.Exts.toList x <= GHC.Exts.toList y
    x `compare` y = GHC.Exts.toList x `compare` GHC.Exts.toList y

instance Read a =>
         Read (FoldR a)
  where
    readPrec = fmap GHC.Exts.fromList readPrec

instance Show a =>
         Show (FoldR a)
  where
    showsPrec d = showsPrec d . GHC.Exts.toList

-- | Creates a 'FoldR' from the 'foldr' of the given 'Foldable' sequence.
fromFoldR :: Foldable t => t a -> FoldR a
fromFoldR xs = FoldR { applyFoldR = \f z -> foldr f z xs }
