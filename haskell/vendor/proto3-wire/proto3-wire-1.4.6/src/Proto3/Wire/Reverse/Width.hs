{-
  Copyright 2020 Awake Networks

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
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Augmentations to type classes such as 'Semigroup' and 'Monoid' that may
-- be used to track the type-level width information of builder primitives.
module Proto3.Wire.Reverse.Width
  ( AssocPlusNat(..)
  , CommPlusNat(..)
  , PChoose(..)
  , Max
  , AssocMaxNat(..)
  , CommMaxNat(..)
  ) where

import Data.Type.Bool (If)
import GHC.Exts (Proxy#)
import GHC.TypeLits (type (<=?), type (+))
import Parameterized.Data.Semigroup (PNullary)

-- | Associativity of '+' in type parameters.
class AssocPlusNat n u v w
  where
    assocLPlusNat ::
      Proxy# '(u, v, w) ->
      PNullary n (u + (v + w)) ->
      PNullary n ((u + v) + w)

    assocRPlusNat ::
      Proxy# '(u, v, w) ->
      PNullary n ((u + v) + w) ->
      PNullary n (u + (v + w))

-- | Commutativity of '+' in type parameters.
class CommPlusNat n u v
  where
    commPlusNat ::
      Proxy# '(u, v) ->
      PNullary n (u + v) ->
      PNullary n (v + u)

-- | Chooses between alternatives based on a condition,
-- adjusting a type-level parameter appropriately.
--
-- Note that while this type class makes sense for bounded builder primitives,
-- it should not be instantiated for fixed-width primitives of differing
-- widths (at least, not without padding to equalize the widths) because
-- the choice between alternatives introduces a run-time variation in width.
-- Instead please use ordinary `Data.Bool.bool` or @if _ then _ else _@.
class PChoose n f t w | f t -> w
  where
    -- | Like `Data.Bool.bool`, chooses the first argument on 'False'
    -- and the second on 'True', either way promoting the type-level
    -- `GHC.TypeLits.Nat` to the larger of the given `GHC.TypeLits.Nat`s.
    --
    -- Defaults to the natural implementation in terms of 'pif'.
    pbool :: PNullary n f -> PNullary n t -> Bool -> PNullary n w
    pbool f t b = pif b t f
    {-# INLINE CONLIKE pbool #-}

    -- | Like @if _ then _ else@, chooses the first argument on 'True'
    -- and the second on 'False', either way promoting the type-level
    -- `GHC.TypeLits.Nat` to the larger of the given `GHC.TypeLits.Nat`s.
    --
    -- Defaults to the natural implementation in terms of 'pbool'.
    pif :: Bool -> PNullary n t -> PNullary n f -> PNullary n w
    pif c t e = pbool e t c
    {-# INLINE CONLIKE pif #-}

    {-# MINIMAL pbool | pif #-}

-- | The larger of two `GHC.TypeLits.Nat`s.
type Max u v = If (v <=? u) u v

-- | Associativity of 'Max' in type parameters.
class AssocMaxNat n u v w
  where
    assocLMaxNat ::
      Proxy# '(u, v, w) ->
      PNullary n (Max u (Max v w)) ->
      PNullary n (Max (Max u v) w)

    assocRMaxNat ::
      Proxy# '(u, v, w) ->
      PNullary n (Max (Max u v) w) ->
      PNullary n (Max u (Max v w))

-- | Commutativity of 'Max' in type parameters.
class CommMaxNat n u v
  where
    commMaxNat ::
      Proxy# '(u, v) ->
      PNullary n (Max u v) ->
      PNullary n (Max v u)
