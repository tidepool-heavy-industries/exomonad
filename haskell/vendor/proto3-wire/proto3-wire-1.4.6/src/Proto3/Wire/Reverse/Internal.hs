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

-- | Implementation details of the "Data.ByteString.Reverse" module.
-- Breaking changes will be more frequent in this module; use with caution.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Proto3.Wire.Reverse.Internal
    ( BuildR (..)
    , BuildRState (..)
    , appendBuildR
    , foldlRVector
    , toBuildR
    , fromBuildR
    , etaBuildR
    , runBuildR
    , SealedState (SealedState, sealedSB, totalSB, stateVarSB, statePtrSB, recycledSB)
    , sealBuffer
    , smallChunkSize
    , readState
    , readSpace
    , writeState
    , writeSpace
    , metaDataSize
    , metaDataAlign
    , withUnused
    , withTotal
    , readTotal
    , withLengthOf
    , withLengthOf#
    , reallocate
    , prependChunk
    , ReverseChunks(..)
    , prependReverseChunks
    , ensure
    , ensure#
    , unsafeConsume
    , floatToWord32
    , doubleToWord64
    ) where

import           Control.Exception             ( bracket )
import           Control.Monad.Trans.State.Strict ( State, runState, state )
import qualified Data.ByteString               as B
import qualified Data.ByteString.Internal      as BI
import qualified Data.ByteString.Builder.Extra as BB
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Internal as BLI
import           Data.IORef                    ( IORef, newIORef,
                                                 readIORef, writeIORef )
import qualified Data.Primitive                as P
import qualified Data.Vector.Generic           as VG
import           Data.Vector.Generic           ( Vector )
import           Data.Word                     ( Word8, Word32, Word64 )
import           Foreign                       ( Storable(..),
                                                 castPtrToStablePtr,
                                                 castStablePtrToPtr,
                                                 freeStablePtr, newStablePtr,
                                                 deRefStablePtr )
import           GHC.Exts                      ( Addr#, Int#, MutVar#,
                                                 RealWorld, StablePtr#, State#,
                                                 addrToAny#, int2Addr#,
                                                 touch# )
import           GHC.ForeignPtr                ( ForeignPtr(..),
                                                 ForeignPtrContents(..) )
import           GHC.IO                        ( IO(..) )
import           GHC.IORef                     ( IORef(..) )
import           GHC.Int                       ( Int(..) )
import           GHC.Ptr                       ( Ptr(..), plusPtr )
import           GHC.Stable                    ( StablePtr(..) )
import           GHC.STRef                     ( STRef(..) )
import           System.IO.Unsafe              ( unsafePerformIO )

#if MIN_VERSION_primitive(0,7,0)
#define PTR P.Ptr
#else
#define PTR P.Addr
#endif

-- $setup
-- >>> :set -XOverloadedStrings

-- | Writes bytes in reverse order, updating the current state.
--
-- It is the responsibility of the execution context and buffer
-- management primitives to ensure that the current buffer remains
-- reachable during builder execution, though completed buffers
-- may be copied to new storage at any time.  Aside from those
-- primitives, 'BuildR' implementations may ignore that issue.
--
-- When combining `BuildR`s with '<>' we expect the best performance
-- when associating to the left.  For example @'foldl' ('<>') 'mempty'@,
-- though unless your 'foldl' iteration starts from the right there may
-- still be issues.  Consider using `Proto3.Wire.Reverse.vectorBuildR`
-- instead of 'foldMap'.
newtype BuildR = BuildR
  (Addr# -> Int# -> State# RealWorld -> (# Addr#, Int#, State# RealWorld #))
    -- ^ Both the builder arguments and the returned values are:
    --
    --   1. The starting address of the *used* portion of the current buffer.
    --
    --   2. The number of *unused* bytes in the current buffer.
    --
    --   3. The state token (which does not consume any machine registers).
    --
    -- It seems we cannot preserve register allocation between the arguments
    -- and the returned components, even by including padding.  If GHC were to
    -- allocate registers right-to-left (instead of the current left-to-right),
    -- and if it made sure to allocate the register that it uses for closure
    -- arguments *last* when allocating return registers, then we would stand
    -- a chance of not having to move the state components between registers.
    -- That way @a -> b -> 'BuildR'@ and 'BuildR' would use the same registers
    -- for state components as each other, and a non-inline return from one
    -- could be used to call the other without moving state components.
    --
    -- But in many cases register movements combine with increments.
    -- Also, we have arranged to put only the most frequently-used state
    -- components into registers, which reduces the costs of both moves
    -- and of save/reload pairs.  For example, our tracking of the total
    -- bytes written involves metadata at the start of the current buffer
    -- rather than an additional state register.

instance Semigroup BuildR
  where
    (<>) = appendBuildR
    {-# INLINE (<>) #-}

instance Monoid BuildR
  where
    mempty = BuildR (\v u s -> (# v, u, s #))
#ifdef ghcjs_HOST_OS
    {-# NOINLINE mempty #-}
#else
    {-# INLINE mempty #-}
#endif

    mappend = (<>)
    {-# INLINE mappend #-}

instance Show BuildR
  where
    showsPrec prec builder =
        showParen (prec > 10)
          (showString "Proto3.Wire.Reverse.lazyByteString " . shows bytes)
      where
        bytes = snd (runBuildR builder)

-- | Needed for rewrite rules; normally you would use '<>'.
appendBuildR  :: BuildR -> BuildR -> BuildR
appendBuildR = \b c ->
  let BuildR f = b
      BuildR g = c
  in
    BuildR (\v0 u0 s0 -> case g v0 u0 s0 of (# v1, u1, s1 #) -> f v1 u1 s1)
{-# INLINE CONLIKE [1] appendBuildR #-}

-- | Like 'foldl' but iterates right-to-left, which
-- is often useful when creating reverse builders.
foldlRVector :: Vector v a => (b -> a -> b) -> b -> v a -> b
foldlRVector f = \z v -> VG.foldr (flip f) z (VG.reverse v)
  -- It may look like we create a reversed vector here, but thanks to
  -- the rewrite rules in the vector library the vector is never actually
  -- allocated, and instead we directly stream elements from right to left.
{-# INLINE foldlRVector #-}

toBuildR :: (Ptr Word8 -> Int -> IO (Ptr Word8, Int)) -> BuildR
toBuildR f =
  BuildR $ \v0 u0 s0 ->
    let IO g = f (Ptr v0) (I# u0) in
    case g s0 of (# s1, (Ptr v1, I# u1) #) -> (# v1, u1, s1 #)

fromBuildR :: BuildR -> Ptr Word8 -> Int -> IO (Ptr Word8, Int)
fromBuildR (BuildR f) (Ptr v0) (I# u0) =
  IO $ \s0 -> case f v0 u0 s0 of (# v1, u1, s1 #) -> (# s1, (Ptr v1, I# u1) #)

-- | Eta-expands a function that produces a 'BuildR', so that
-- its input is not evaluated until the builder state is presented.
--
-- This odd combinator seems to help performance at times, though
-- it may change behavior on nonterminating values of type @a@.
etaBuildR :: (a -> BuildR) -> a -> BuildR
etaBuildR f x = toBuildR $ \v u -> fromBuildR (f x) v u

-- | The current state of execution of a builder
-- is a @'StablePtr' ('IORef' 'BuildRState')@.
--
-- Current Buffer Layout:
--
-- We devote the first few octets of the current buffer
-- to metadata that does not change very frequently:
--
--   * 'Addr#': cast to 'StablePtr# (IORef BuildRState)'; indirect acceses
--   to the full builder state, which is used relatively infrequently.
--
--   * 'Int#': the total number of non-metadata bytes originally available
--   within the current buffer before we started consuming them,
--   *plus* the number of bytes actually written to previous buffers.
--   We subtract the current unused figure to get the total bytes written.
--
--   * `GHC.Float.Double#`: suitably-aligned scratch space for serialization
--   of 'Double' and 'Float' values.
--
-- Though this choice imposes a small memory overhead in every buffer,
-- it reduces the time and space required to save and restore metadata
-- around the many non-inline calls that typically occur while writing
-- data into the buffer.
data BuildRState = BuildRState
  { currentBuffer :: {-# UNPACK #-}!(P.MutableByteArray RealWorld)
      -- ^ Specifies the current buffer.  Note that through this field
      -- every @'StablePtr' ('IORef' 'BuildRState')@ keeps one buffer
      -- reachable until that stable pointer is explicitly destroyed.
  , sealedBuffers :: BL.ByteString
      -- ^ Holds the bytes written to previous buffers.  We arrange for
      -- this field to be in normal form (not just weak head normal form).
      -- But to avoid redundant evaluation we do not mark it strict.
  }

-- | Allocates fields backward from offset 0 relative to some hypothetical
-- address, yielding the total size and alignment requirements, respectively,
-- along with the monadic return value.  The total size includes any padding
-- at the end that is needed to make it a multiple of the overall alignment.
allocateFields :: State (Int, Int) a -> (a, Int, Int)
allocateFields fields = (x, size, align)
  where
    (x, (off, align)) = runState fields (0, 1)
    size = mod off align - off
      -- Aligns the overall size @- off@ by prepending @mod off align@ padding
      -- bytes.  Because @mod off align@ is in @[0, align)@ we are are neither
      -- removing bytes nor adding more than we need.  And for some @k@ we have
      --
      -- > mod off align == off + k * align
      --
      -- and therefore we achieve the precise alignment desired:
      --
      -- > size = (off + k * align) - off == k * align

-- | Within the monadic context established by 'allocateFields',
-- allocates one suitably-aligned field and returns its offset.
-- The argument is only a proxy for its type; we never evaluate it,
-- and therefore you may pass 'undefined'.
--
-- WARNING: We assume that 'max' is the same as 'lcm' for any pair of
-- alignment values, so that we can avoid using 'lcm', which does not
-- evaluate at compile time.  Compile-time evaluation helps our speed.
allocatePrimitiveField :: Storable a => a -> State (Int, Int) Int
allocatePrimitiveField proxy = state $ \(prevOff, prevAlign) ->
  let fieldWidth = sizeOf proxy
      fieldAlign = alignment proxy
      unaligned = prevOff - fieldWidth
      nextOff = unaligned - mod unaligned fieldAlign
      nextAlign = max prevAlign fieldAlign
  in (nextOff, (nextOff, nextAlign))

scratchOffset, spaceOffset, stateOffset, metaDataSize, metaDataAlign :: Int
((scratchOffset, spaceOffset, stateOffset), metaDataSize, metaDataAlign) =
  allocateFields $
    (,,) <$> allocatePrimitiveField (undefined :: Double)
         <*> allocatePrimitiveField (undefined :: Int)
         <*> allocatePrimitiveField (undefined :: Ptr ())
               -- Note that we are allocating backward, so this
               -- will put the pointer at the lowest address.

smallChunkSize, defaultChunkSize :: Int
smallChunkSize = BB.smallChunkSize - metaDataSize
defaultChunkSize = BB.defaultChunkSize - metaDataSize

data MetaData

metaPtr :: Ptr Word8 -> Int -> Ptr MetaData
metaPtr v = plusPtr v . negate

readState :: Ptr MetaData -> IO (StablePtr (IORef BuildRState))
readState m = castPtrToStablePtr <$> peekByteOff m stateOffset

writeState :: Ptr MetaData -> StablePtr (IORef BuildRState) -> IO ()
writeState m = pokeByteOff m stateOffset . castStablePtrToPtr

readSpace :: Ptr MetaData -> IO Int
readSpace m = peekByteOff m spaceOffset

writeSpace :: Ptr MetaData -> Int -> IO ()
writeSpace m = pokeByteOff m spaceOffset

-- | The arguments are the same as the 'BuildR' arguments.
readTotal :: Ptr Word8 -> Int -> IO Int
readTotal v unused = do
  -- Because we do not wish to update a record of the total
  -- every time we write a byte, instead we record "space",
  -- which changes rarely, and subtract "unused" from it
  -- when we need to compute the total, which is somewhat
  -- frequent but not as frequent as updates to "unused".
  space <- readSpace (metaPtr v unused)
  let !total = space - unused

  -- GHC (at least v8.2.2 and v8.6.5) seems quite eager to delay the above
  -- subtraction, even though we have indicated that the computation of
  -- "total" is strict, and even though delaying the subtraction across
  -- a non-inline call requires saving and restoring two registers
  -- ("space" and "unused") instead of one ("total").  Unless we were to
  -- completely ignore the result of the subtraction, which would be quite
  -- unusual, an eager subtraction is faster.  Therefore we force it:
  strictify total

-- | Sometimes GHC (at least v8.2.2 and v8.6.5) appears to be lazy even with
-- unlifted values, and we apply this combination to force computation so that
-- we do not have to save and restore the several inputs to the computation.
--
-- The implementation requires converting the 'Int#' to a lifted pointer
-- type and then invoking 'touch#' on it, which is slightly questionable
-- because we would crash if the garbage collector actually followed the
-- converted value.  But there would be no reason to collect between the
-- conversion and the 'touch#' because that span involves no computation.
strictify :: Int -> IO Int
strictify (I# x) = IO $ \s0 ->
  case addrToAny# (int2Addr# x) of
    (# y #) -> case touch# y s0 of
      s1 -> (# s1, I# x #)

-- | Allocates a new buffer and stores a pointer to that buffer in
-- the 'currentBuffer' field of the overall builder state, along with the
-- first argument, then returns a pointer to the end of the payload area.
--
-- (This is a manual wrapper around 'newBuffer#'.)
newBuffer ::
  -- | All bytes previously written.
  --
  -- It is ASSUMED that the caller already fully
  -- evaluated this otherwise-lazy 'BL.ByteString'.
  BL.ByteString ->
  -- | Total number of bytes previously written.
  Int ->
  -- | Builder state variable.  The old value of this variable
  -- will NOT be used; rather, it will be overwritten.
  -- Therefore that old value may be 'undefined'.
  IORef BuildRState ->
  -- | Stable pointer to builder state variable.
  StablePtr (IORef BuildRState) ->
  -- | Desired payload size of new current buffer, not counting metadata.
  Int ->
  IO (Ptr Word8)
newBuffer sealed (I# total) (IORef (STRef stateVar)) (StablePtr stateSP)
          (I# unused) =
  IO $ \s0 ->
    case newBuffer# sealed total stateVar stateSP unused s0 of
      (# s1, addr #) -> (# s1, Ptr addr #)

newBuffer# ::
  BL.ByteString ->
  Int# ->
  MutVar# RealWorld BuildRState ->
  StablePtr# (IORef BuildRState) ->
  Int# ->
  State# RealWorld ->
  (# State# RealWorld, Addr# #)
newBuffer# sealed total stateVar stateSP unused s0 =
    case go s0 of
      (# s1, Ptr addr #) -> (# s1, addr #)
  where
    IO go = do
      let allocation = metaDataSize + I# unused
      buf <- P.newAlignedPinnedByteArray allocation metaDataAlign
      let !(PTR base) = P.mutableByteArrayContents buf
          !v = plusPtr (Ptr base) (metaDataSize + I# unused)
          !m = plusPtr (Ptr base) metaDataSize
      writeState m (StablePtr stateSP)
      writeSpace m (I# unused + I# total)
      let !nextState = BuildRState{currentBuffer = buf, sealedBuffers = sealed}
      writeIORef (IORef (STRef stateVar)) nextState
      pure v

-- | The result of a call to 'sealBuffer'.
data SealedState = SealedState
  { sealedSB :: BL.ByteString
      -- ^ All bytes written thus far.
  , totalSB :: {-# UNPACK #-}!Int
      -- ^ The total number of bytes written thus far.
  , stateVarSB :: {-# UNPACK #-}!(IORef BuildRState)
      -- ^ The builder state variable.
      -- This function does NOT modify that variable--it will still
      -- refer to the old buffer unless and until you modify it.
  , statePtrSB :: {-# UNPACK #-}!(StablePtr (IORef BuildRState))
      -- ^ The stable pointer to the variable referenced by 'stateVarSB'.
  , recycledSB :: Maybe (P.MutableByteArray RealWorld)
      -- ^ Returns ownership of the old current buffer to the caller
      -- if it is no longer needed to track the already-written bytes.
      --
      -- If you reuse it within the same builder then there is
      -- no need to reset the stable pointer to the state variable,
      -- but please be sure to update the "space" metadatum.
  }

-- | Takes ownership of the current buffer,
-- but sometimes hands it back for reuse.
--
-- If more building is required then please allocate a new current buffer
-- and update the builder state variable accordingly.
--
-- (This is a manual wrapper around 'sealBuffer#'.)
sealBuffer ::
  -- | Pointer to the used portion of the current buffer.
  Ptr Word8 ->
  -- | The number of bytes still unused in the current buffer.
  Int ->
  IO SealedState
sealBuffer (Ptr addr) (I# u) = IO $ \s0 ->
  case sealBuffer# addr u s0 of
    (# s1, sealed, total, stateVar, statePtr, recycled #) ->
      (# s1
       , SealedState
           { sealedSB = sealed
           , totalSB = I# total
           , stateVarSB = IORef (STRef stateVar)
           , statePtrSB = StablePtr statePtr
           , recycledSB = recycled
           }
       #)

sealBuffer# ::
  Addr# ->
  Int# ->
  State# RealWorld ->
  (# State# RealWorld
   , BL.ByteString
   , Int#
   , MutVar# RealWorld BuildRState
   , StablePtr# (IORef BuildRState)
   , Maybe (P.MutableByteArray RealWorld)
   #)
sealBuffer# addr unused s0 =
    case go s0 of
      (# s1, (sealed, I# total, IORef (STRef sv), StablePtr sp, re) #) ->
        (# s1, sealed, total, sv, sp, re #)
  where
    IO go = do
      let v = Ptr addr
      statePtr <- readState (metaPtr v (I# unused))
      stateVar <- deRefStablePtr statePtr
      BuildRState { currentBuffer = buffer, sealedBuffers = oldSealed } <-
       readIORef stateVar
      total <- readTotal v (I# unused)
      -- The above call to 'readTotal' is the last access of the current
      -- buffer through a raw pointer made by this function.  Therefore
      -- we must be sure that the current buffer remains reachable at this
      -- point in the state thread.  And we are sure of that fact, because
      -- until a state action frees the stable pointer or modifies the state
      -- variable, the stable pointer will reference the state variable,
      -- which in turn will reference the current buffer.
      allocation <- subtract metaDataSize <$> P.getSizeofMutableByteArray buffer
      if allocation <= I# unused
        then
          pure (oldSealed, total, stateVar, statePtr, Just buffer)
        else do
          let !(PTR base) = P.mutableByteArrayContents buffer
              !(P.MutableByteArray mba) = buffer
              fp = ForeignPtr base (PlainPtr mba)
              offset = metaDataSize + I# unused
              finish trimmed recycled = do
                let !newSealed = BLI.Chunk trimmed oldSealed
                pure (newSealed, total, stateVar, statePtr, recycled)
              untrimmed = BI.fromForeignPtr fp offset (allocation - I# unused)
          if offset <= B.length untrimmed
            then finish untrimmed Nothing
            else finish (B.copy untrimmed) (Just buffer)

-- | Like `Proto3.Wire.Reverse.toLazyByteString` but also
-- returns the total length of the lazy 'BL.ByteString',
-- which is computed as a side effect of encoding.
runBuildR :: BuildR -> (Int, BL.ByteString)
runBuildR f = unsafePerformIO $ do
  stateVar <- newIORef undefined   -- undefined only until 'newBuffer'
  bracket (newStablePtr stateVar) freeStablePtr $ \statePtr -> do
    let u0 = smallChunkSize
    v0 <- newBuffer BL.empty 0 stateVar statePtr u0
    (v1, u1) <- fromBuildR f v0 u0
    SealedState { sealedSB = bytes, totalSB = total } <- sealBuffer v1 u1
    pure (total, bytes)

-- | First reads the number of unused bytes in the current buffer.
withUnused :: (Int -> BuildR) -> BuildR
withUnused f = toBuildR $ \v u -> fromBuildR (f u) v u

-- | First reads the number of bytes previously written.
withTotal :: (Int -> BuildR) -> BuildR
withTotal f = withTotal# (\total -> f (I# total))

-- | First reads the number of bytes previously written.
withTotal# :: (Int# -> BuildR) -> BuildR
withTotal# f = toBuildR $ \v u -> do
  I# total <- readTotal v u
  fromBuildR (f total) v u

-- | Executes the right builder, measuring how many bytes
-- it writes, then provides that figure to the left builder.
withLengthOf :: (Int -> BuildR) -> BuildR -> BuildR
withLengthOf = \f g -> withLengthOf# (\len -> f (I# len)) g
{-# INLINE CONLIKE withLengthOf #-}

-- | Executes the right builder, measuring how many bytes
-- it writes, then provides that figure to the left builder.
withLengthOf# :: (Int# -> BuildR) -> BuildR -> BuildR
withLengthOf# = \f g -> toBuildR $ \v0 u0 -> do
  !before <- readTotal v0 u0
  (v1, u1) <- fromBuildR g v0 u0
  !after <- readTotal v1 u1
  let !(I# len) = after - before
  fromBuildR (f len) v1 u1
{-# INLINE CONLIKE [1] withLengthOf# #-}  -- See Prim module for rules.

-- | Seals the current buffer and creates a new
-- one with at least the given number of bytes.
reallocate :: Int -> BuildR
reallocate (I# required) = reallocate# required

reallocate# :: Int# -> BuildR
reallocate# required = toBuildR $ \v0 u0 -> do
  SealedState
    { sealedSB = bytes
    , totalSB = total
    , stateVarSB = IORef (STRef stateVar)
    , statePtrSB = StablePtr statePtr
    } <- sealBuffer v0 u0
  let !u1 = max (I# required) defaultChunkSize
  v1 <- newBuffer bytes total (IORef (STRef stateVar)) (StablePtr statePtr) u1
  pure (v1, u1)
{-# NOINLINE reallocate# #-}  -- Avoid code bloat in library clients.

-- | Called by 'prependChunk' and 'prependReverseChunks'
-- to prepare a current buffer.
--
-- (This is a manual wrapper around 'afterPrependChunks#'.)
afterPrependChunks :: SealedState -> IO (Ptr Word8, Int)
afterPrependChunks !st = IO $ \s0 ->
  case afterPrependChunks# st s0 of
    (# v1, u1, s1 #) -> (# s1, (Ptr v1, I# u1) #)

afterPrependChunks# ::
  SealedState ->
  State# RealWorld ->
  (# Addr#, Int#, State# RealWorld #)
afterPrependChunks# SealedState
                      { sealedSB = sealed
                      , totalSB = total
                      , stateVarSB = stateVar
                      , statePtrSB = statePtr
                      , recycledSB = recycled
                      } s0 =
    case go s0 of (# s2, (Ptr v2, I# u2) #) -> (# v2, u2, s2 #)
  where
    IO go = case recycled of
      Nothing -> do
        -- The old buffer is part of 'sealed'.  Allocate a new buffer.
        let u1 = defaultChunkSize
        v1 <- newBuffer sealed total stateVar statePtr u1
        pure (v1, u1)

      Just buf -> do
        -- Recycle the old current buffer, from which
        -- we already copied what we wished to keep.
        u1 <- subtract metaDataSize <$> P.getSizeofMutableByteArray buf
        let !(PTR base) = P.mutableByteArrayContents buf
            !v1 = plusPtr (Ptr base) (metaDataSize + u1)
            !m = plusPtr (Ptr base) metaDataSize
        writeSpace m (u1 + total)
        let !nextState = BuildRState
               { currentBuffer = buf, sealedBuffers = sealed }
        writeIORef stateVar nextState
        pure (v1, u1)

-- | Prepends a 'B.ByteString' to the output.
--
-- NOTE: This is a relatively heavyweight operation.  For small
-- strings it may be faster to copy them to the current buffer.
prependChunk :: B.ByteString -> BuildR
prependChunk (BI.PS (ForeignPtr ad ct) (I# off) (I# len))
  | I# len == 0 = mempty
  | otherwise = BuildR (\v u s -> prependChunk# v u s ad ct off len)

prependChunk# ::
  -- | Used bytes.
  Addr# ->
  -- | Count of unused bytes.
  Int# ->
  -- | State token.
  State# RealWorld ->
  -- | Base address of 'B.ByteString'.
  Addr# ->
  -- | Finalizer for 'B.ByteString'.
  ForeignPtrContents ->
  -- | Offset from base of 'B.ByteString'.
  Int# ->
  -- | Length of 'B.ByteString'.
  Int# ->
  (# Addr#, Int#, State# RealWorld #)
prependChunk# v u s ad ct off len = go v u s
  where
    BuildR go = toBuildR $ \v1 u1 -> do
      SealedState
        { sealedSB = oldSealed
        , totalSB = oldTotal
        , stateVarSB = stateVar
        , statePtrSB = statePtr
        , recycledSB = recycled
        } <- sealBuffer v1 u1

      let chunk = BI.PS (ForeignPtr ad ct) (I# off) (I# len)

      afterPrependChunks SealedState
        { sealedSB = BLI.Chunk chunk oldSealed
        , totalSB = I# len + oldTotal
        , stateVarSB = stateVar
        , statePtrSB = statePtr
        , recycledSB = recycled
        }

-- | Like 'BL.ByteString', but with the chunks in reverse order,
-- even though the bytes within each chunk are in forward order.
newtype ReverseChunks = ReverseChunks { getReverseChunks :: BL.ByteString }

-- | Equivalent to the following, but faster:
--
-- > foldMap prependChunk . reverse . getReverseChunks
--
-- NOTE: This is a relatively heavyweight operation.  For small
-- strings it may be faster to copy them to the current buffer.
prependReverseChunks :: ReverseChunks -> BuildR
prependReverseChunks (ReverseChunks BLI.Empty) = mempty
prependReverseChunks
  (ReverseChunks (BLI.Chunk (BI.PS (ForeignPtr ad ct) (I# off) (I# len)) cs)) =
  BuildR (\v u s -> prependReverseChunks# v u s ad ct off len cs)

prependReverseChunks# ::
  -- | Used bytes.
  Addr# ->
  -- | Count of unused bytes.
  Int# ->
  -- | State token.
  State# RealWorld ->
  -- | Base address of first 'B.ByteString' chunk.
  Addr# ->
  -- | Finalizer for first 'B.ByteString' chunk.
  ForeignPtrContents ->
  -- | Offset from base of first 'B.ByteString' chunk.
  Int# ->
  -- | Length of first 'B.ByteString' chunk.
  Int# ->
  -- | Other chunks.
  BL.ByteString ->
  (# Addr#, Int#, State# RealWorld #)
prependReverseChunks# v0 u0 s0 ad ct off len cs0 = go v0 u0 s0
  where
    BuildR go = toBuildR $ \v1 u1 -> do
      SealedState
        { sealedSB = oldSealed
        , totalSB = oldTotal
        , stateVarSB = stateVar
        , statePtrSB = statePtr
        , recycledSB = recycled
        } <- sealBuffer v1 u1

      let appendChunks !total sealed (BLI.Chunk c cs) =
            appendChunks (B.length c + total) (BLI.Chunk c sealed) cs
          appendChunks newTotal newSealed BLI.Empty =
            afterPrependChunks SealedState
              { sealedSB = newSealed
              , totalSB = newTotal
              , stateVarSB = stateVar
              , statePtrSB = statePtr
              , recycledSB = recycled
              }

      let rchunks = BLI.Chunk (BI.PS (ForeignPtr ad ct) (I# off) (I# len)) cs0

      appendChunks oldTotal oldSealed rchunks

-- | Ensures that the current buffer has at least the given
-- number of bytes before executing the given builder.
ensure :: Int -> BuildR -> BuildR
ensure (I# required) f = ensure# required f

ensure# :: Int# -> BuildR -> BuildR
ensure# required (BuildR f) = BuildR $ \v u s ->
  if I# required <= I# u
    then f v u s
    else let BuildR g = BuildR f <> reallocate# required in g v u s

-- | ASSUMES that the specified number of bytes is both nonnegative and
-- less than or equal to the number of unused bytes in the current buffer,
-- consumes that number of unused bytes, and provides their starting address.
unsafeConsume :: Int -> (Ptr Word8 -> IO ()) -> BuildR
unsafeConsume = \width f ->
  toBuildR $ \v0 u0 -> do
    let !m = - width
        !v1 = plusPtr v0 m
        !u1 = u0 + m
    f v1
    pure (v1, u1)
{-# INLINE unsafeConsume #-}

-- | Given the builder inputs and a 'Float', converts
-- that number to its bit pattern in native byte order.
floatToWord32 :: Ptr Word8 -> Int -> Float -> IO Word32
floatToWord32 v u x = do
  let m = metaPtr v u
  pokeByteOff m scratchOffset x
  peekByteOff m scratchOffset

-- | Given the builder inputs and a 'Double', converts
-- that number to its bit pattern in native byte order.
doubleToWord64 :: Ptr Word8 -> Int -> Double -> IO Word64
doubleToWord64 v u x = do
  let m = metaPtr v u
  pokeByteOff m scratchOffset x
  peekByteOff m scratchOffset
