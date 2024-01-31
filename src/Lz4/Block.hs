{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

{- | Compress a contiguous sequence of bytes into a single LZ4 block.
These functions do not perform any framing.
-}
module Lz4.Block
  ( -- * Compression
    compress
  , compressU
  , compressHighly
  , compressHighlyU

    -- * Decompression
  , decompress
  , decompressU

    -- * Unsafe Compression
  , compressInto

    -- * Computing buffer size
  , requiredBufferSize
  ) where

import Lz4.Internal (c_hs_compress_HC, requiredBufferSize)

import Control.Monad.ST (runST)
import Control.Monad.ST.Run (runByteArrayST)
import Data.Bytes.Types (Bytes (Bytes))
import Data.Primitive (ByteArray (..), MutableByteArray (..))
import GHC.Exts (ByteArray#, MutableByteArray#)
import GHC.IO (unsafeIOToST)
import GHC.ST (ST (ST))

import qualified Control.Exception
import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts

{- | Compress bytes using LZ4's HC algorithm. This is slower
than 'compress' but provides better compression. A higher
compression level increases compression but decreases speed.
This function has undefined behavior on byte sequences larger
than 2,113,929,216 bytes. This calls @LZ4_compress_HC@.
-}
compressHighly ::
  -- | Compression level (Use 9 if uncertain)
  Int ->
  -- | Bytes to compress
  Bytes ->
  Bytes
compressHighly !lvl (Bytes (ByteArray arr) off len) = runST do
  let maxSz = requiredBufferSize len
  dst@(MutableByteArray dst#) <- PM.newByteArray maxSz
  actualSz <- unsafeIOToST (c_hs_compress_HC arr off dst# 0 len maxSz lvl)
  PM.shrinkMutableByteArray dst actualSz
  result <- PM.unsafeFreezeByteArray dst
  pure (Bytes result 0 actualSz)

-- | Variant of 'compressHighly' with an unsliced result.
compressHighlyU ::
  -- | Compression level (Use 9 if uncertain)
  Int ->
  -- | Bytes to compress
  Bytes ->
  ByteArray
compressHighlyU !lvl (Bytes (ByteArray arr) off len) = runST do
  let maxSz = requiredBufferSize len
  dst@(MutableByteArray dst#) <- PM.newByteArray maxSz
  actualSz <- unsafeIOToST (c_hs_compress_HC arr off dst# 0 len maxSz lvl)
  PM.shrinkMutableByteArray dst actualSz
  PM.unsafeFreezeByteArray dst

{- | Compress bytes using LZ4.
A higher acceleration factor increases speed but decreases
compression. This function has undefined
behavior on byte sequences larger than 2,113,929,216 bytes.
This calls @LZ4_compress_default@.
-}
compress ::
  -- | Acceleration Factor (Use 1 if uncertain)
  Int ->
  -- | Bytes to compress
  Bytes ->
  Bytes
compress !lvl (Bytes (ByteArray arr) off len) = runST do
  let maxSz = requiredBufferSize len
  dst@(MutableByteArray dst#) <- PM.newByteArray maxSz
  actualSz <- unsafeIOToST (c_hs_compress_fast arr off dst# 0 len maxSz lvl)
  PM.shrinkMutableByteArray dst actualSz
  result <- PM.unsafeFreezeByteArray dst
  pure (Bytes result 0 actualSz)

{- | Compress bytes using LZ4, pasting the compressed bytes into the
mutable byte array at the specified offset.

Precondition: There must be at least
@'requiredBufferSize' (Bytes.length src)@ bytes available starting
from the offset in the destination buffer. This is checked, and
this function will throw an exception if this invariant is violated.
-}
compressInto ::
  -- | Acceleration Factor (Use 1 if uncertain)
  Int ->
  -- | Bytes to compress
  Bytes ->
  -- | Destination buffer
  MutableByteArray s ->
  -- | Offset into destination buffer
  Int ->
  -- | Bytes remaining in destination buffer
  Int ->
  -- | Next available offset in destination buffer
  ST s Int
compressInto !lvl (Bytes (ByteArray arr) off len) dst@(MutableByteArray dst#) !doff !dlen = do
  let maxSz = requiredBufferSize len
  if dlen < maxSz
    then unsafeIOToST (Control.Exception.throwIO Lz4BufferTooSmall)
    else do
      actualSz <- unsafeIOToST (c_hs_compress_fast arr off dst# doff len maxSz lvl)
      pure (doff + actualSz)

-- | Variant of 'compress' with an unsliced result.
compressU ::
  -- | Acceleration Factor (Use 1 if uncertain)
  Int ->
  -- | Bytes to compress
  Bytes ->
  ByteArray
compressU !lvl (Bytes (ByteArray arr) off len) = runByteArrayST do
  let maxSz = requiredBufferSize len
  dst@(MutableByteArray dst#) <- PM.newByteArray maxSz
  actualSz <- unsafeIOToST (c_hs_compress_fast arr off dst# 0 len maxSz lvl)
  PM.shrinkMutableByteArray dst actualSz
  PM.unsafeFreezeByteArray dst

{- | Decompress a byte sequence. Fails if the actual decompressed
result does not match the given expected length.
-}
decompress ::
  -- | Expected length of decompressed bytes
  Int ->
  -- | Compressed bytes
  Bytes ->
  Maybe Bytes
decompress !dstSz !b = case decompressU dstSz b of
  Nothing -> Nothing
  Just r -> Just (Bytes r 0 dstSz)

-- | Variant of 'decompress' with an unsliced result.
decompressU ::
  -- | Expected length of decompressed bytes
  Int ->
  -- | Compressed bytes
  Bytes ->
  Maybe ByteArray
decompressU dstSz (Bytes (ByteArray arr) off len) = runST do
  dst@(MutableByteArray dst#) <- PM.newByteArray dstSz
  actualSz <- unsafeIOToST (c_hs_decompress_safe arr off dst# 0 len dstSz)
  if actualSz == dstSz
    then do
      result <- PM.unsafeFreezeByteArray dst
      pure (Just result)
    else pure Nothing

foreign import ccall unsafe "hs_compress_fast"
  c_hs_compress_fast ::
    ByteArray# -> -- Source
    Int -> -- Source offset
    MutableByteArray# s -> -- Destination
    Int -> -- Destination offset
    Int -> -- Input size
    Int -> -- Destination capacity
    Int -> -- Acceleration factor
    IO Int -- Result length

foreign import ccall unsafe "hs_decompress_safe"
  c_hs_decompress_safe ::
    ByteArray# -> -- Source
    Int -> -- Source offset
    MutableByteArray# s -> -- Destination
    Int -> -- Destination offset
    Int -> -- Input size
    Int -> -- Destination capacity
    IO Int -- Result length

data Lz4BufferTooSmall = Lz4BufferTooSmall
  deriving stock (Eq, Show)
  deriving anyclass (Control.Exception.Exception)

-- foreign import capi "lz4.h value sizeof(LZ4_stream_t)" lz4StreamSz :: Int
--
-- allocateLz4StreamT :: ST s (MutableByteArray s)
-- allocateLz4StreamT = PM.newPinnedByteArray lz4StreamSz
