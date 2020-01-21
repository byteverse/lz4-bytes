{-# language BlockArguments #-}
{-# language UnliftedFFITypes #-}
{-# language MagicHash #-}
{-# language BangPatterns #-}
{-# language UnboxedTuples #-}

module Lz4.Block
  ( compress
  , compressU
  , decompress
  , decompressU
  ) where

import GHC.Exts (ByteArray#,MutableByteArray#)
import Data.Primitive (MutableByteArray(..),ByteArray(..))
import GHC.ST (ST(ST))
import Data.Bytes.Types (Bytes(Bytes))
import GHC.IO (unsafeIOToST)
import Control.Monad.ST.Run (runByteArrayST)
import Control.Monad.ST (runST)

import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts

-- | Compress bytes using LZ4. This function has undefined
-- behavior on byte sequences larger than 2,113,929,216 bytes. This calls @LZ4_compress_default@.
compress ::
     Int -- ^ Acceleration Factor (Use 1 if uncertain)
  -> Bytes -- ^ Bytes to compress
  -> Bytes
compress !lvl (Bytes (ByteArray arr) off len) = runST do
  let maxSz = inlineCompressBound len
  dst@(MutableByteArray dst# ) <- PM.newByteArray maxSz
  actualSz <- unsafeIOToST (c_hs_compress_fast arr off dst# 0 len maxSz lvl)
  shrinkMutableByteArray dst actualSz
  result <- PM.unsafeFreezeByteArray dst
  pure (Bytes result 0 actualSz)

-- | Variant of 'compress' with an unsliced result.
compressU :: 
     Int -- ^ Acceleration Factor (Use 1 if uncertain)
  -> Bytes -- ^ Bytes to compress
  -> ByteArray
compressU !lvl (Bytes (ByteArray arr) off len) = runByteArrayST do
  let maxSz = inlineCompressBound len
  dst@(MutableByteArray dst# ) <- PM.newByteArray maxSz
  actualSz <- unsafeIOToST (c_hs_compress_fast arr off dst# 0 len maxSz lvl)
  shrinkMutableByteArray dst actualSz
  PM.unsafeFreezeByteArray dst

-- | Decompress a byte sequence. Fails if the actual decompressed
-- result does not match the given expected length.
decompress ::
     Int -- ^ Expected length of decompressed bytes
  -> Bytes -- ^ Compressed bytes
  -> Maybe Bytes
decompress !dstSz !b = case decompressU dstSz b of
  Nothing -> Nothing
  Just r -> Just (Bytes r 0 dstSz)

-- | Variant of 'decompress' with an unsliced result.
decompressU ::
     Int -- ^ Expected length of decompressed bytes
  -> Bytes -- ^ Compressed bytes
  -> Maybe ByteArray
decompressU dstSz (Bytes (ByteArray arr) off len) = runST do
  dst@(MutableByteArray dst# ) <- PM.newByteArray dstSz
  actualSz <- unsafeIOToST (c_hs_decompress_safe arr off dst# 0 len dstSz)
  if actualSz == dstSz
    then do
      result <- PM.unsafeFreezeByteArray dst
      pure (Just result)
    else pure Nothing

-- Copied from a macro lz4.h to avoid using FFI for simple
-- arithmetic. Make sure this stays in sync with the macro.
inlineCompressBound :: Int -> Int
inlineCompressBound s = s + (div s 255) + 16

foreign import ccall unsafe "hs_compress_fast"
  c_hs_compress_fast ::
       ByteArray# -- Source
    -> Int       -- Source offset
    -> MutableByteArray# s -- Destination
    -> Int       -- Destination offset
    -> Int       -- Input size
    -> Int       -- Destination capacity
    -> Int       -- Compression level
    -> IO Int    -- Result length

foreign import ccall unsafe "hs_decompress_safe"
  c_hs_decompress_safe ::
       ByteArray# -- Source
    -> Int       -- Source offset
    -> MutableByteArray# s -- Destination
    -> Int       -- Destination offset
    -> Int       -- Input size
    -> Int       -- Destination capacity
    -> IO Int    -- Result length

shrinkMutableByteArray :: MutableByteArray s -> Int -> ST s ()
shrinkMutableByteArray (MutableByteArray x) (Exts.I# i) =
  ST (\s -> (# Exts.shrinkMutableByteArray# x i s, () #) )
