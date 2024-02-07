{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE UnliftedFFITypes #-}

{- | Compress a contiguous sequence of bytes into an LZ4 frame
containing a single block.
-}
module Lz4.Frame
  ( -- * Compression
    compressHighlyU
  ) where

import Lz4.Internal (c_hs_compress_HC, requiredBufferSize)

import Control.Monad.ST (runST)
import Data.Bytes.Types (Bytes (Bytes))
import Data.Int (Int32)
import Data.Primitive (ByteArray (..), MutableByteArray (..))
import Data.Word (Word8)
import GHC.IO (unsafeIOToST)

import qualified Data.Primitive as PM
import qualified Data.Primitive.ByteArray.LittleEndian as LE

{- | Use HC compression to produce a frame with a single block.
All optional fields (checksums, content sizes, and dictionary IDs)
are omitted.

Note: Currently, this produces incorrect output when the size of
the input to be compressed is greater than 4MiB. The only way
to correct this function is to make it not compress large input.
This can be done by setting the high bit of the size. This needs
to be tested though since it is an uncommon code path.
-}
compressHighlyU ::
  -- | Compression level (Use 9 if uncertain)
  Int ->
  -- | Bytes to compress
  Bytes ->
  ByteArray
compressHighlyU !lvl (Bytes (ByteArray arr) off len) = runST do
  let maxSz = requiredBufferSize len + 15
  dst@(MutableByteArray dst#) <- PM.newByteArray maxSz
  -- -- First 4 bytes: magic identifier
  PM.writeByteArray dst 0 (0x04 :: Word8)
  PM.writeByteArray dst 1 (0x22 :: Word8)
  PM.writeByteArray dst 2 (0x4D :: Word8)
  PM.writeByteArray dst 3 (0x18 :: Word8)
  -- Next 3 bytes: frame descriptor
  PM.writeByteArray dst 4 (0b0110_0000 :: Word8)
  if
    | len <= 65_536 -> do
        PM.writeByteArray dst 5 (0b0100_0000 :: Word8)
        PM.writeByteArray dst 6 (0x82 :: Word8)
    | len <= 262_144 -> do
        PM.writeByteArray dst 5 (0b0101_0000 :: Word8)
        PM.writeByteArray dst 6 (0xFB :: Word8)
    | len <= 1_048_576 -> do
        PM.writeByteArray dst 5 (0b0110_0000 :: Word8)
        PM.writeByteArray dst 6 (0x51 :: Word8)
    | otherwise -> do
        PM.writeByteArray dst 5 (0b0111_0000 :: Word8)
        PM.writeByteArray dst 6 (0x73 :: Word8)
  actualSz <- unsafeIOToST (c_hs_compress_HC arr off dst# 11 len maxSz lvl)
  LE.writeUnalignedByteArray dst 7 (fromIntegral actualSz :: Int32)
  PM.writeByteArray dst (actualSz + 11) (0x00 :: Word8)
  PM.writeByteArray dst (actualSz + 12) (0x00 :: Word8)
  PM.writeByteArray dst (actualSz + 13) (0x00 :: Word8)
  PM.writeByteArray dst (actualSz + 14) (0x00 :: Word8)
  PM.shrinkMutableByteArray dst (actualSz + 15)
  PM.unsafeFreezeByteArray dst
