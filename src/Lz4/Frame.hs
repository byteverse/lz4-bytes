{-# language BangPatterns #-}
{-# language BinaryLiterals #-}
{-# language BlockArguments #-}
{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language MagicHash #-}
{-# language MultiWayIf #-}
{-# language NumericUnderscores #-}
{-# language UnboxedTuples #-}
{-# language UnliftedFFITypes #-}

-- | Compress a contiguous sequence of bytes into an LZ4 frame
-- containing a single block.
module Lz4.Frame
  ( -- * Compression
    compressHighlyU
  ) where

import Lz4.Internal (requiredBufferSize,c_hs_compress_HC)

import Control.Monad.ST (runST)
import Control.Monad.ST.Run (runByteArrayST)
import Data.Bytes.Types (Bytes(Bytes))
import Data.Int (Int32)
import Data.Primitive (MutableByteArray(..),ByteArray(..))
import Data.Word (Word8)
import GHC.Exts (ByteArray#,MutableByteArray#)
import GHC.IO (unsafeIOToST)
import GHC.ST (ST(ST))

import qualified Control.Exception
import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts
import qualified Data.Primitive.ByteArray.LittleEndian as LE

-- | Use HC compression to produce a frame with a single block.
-- All optional fields (checksums, content sizes, and dictionary IDs)
-- are omitted.
--
-- Note: Currently, this produces incorrect output when the size of
-- the input to be compressed is greater than 4MiB. The only way
-- to correct this function is to make it not compress large input.
-- This can be done by setting the high bit of the size. This needs
-- to be tested though since it is an uncommon code path.
compressHighlyU ::
     Int -- ^ Compression level (Use 9 if uncertain)
  -> Bytes -- ^ Bytes to compress
  -> ByteArray
compressHighlyU !lvl (Bytes (ByteArray arr) off len) = runST do
  let maxSz = requiredBufferSize len + 15
  dst@(MutableByteArray dst# ) <- PM.newByteArray maxSz
  -- -- First 4 bytes: magic identifier
  PM.writeByteArray dst 0 (0x04 :: Word8)
  PM.writeByteArray dst 1 (0x22 :: Word8)
  PM.writeByteArray dst 2 (0x4D :: Word8)
  PM.writeByteArray dst 3 (0x18 :: Word8)
  -- Next 3 bytes: frame descriptor
  PM.writeByteArray dst 4 (0b0110_0000 :: Word8)
  if | len <= 65536 -> do
         PM.writeByteArray dst 5 (0b0100_0000 :: Word8)
         PM.writeByteArray dst 6 (0x82 :: Word8)
     | len <= 262144 -> do
         PM.writeByteArray dst 5 (0b0101_0000 :: Word8)
         PM.writeByteArray dst 6 (0xFB :: Word8)
     | len <= 1048576 -> do
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