{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    -- * Decompression
  , decompressU
  ) where

import Lz4.Internal (requiredBufferSize,c_hs_compress_HC,c_hs_decompress_safe,DecompressionContext)
import Lz4.Internal (c_LZ4F_createDecompressionContext, c_hs_decompress_frame)
import Lz4.Internal (c_LZ4F_freeDecompressionContext)
import Foreign.C.Types (CSize)

import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import GHC.Exts (RealWorld)
import Control.Monad (when)
import Control.Monad.ST (runST)
import Data.Bits ((.&.))
import Data.Bytes.Types (Bytes (Bytes))
import Data.Int (Int32)
import Data.Primitive (ByteArray (..), MutableByteArray (..), MutablePrimArray)
import Data.Word (Word8, Word32)
import GHC.IO (unsafeIOToST,unsafeDupablePerformIO)

import qualified Data.Primitive as PM
import qualified Data.Primitive.ByteArray.LittleEndian as LE
import qualified Data.Bytes as Bytes

-- | Decompress an LZ4 frame. The caller must know the exact size
-- of the decompressed byte array.
--
-- Note: This currently fails if any of the optional headers are used.
-- It is difficult to find examples of lz4 frames that actually use
-- any of these. Open a PR with an example of an lz4 frame that fails
-- to decode if you find one.
--
-- Error codes from the C library lz4 are returned in the Left data
-- constructor.
decompressU ::
     Int -- ^ The exact size of the decompressed bytes
  -> Bytes -- ^ Compressed bytes
  -> Either Int ByteArray
decompressU !decompressedSize (Bytes arr@(ByteArray arr# ) off len) = do
  when (len < 11) (Left 10000)
  when (indexWord8 arr off /= 0x04) (Left 10001)
  when (indexWord8 arr (off + 1) /= 0x22) (Left 10002)
  when (indexWord8 arr (off + 2) /= 0x4D) (Left 10003)
  when (indexWord8 arr (off + 3) /= 0x18) (Left 10004)
  let !flag = indexWord8 arr (off + 4)
  -- when (flag /= 0b0110_0000) Nothing
  -- Here is the code that would read the size hint from the bd. However,
  -- there is no reason to use this since this function takes the actual
  -- size as an argument. We ignore the checksum at position off+6 as well.
  -- let !bd = indexWord8 arr (off + 5)
  -- maximumDecompressedSize <- case bd of
  --   0b0111_0000 -> pure 4194304
  --   0b0110_0000 -> pure 1048576
  --   0b0101_0000 -> pure 262144
  --   0b0100_0000 -> pure 65536
  --   _ -> Nothing
  -- when (maximumDecompressedSize < decompressedSize) Nothing
  if | flag == 0b0110_0000
     , !compressedSize <- LE.indexUnalignedByteArray arr (off + 7) :: Word32
     , !compressedSizeI <- fromIntegral (compressedSize .&. 0x7fff_ffff) :: Int
     , compressedSizeI + (4 + 3 + 4 + 4) == len -> do
         -- When there is only one block and there aren't any checksums,
         -- we can just decompress the single block and allocating and
         -- freeing a context. This case is pretty common in practice.
         let !offPost = off + 11 + compressedSizeI
         when (indexWord8 arr offPost /= 0x00) (Left 10005)
         when (indexWord8 arr (offPost + 1) /= 0x00) (Left 10006)
         when (indexWord8 arr (offPost + 2) /= 0x00) (Left 10007)
         when (indexWord8 arr (offPost + 3) /= 0x00) (Left 10008)
         case compressedSize .&. 0x8000_0000 of
           0 -> runST $ do
             dst@(MutableByteArray dst# ) <- PM.newByteArray decompressedSize
             actualSz <- unsafeIOToST (c_hs_decompress_safe arr# (off + 11) dst# 0 compressedSizeI decompressedSize)
             -- Note: actualSz will be negative if decompression fails. That's fine.
             if actualSz == decompressedSize
               then do
                 dst' <- PM.unsafeFreezeByteArray dst
                 pure (Right dst')
               else pure (Left 10009)
           _ -> do
             -- When the upper bit of the size is set, it means that the data in
             -- the block is uncompressed. This code path is not tested in the test
             -- suite, and I cannot find examples of this feature used in the wild.
             -- If anyone knows of an example, open a PR.
             when (decompressedSize /= compressedSizeI) (Left 10010)
             when (decompressedSize + 15 /= len) (Left 10011)
             Right $! Bytes.toByteArrayClone (Bytes arr (off + 11) decompressedSize)
     | otherwise -> unsafeDupablePerformIO $ do
         dst@(MutableByteArray dst# ) <- PM.newByteArray decompressedSize
         dctxPtrMba :: MutablePrimArray RealWorld (Ptr DecompressionContext) <- PM.newPinnedPrimArray 1
         let !dctxPtrPtr = PM.mutablePrimArrayContents dctxPtrMba
         x <- c_LZ4F_createDecompressionContext dctxPtrPtr 100
         case x of
           0 -> do
             dctxPtr <- peek dctxPtrPtr
             r <- c_hs_decompress_frame dctxPtr arr# off len dst# decompressedSize
             _ <- c_LZ4F_freeDecompressionContext dctxPtr
             case r of
               0 -> Right <$> PM.unsafeFreezeByteArray dst
               _ -> pure (Left r)
           _ -> errorWithoutStackTrace "Lz4.Frame.decompressU: failed to create decompression context"

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

indexWord8 :: ByteArray -> Int -> Word8
{-# inline indexWord8 #-}
indexWord8 = PM.indexByteArray
