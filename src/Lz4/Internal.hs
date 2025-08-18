{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

{- | Compress a contiguous sequence of bytes into a single LZ4 block.
These functions do not perform any framing.
-}
module Lz4.Internal
  ( DecompressionContext
  , requiredBufferSize
  , c_hs_compress_HC
  , c_hs_compress_fast
  , c_hs_decompress_safe
  , c_hs_decompress_frame
  , c_LZ4F_createDecompressionContext
  , c_LZ4F_freeDecompressionContext
  ) where

import GHC.Exts (ByteArray#, MutableByteArray#, Ptr)
import Foreign.C.Types (CUInt(..), CSize(..))

-- | Phantom type for pointers
data DecompressionContext

{- | Copied from the @LZ4_COMPRESSBOUND@ macro lz4.h to avoid using
FFI for simple arithmetic. Make sure this stays in sync with the macro.
-}
requiredBufferSize :: Int -> Int
{-# inline requiredBufferSize #-}
requiredBufferSize s = s + (div s 255) + 16

foreign import ccall unsafe "hs_compress_HC"
  c_hs_compress_HC ::
    ByteArray# -> -- Source
    Int -> -- Source offset
    MutableByteArray# s -> -- Destination
    Int -> -- Destination offset
    Int -> -- Input size
    Int -> -- Destination capacity
    Int -> -- Compression level
    IO Int -- Result length

foreign import ccall unsafe "hs_compress_fast"
  c_hs_compress_fast ::
    ByteArray# -> -- Source
    Int -> -- Source offset
    MutableByteArray# s -> -- Destination
    Int -> -- Destination offset
    Int -> -- Input size
    Int -> -- Destination capacity
    Int -> -- Compression level
    IO Int -- Result length

foreign import ccall unsafe "hs_decompress_safe"
  c_hs_decompress_safe ::
       ByteArray# -- Source
    -> Int       -- Source offset
    -> MutableByteArray# s -- Destination
    -> Int       -- Destination offset
    -> Int       -- Input size
    -> Int       -- Destination capacity
    -> IO Int    -- Result length

foreign import ccall unsafe "hs_decompress_frame"
  c_hs_decompress_frame ::
       Ptr DecompressionContext
    -> ByteArray# -- Source
    -> Int        -- Source offset
    -> Int        -- Source length
    -> MutableByteArray# s -- Destination
    -> Int                 -- Destination offset
    -> IO Int              -- Result (0 means success)

foreign import ccall unsafe "LZ4F_createDecompressionContext"
  c_LZ4F_createDecompressionContext ::
       Ptr (Ptr DecompressionContext)
    -> CUInt -- version
    -> IO CSize

foreign import ccall unsafe "LZ4F_freeDecompressionContext"
  c_LZ4F_freeDecompressionContext ::
       Ptr DecompressionContext
    -> IO CSize
