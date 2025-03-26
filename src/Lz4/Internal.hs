{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

{- | Compress a contiguous sequence of bytes into a single LZ4 block.
These functions do not perform any framing.
-}
module Lz4.Internal
  ( requiredBufferSize
  , c_hs_compress_HC
  , c_hs_decompress_safe
  ) where

import GHC.Exts (ByteArray#, MutableByteArray#)


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

foreign import ccall unsafe "hs_decompress_safe"
  c_hs_decompress_safe ::
       ByteArray# -- Source
    -> Int       -- Source offset
    -> MutableByteArray# s -- Destination
    -> Int       -- Destination offset
    -> Int       -- Input size
    -> Int       -- Destination capacity
    -> IO Int    -- Result length
