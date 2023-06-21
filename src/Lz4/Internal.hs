{-# language BangPatterns #-}
{-# language BlockArguments #-}
{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language UnliftedFFITypes #-}

-- | Compress a contiguous sequence of bytes into a single LZ4 block.
-- These functions do not perform any framing.
module Lz4.Internal
  ( requiredBufferSize
  , c_hs_compress_HC
  ) where

import Control.Monad.ST (runST)
import Control.Monad.ST.Run (runByteArrayST)
import Data.Bytes.Types (Bytes(Bytes))
import Data.Primitive (MutableByteArray(..),ByteArray(..))
import GHC.Exts (ByteArray#,MutableByteArray#)
import GHC.IO (unsafeIOToST)
import GHC.ST (ST(ST))

import qualified Control.Exception
import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts

-- | Copied from the @LZ4_COMPRESSBOUND@ macro lz4.h to avoid using
-- FFI for simple arithmetic. Make sure this stays in sync with the macro.
requiredBufferSize :: Int -> Int
requiredBufferSize s = s + (div s 255) + 16

foreign import ccall unsafe "hs_compress_HC"
  c_hs_compress_HC ::
       ByteArray# -- Source
    -> Int       -- Source offset
    -> MutableByteArray# s -- Destination
    -> Int       -- Destination offset
    -> Int       -- Input size
    -> Int       -- Destination capacity
    -> Int       -- Compression level
    -> IO Int    -- Result length
