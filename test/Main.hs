{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Bytes (Bytes)
import Data.Primitive (ByteArray,sizeofByteArray)
import Data.Word (Word8)
import Lz4.Block (compress, compressHighly, decompress)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase,(@=?))
import Test.Tasty.QuickCheck (Gen, choose, forAll, testProperty, vectorOf, (===))

import qualified Data.Bytes as Bytes
import qualified GHC.Exts as Exts
import qualified Lz4.Frame as Frame

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "lz4"
  [ testGroup "block"
    [ testProperty "roundtrip" $ forAll genBytes $ \bs ->
        let cs = compress 1 bs in
        Just bs
        ===
        decompress (Bytes.length bs) cs
    , testProperty "roundtrip-HC" $ forAll genBytes $ \bs ->
        let cs = compressHighly 3 bs in
        Just bs
        ===
        decompress (Bytes.length bs) cs
    ]
  , testGroup "frame"
    [ testProperty "roundtrip-HC" $ forAll genByteArray $ \bs ->
        let cs = Frame.compressHighlyU 3 (Bytes.fromByteArray bs) in
        Just bs
        ===
        Frame.decompressU (sizeofByteArray bs) (Bytes.fromByteArray cs)
    , testCase "example-a" $ case Frame.decompressU 20 (Bytes.fromByteArray exampleA) of
        Nothing -> fail "decompression failed"
        Just _ -> pure ()
    , testCase "example-b" $ case Frame.decompressU 10 (Bytes.fromByteArray exampleB) of
        Nothing -> fail "decompression failed"
        Just x -> x @=? Exts.fromList [0xbb :: Word8, 0x01, 0xbb, 0x01, 0xbb, 0x01, 0xbb, 0x01, 0xbb, 0x01 ]
    ]
  ]

genBytes :: Gen Bytes
genBytes = do
  n <- choose (0, 200)
  bs <- vectorOf n (choose (0, 2 :: Word8))
  pure (Exts.fromList bs)

genByteArray :: Gen ByteArray
genByteArray = fmap Bytes.toByteArray genBytes

-- Example from Clickhouse arrow output
-- Compressed Length: 28
-- Decompressed Length: 20
exampleA :: ByteArray
exampleA = Exts.fromList
  [ 0x04, 0x22, 0x4d, (0x18 :: Word8)
  , 0x60, 0x40, 0x82
  , 0x0d, 0x00, 0x00, 0x00 -- little-endian encoding of the number 13
  , 0x47, 0x15, 0x08, 0x01, 0x0a, 0x04 , 0x00, 0x50, 0x0a, 0x15, 0x08, 0x01, 0x0a
  , 0x00, 0x00, 0x00, 0x00
  ]

-- Example that tests a frame that does not use compression
exampleB :: ByteArray
exampleB = Exts.fromList
  [ 0x04, 0x22, 0x4d, (0x18 :: Word8)
  , 0x60, 0x40, 0x82
  , 0x0a, 0x00, 0x00, 0x80 -- little-endian encoding of 10 but with the high bit set to disable compression
  , 0xbb, 0x01
  , 0xbb, 0x01
  , 0xbb, 0x01
  , 0xbb, 0x01
  , 0xbb, 0x01
  , 0x00, 0x00, 0x00, 0x00
  ]
