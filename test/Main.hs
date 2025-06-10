{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
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
        Right bs
        ===
        Frame.decompressU (sizeofByteArray bs) (Bytes.fromByteArray cs)
    , testCase "roundtrip-HC-80k" $ testRoundtrip 80_000
    , testCase "roundtrip-HC-800k" $ testRoundtrip 800_000
    , testCase "roundtrip-HC-2M" $ testRoundtrip 2_000_000
    , testCase "roundtrip-HC-5M" $ testRoundtrip 5_000_000
    , testCase "roundtrip-HC-9M" $ testRoundtrip 9_000_000
    , testCase "packing-HC-80k" $ testPacking 80_000
    , testCase "packing-HC-800k" $ testPacking 800_000
    , testCase "packing-HC-2M" $ testPacking 2_000_000
    , testCase "packing-HC-5M" $ testPacking 5_000_000
    , testCase "packing-HC-9M" $ testPacking 9_000_000
    , testCase "example-a" $ case Frame.decompressU 20 (Bytes.fromByteArray exampleA) of
        Left{} -> fail "decompression failed"
        Right _ -> pure ()
    , testCase "example-b" $ case Frame.decompressU 10 (Bytes.fromByteArray exampleB) of
        Left{} -> fail "decompression failed"
        Right x -> x @=? Exts.fromList [0xbb :: Word8, 0x01, 0xbb, 0x01, 0xbb, 0x01, 0xbb, 0x01, 0xbb, 0x01 ]
    , testCase "example-c" $ case Frame.decompressU 20 (Bytes.fromByteArray exampleC) of
        Left e -> fail ("decompression failed, error code: " ++ show e)
        Right _ -> pure ()
    ]
  ]

testRoundtrip :: Int -> IO ()
testRoundtrip sz =
  let uncompressed = Bytes.replicate sz 0x01 in
  let compressed = Frame.compressHighlyU 3 uncompressed in
  case Frame.decompressU sz (Bytes.fromByteArray compressed) of
    Left{} -> fail "decompression failed"
    Right result -> if uncompressed == Bytes.fromByteArray result
      then pure ()
      else fail "compression and decompression did not round trip"

testPacking :: Int -> IO ()
testPacking sz =
  let uncompressed = Bytes.replicate sz 0x01 in
  let compressed = Frame.compressHighlyU 3 uncompressed in
  let lenCompressed = sizeofByteArray compressed in
  if lenCompressed * 100 > sz
    then fail "Repetition of same byte has compression factor less than 100"
    else pure ()

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

-- Same as exampleA, but the flag indicates that linked blocks are used.
-- There is only one block, so there aren't actually linked blocks, but
-- this lets us force the decompress function to take the slow path
-- where it sets up a decompression context.
exampleC :: ByteArray
exampleC = Exts.fromList
  [ 0x04, 0x22, 0x4d, (0x18 :: Word8)
  , 0x40, 0x40, 0xc0
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
