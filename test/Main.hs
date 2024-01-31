{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Bytes (Bytes)
import Data.Word (Word8)
import Lz4.Block (compress, compressHighly, decompress)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck (Gen, choose, forAll, testProperty, vectorOf, (===))

import qualified Data.Bytes as Bytes
import qualified GHC.Exts as Exts

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "lz4"
    [ testProperty "roundtrip" $ forAll genBytes $ \bs ->
        let cs = compress 1 bs
         in Just bs
              === decompress (Bytes.length bs) cs
    , testProperty "roundtrip-HC" $ forAll genBytes $ \bs ->
        let cs = compressHighly 3 bs
         in Just bs
              === decompress (Bytes.length bs) cs
    ]

genBytes :: Gen Bytes
genBytes = do
  n <- choose (0, 200)
  bs <- vectorOf n (choose (0, 2 :: Word8))
  pure (Exts.fromList bs)
