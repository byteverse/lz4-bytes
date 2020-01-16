{-# language BangPatterns #-}
{-# language MultiWayIf #-}
{-# language NumDecimals #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

import Lz4.Block (compress,decompress)
import Data.Bytes.Types (Bytes(Bytes))
import Data.Char (ord)
import Data.Primitive (ByteArray)
import Data.Proxy (Proxy(..))
import Data.Word (Word8)
import Test.Tasty (defaultMain,testGroup,TestTree)
import Test.Tasty.QuickCheck ((===),testProperty,property,Gen)
import Test.Tasty.QuickCheck (choose,vectorOf,forAll)

import qualified Data.Bytes as Bytes
import qualified Data.List as List
import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts
import qualified Test.QuickCheck.Classes as QCC
import qualified Test.Tasty.QuickCheck as TQC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "lz4"
  [ testProperty "roundtrip" $ forAll genBytes $ \bs ->
      let cs = compress bs in
      Just bs
      ===
      decompress (Bytes.length bs) cs
  ]

genBytes :: Gen Bytes
genBytes = do
  n <- choose (0,200)
  bs <- vectorOf n (choose (0,2 :: Word8)) 
  pure (Exts.fromList bs)
