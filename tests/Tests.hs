{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Main where

import           Basement.Block ( Block )
import           Basement.UArray ( UArray )
import           Control.Monad ( replicateM )
import qualified Data.ByteString as BS
import           Data.ByteArray ( ByteArray, Bytes, ScrubbedBytes )
import qualified Data.ByteArray as B
import qualified Data.ByteArray.Encoding as B
import qualified Data.ByteArray.Parse as Parse
import           Data.Char ( chr )
import           Data.Int ( Int, Int16, Int32, Int64, Int8 )
import           Data.Typeable ( Typeable )
import           Data.Word ( Word, Word8 )
import           Prelude
                   ( Applicative (..), Bool (..), Bounded (..), Either (..)
                   , Enum (..), Eq (..), Functor (..), IO, Monad (..)
                   , Monoid (..), Ord (..), Show (..), String, ($), (.), (<$>)
                   , all, any, concatMap, const, fromIntegral, replicate
                   , reverse, snd, zip
                   )
import qualified SipHash
import           Test.Tasty ( TestTree, defaultMain, testGroup )
import qualified Test.Tasty.HUnit as HU
import qualified Test.Tasty.QuickCheck as QC
import           Utils

newtype Positive = Positive Word
  deriving (Eq, Ord, Show)

instance QC.Arbitrary Positive where
  arbitrary = Positive <$> QC.choose (0, 255)

data Backend = BackendByte | BackendScrubbedBytes
  | BackendBlock
  | BackendUArray
  deriving (Bounded, Enum, Eq, Show)

allBackends :: QC.NonEmptyList Backend
allBackends = QC.NonEmpty $ enumFrom BackendByte

data ArbitraryBS = forall a . ByteArray a => ArbitraryBS a

arbitraryBS :: Word -> QC.Gen ArbitraryBS
arbitraryBS n = do
  backend <- QC.elements $ QC.getNonEmpty allBackends
  case backend of
    BackendByte          -> ArbitraryBS `fmap` ((B.pack `fmap` replicateM (fromIntegral n) QC.arbitrary) :: QC.Gen Bytes)
    BackendScrubbedBytes -> ArbitraryBS `fmap` ((B.pack `fmap` replicateM (fromIntegral n) QC.arbitrary) :: QC.Gen ScrubbedBytes)
    BackendBlock         -> ArbitraryBS `fmap` ((B.pack `fmap` replicateM (fromIntegral n) QC.arbitrary) :: QC.Gen (Block Word8))
    BackendUArray        -> ArbitraryBS `fmap` ((B.pack `fmap` replicateM (fromIntegral n) QC.arbitrary) :: QC.Gen (UArray Word8))

arbitraryBSof :: Word -> Word -> QC.Gen ArbitraryBS
arbitraryBSof minBytes maxBytes = QC.choose (minBytes, maxBytes) >>= arbitraryBS

newtype SmallList a = SmallList [a]
  deriving (Eq, Show)

instance QC.Arbitrary a => QC.Arbitrary (SmallList a) where
  arbitrary = QC.chooseInt (0,8) >>= \n -> SmallList `fmap` replicateM (fromIntegral n) QC.arbitrary

instance QC.Arbitrary ArbitraryBS where
  arbitrary = arbitraryBSof 0 259

newtype Words8 = Words8 { unWords8 :: [Word8] }
  deriving (Eq, Show)

instance QC.Arbitrary Words8 where
  arbitrary = QC.chooseInt (0, 259) >>= \n -> Words8 <$> replicateM (fromIntegral n) QC.arbitrary

testGroupBackends ::
     String
  -> (forall ba . (Show ba, Eq ba, Typeable ba, ByteArray ba) => (ba -> ba)
  -> [TestTree])
  -> TestTree
testGroupBackends x l =
  testGroup x
    [ testGroup "Bytes" (l withBytesWitness)
    , testGroup "ScrubbedBytes" (l withScrubbedBytesWitness)
    , testGroup "Block" (l withBlockWitness)
    , testGroup "UArray" (l withUArrayWitness)
    ]

testShowProperty ::
     QC.Testable a
  => String
  -> (forall ba . (Show ba, Eq ba, Typeable ba, ByteArray ba) => (ba -> ba) -> ([Word8] -> String) -> a)
  -> TestTree
testShowProperty x p =
  testGroup x
    [ QC.testProperty "Bytes" (p withBytesWitness showLikeString)
    , QC.testProperty "ScrubbedBytes" (p withScrubbedBytesWitness showLikeEmptySB)
    ]
 where
  showLikeString  l = show $ chr . fromIntegral <$> l
  showLikeEmptySB _ = show (withScrubbedBytesWitness B.empty)

base64Kats :: [(String, String)]
base64Kats =
  [ ("pleasure.", "cGxlYXN1cmUu")
  , ("leasure.", "bGVhc3VyZS4=")
  , ("easure.", "ZWFzdXJlLg==")
  , ("asure.", "YXN1cmUu")
  , ("sure.", "c3VyZS4=")
  , ("", "")
  ]

base64URLKats :: [(String, String)]
base64URLKats =
  [ ("pleasure.", "cGxlYXN1cmUu")
  , ("leasure.", "bGVhc3VyZS4")
  , ("easure.", "ZWFzdXJlLg")
  , ("asure.", "YXN1cmUu")
  , ("sure.", "c3VyZS4")
  , ("\DC4\251\156\ETX\217~", "FPucA9l-") -- From RFC4648
  , ("\DC4\251\156\ETX\217\DEL", "FPucA9l_")
  , ("", "")
  ]

base16Kats :: [(String, String)]
base16Kats =
  [ ("this is a string", "74686973206973206120737472696e67") ]

base32Kats :: [(String, String)]
base32Kats =
  [ ("-pleasure.", "FVYGYZLBON2XEZJO")
  , ("pleasure.",  "OBWGKYLTOVZGKLQ=")
  , ("leasure.",   "NRSWC43VOJSS4===")
  , ("easure.",    "MVQXG5LSMUXA====")
  , ("asure.",     "MFZXK4TFFY======")
  , ("sure.",      "ON2XEZJO")
  , ("ure.",       "OVZGKLQ=")
  , ("re.",        "OJSS4===")
  , ("e.",         "MUXA====")
  , (".",          "FY======")
  , ("",           "")
  ]

encodingTests ::
     (Show bin, Typeable bin, ByteArray bin, ByteArray t)
  => (t -> bin)
  -> [TestTree]
encodingTests witnessID =
  [ testGroup "BASE64"
      [ testGroup "encode-KAT" encodeKats64
      , testGroup "decode-KAT" decodeKats64
      ]
  , testGroup "BASE64URL"
      [ testGroup "encode-KAT" encodeKats64URLUnpadded
      , testGroup "decode-KAT" decodeKats64URLUnpadded
      ]
  , testGroup "BASE32"
      [ testGroup "encode-KAT" encodeKats32
      , testGroup "decode-KAT" decodeKats32
      ]
  , testGroup "BASE16"
      [ testGroup "encode-KAT" encodeKats16
      , testGroup "decode-KAT" decodeKats16
      ]
  ]
 where
  encodeKats64 = toTest B.Base64 <$> zip [1..] base64Kats
  decodeKats64 = toBackTest B.Base64 <$> zip [1..] base64Kats
  encodeKats32 = toTest B.Base32 <$> zip [1..] base32Kats
  decodeKats32 = toBackTest B.Base32 <$> zip [1..] base32Kats
  encodeKats16 = toTest B.Base16 <$> zip [1..] base16Kats
  decodeKats16 = toBackTest B.Base16 <$> zip [1..] base16Kats
  encodeKats64URLUnpadded = toTest B.Base64URLUnpadded <$> zip [1..] base64URLKats
  decodeKats64URLUnpadded = toBackTest B.Base64URLUnpadded <$> zip [1..] base64URLKats

  toTest :: B.Base -> (Int, (String, String)) -> TestTree
  toTest base (i, (inp, out)) = QC.testProperty (show i) $
    let inpbs = witnessID $ B.convertToBase base $ witnessID $ B.pack $ unS inp
        outbs = witnessID $ B.pack $ unS out
    in  outbs QC.=== inpbs
  toBackTest :: B.Base -> (Int, (String, String)) -> TestTree
  toBackTest base (i, (inp, out)) = QC.testProperty (show i) $
    let inpbs = witnessID $ B.pack $ unS inp
        outbs = B.convertFromBase base $ witnessID $ B.pack $ unS out
    in  Right inpbs QC.=== outbs

-- check not to touch internal null pointer of the empty ByteString
bsNullEncodingTest :: TestTree
bsNullEncodingTest =
  testGroup "BS-null"
    [ testGroup "BASE64"
      [ QC.testProperty "encode-KAT" $ toTest B.Base64
      , QC.testProperty "decode-KAT" $ toBackTest B.Base64
      ]
    , testGroup "BASE32"
      [ QC.testProperty "encode-KAT" $ toTest B.Base32
      , QC.testProperty "decode-KAT" $ toBackTest B.Base32
      ]
    , testGroup "BASE16"
      [ QC.testProperty "encode-KAT" $ toTest B.Base16
      , QC.testProperty "decode-KAT" $ toBackTest B.Base16
      ]
    ]
 where
  toTest base =
    B.convertToBase base BS.empty QC.=== BS.empty
  toBackTest base =
    B.convertFromBase base BS.empty QC.=== Right BS.empty


parsingTests ::
     (Typeable t1, ByteArray t2, ByteArray t1, Show t1)
  => (t2 -> t1)
  -> [TestTree]
parsingTests witnessID =
  [ HU.testCase "parse" $
      let input = witnessID $ B.pack $ unS "xx abctest"
          abc   = witnessID $ B.pack $ unS "abc"
          est   = witnessID $ B.pack $ unS "est"
          result = Parse.parse ((,,) <$> Parse.take 2 <*> Parse.byte 0x20 <*> (Parse.bytes abc *> Parse.anyByte)) input
      in  case result of
            Parse.ParseOK remaining (_,_,_) -> HU.assertEqual "remaining" est remaining
            _                               -> HU.assertFailure "unexpected result"
  ]

main :: IO ()
main = defaultMain $ testGroup "memoria"
  [ testGroupBackends "basic" basicProperties
  , bsNullEncodingTest
  , testGroupBackends "encoding" encodingTests
  , testGroupBackends "parsing" parsingTests
  , testGroupBackends "hashing" $ \witnessID ->
      [ testGroup "SipHash" $ SipHash.tests witnessID
      ]
  , testShowProperty "showing" $ \witnessID expectedShow (Words8 l) ->
        (show . witnessID . B.pack $ l) == expectedShow l
  , testFoundationTypes
  ]
 where
  basicProperties witnessID =
    [ QC.testProperty "unpack . pack == id" $ \(Words8 l) -> l == (B.unpack . witnessID . B.pack $ l)
    , QC.testProperty "self-eq" $ \(Words8 l) -> let b = witnessID . B.pack $ l in b == b
    , QC.testProperty "add-empty-eq" $ \(Words8 l) ->
        let b = witnessID $ B.pack l
        in  B.append b B.empty == b
    , QC.testProperty "zero" $ \(Positive n) ->
        let expected = witnessID $ B.pack $ replicate (fromIntegral n) 0
        in  expected == B.zero (fromIntegral n)
    , QC.testProperty "Ord" $ \(Words8 l1) (Words8 l2) ->
        compare l1 l2 == compare (witnessID $ B.pack l1) (B.pack l2)
    , QC.testProperty "Monoid(mappend)" $ \(Words8 l1) (Words8 l2) ->
        mappend l1 l2 == B.unpack ( mappend (witnessID $ B.pack l1) (B.pack l2))
    , QC.testProperty "Monoid(mconcat)" $ \(SmallList l) ->
        mconcat (fmap unWords8 l) == B.unpack ( mconcat $ fmap (witnessID . B.pack . unWords8) l)
    , QC.testProperty "append (append a b) c == append a (append b c)" $ \(Words8 la) (Words8 lb) (Words8 lc) ->
        let a = witnessID $ B.pack la
            b = witnessID $ B.pack lb
            c = witnessID $ B.pack lc
        in  B.append (B.append a b) c == B.append a (B.append b c)
    , QC.testProperty "concat l" $ \(SmallList l) ->
        let chunks   = fmap (witnessID . B.pack . unWords8) l
            expected = concatMap unWords8 l
        in  B.pack expected == witnessID (B.concat chunks)
    , QC.testProperty "reverse" $ \(Words8 l) ->
        let b = witnessID (B.pack l)
        in  reverse l == B.unpack (B.reverse b)
    , QC.testProperty "cons b (reverse bs) == reverse (snoc bs b)" $ \(Words8 l) b ->
        let a = witnessID (B.pack l)
        in  B.cons b (B.reverse a) == B.reverse (B.snoc a b)
    , QC.testProperty "all == Prelude.all" $ \(Words8 l) b ->
        let b1 = witnessID (B.pack l)
            p  = (/= b)
        in  B.all p b1 == all p l
    , QC.testProperty "any == Prelude.any" $ \(Words8 l) b ->
        let b1 = witnessID (B.pack l)
            p  = (== b)
        in  B.any p b1 == any p l
    , QC.testProperty "singleton b == pack [b]" $ \b ->
        witnessID (B.singleton b) == B.pack [b]
    , QC.testProperty "span" $ \x (Words8 l) ->
        let c = witnessID (B.pack l)
            (a, b) = B.span (== x) c
        in  c == B.append a b
    , QC.testProperty "span (const True)" $ \(Words8 l) ->
        let a = witnessID (B.pack l)
        in  B.span (const True) a == (a, B.empty)
    , QC.testProperty "span (const False)" $ \(Words8 l) ->
        let b = witnessID (B.pack l)
        in  B.span (const False) b == (B.empty, b)
    ]

testFoundationTypes :: TestTree
testFoundationTypes = testGroup "Basement"
  [ HU.testCase "allocRet 4 _ :: UArray Int8 === 4" $ do
      x <- (B.length :: UArray Int8 -> Int) . snd <$> B.allocRet 4 (const $ pure ())
      HU.assertEqual "4 === x" x 4
  , HU.testCase "allocRet 4 _ :: UArray Int16 === 4" $ do
      x <- (B.length :: UArray Int16 -> Int) . snd <$> B.allocRet 4 (const $ pure ())
      HU.assertEqual "4 === x" x 4
  , HU.testCase "allocRet 4 _ :: UArray Int32 === 4" $ do
      x <- (B.length :: UArray Int32 -> Int) . snd <$> B.allocRet 4 (const $ pure ())
      HU.assertEqual "4 === x" x 4
  , HU.testCase "allocRet 4 _ :: UArray Int64 === 8" $ do
      x <- (B.length :: UArray Int64 -> Int) . snd <$> B.allocRet 4 (const $ pure ())
      HU.assertEqual "8 === x" x 8
  ]
