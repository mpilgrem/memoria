{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Module      : Data.ByteArray.ScrubbedBytes
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : Stable
-- Portability : GHC
--
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE CPP #-}
module Data.ByteArray.ScrubbedBytes
    ( ScrubbedBytes
    ) where

import           GHC.Types
import           GHC.Prim
import           GHC.Ptr
import           GHC.Word
#if MIN_VERSION_base(4,15,0)
import           GHC.Exts (unsafeCoerce#)
#endif
import           Data.Semigroup
import           Data.Foldable (toList)
import           Data.String (IsString(..))
import           Data.Memory.PtrMethods
import           Data.Memory.Internal.CompatPrim
import           Data.Memory.Internal.Compat     (unsafeDoIO)
import           Data.Memory.Internal.Imports
import           Data.ByteArray.Types
import           Foreign.Storable
import           Basement.NormalForm

-- | ScrubbedBytes is a memory chunk which have the properties of:
--
-- * Being scrubbed after its goes out of scope.
--
-- * A Show instance that doesn't actually show any content
--
-- * A Eq instance that is constant time
--
data ScrubbedBytes = ScrubbedBytes (MutableByteArray# RealWorld)

instance Show ScrubbedBytes where
    show _ = "<scrubbed-bytes>"

instance Eq ScrubbedBytes where
    (==) = scrubbedBytesEq
instance Ord ScrubbedBytes where
    compare = scrubbedBytesCompare
instance Semigroup ScrubbedBytes where
    b1 <> b2      = unsafeDoIO $ scrubbedBytesAppend b1 b2
    sconcat       = unsafeDoIO . scrubbedBytesConcat . toList
instance Monoid ScrubbedBytes where
    mempty        = unsafeDoIO (newScrubbedBytes 0)
instance NFData ScrubbedBytes where
    rnf b = b `seq` ()
instance NormalForm ScrubbedBytes where
    toNormalForm b = b `seq` ()
instance IsString ScrubbedBytes where
    fromString = scrubbedFromChar8

instance ByteArrayAccess ScrubbedBytes where
    length        = sizeofScrubbedBytes
    withByteArray = withPtr

instance ByteArray ScrubbedBytes where
    allocRet = scrubbedBytesAllocRet

newScrubbedBytes :: Int -> IO ScrubbedBytes
newScrubbedBytes (I# sz)
    | booleanPrim (sz <# 0#)  = error "ScrubbedBytes: size must be >= 0"
    | booleanPrim (sz ==# 0#) = IO $ \s ->
        case newAlignedPinnedByteArray# 0# 8# s of
            (# s2, mba #) -> (# s2, ScrubbedBytes mba #)
    | otherwise               = IO $ \s ->
        case newAlignedPinnedByteArray# sz 8# s of
            (# s1, mbarr #) ->
                let !scrubber = getScrubber (byteArrayContents# (unsafeCoerce# mbarr))
                    !mba      = ScrubbedBytes mbarr
                 in case mkWeak# mbarr () (finalize scrubber mba) s1 of
                    (# s2, _ #) -> (# s2, mba #)
  where
    getScrubber :: Addr# -> State# RealWorld -> State# RealWorld
    getScrubber addr s =
        let IO scrubBytes = memSet (Ptr addr) 0 (I# sz)
         in case scrubBytes s of
                (# s', _ #) -> s'

    finalize :: (State# RealWorld -> State# RealWorld) -> ScrubbedBytes -> State# RealWorld -> (# State# RealWorld, () #)
    finalize scrubber mba@(ScrubbedBytes _) s1 =
        case scrubber s1 of
            s2 -> case touch# mba s2 of
                    s3 -> (# s3, () #)

scrubbedBytesAllocRet :: Int -> (Ptr p -> IO a) -> IO (a, ScrubbedBytes)
scrubbedBytesAllocRet sz f = do
    ba <- newScrubbedBytes sz
    r  <- withPtr ba f
    return (r, ba)

scrubbedBytesAlloc :: Int -> (Ptr p -> IO ()) -> IO ScrubbedBytes
scrubbedBytesAlloc sz f = do
    ba <- newScrubbedBytes sz
    withPtr ba f
    return ba

scrubbedBytesConcat :: [ScrubbedBytes] -> IO ScrubbedBytes
scrubbedBytesConcat l = scrubbedBytesAlloc retLen (copy l)
  where
    retLen = sum $ map sizeofScrubbedBytes l

    copy []     _   = return ()
    copy (x:xs) dst = do
        withPtr x $ \src -> memCopy dst src chunkLen
        copy xs (dst `plusPtr` chunkLen)
      where
        chunkLen = sizeofScrubbedBytes x

scrubbedBytesAppend :: ScrubbedBytes -> ScrubbedBytes -> IO ScrubbedBytes
scrubbedBytesAppend b1 b2 = scrubbedBytesAlloc retLen $ \dst -> do
    withPtr b1 $ \s1 -> memCopy dst                  s1 len1
    withPtr b2 $ \s2 -> memCopy (dst `plusPtr` len1) s2 len2
  where
    len1   = sizeofScrubbedBytes b1
    len2   = sizeofScrubbedBytes b2
    retLen = len1 + len2


sizeofScrubbedBytes :: ScrubbedBytes -> Int
sizeofScrubbedBytes (ScrubbedBytes mba) = I# (sizeofMutableByteArray# mba)

withPtr :: ScrubbedBytes -> (Ptr p -> IO a) -> IO a
withPtr b@(ScrubbedBytes mba) f = do
    a <- f (Ptr (byteArrayContents# (unsafeCoerce# mba)))
    touchScrubbedBytes b
    return a

touchScrubbedBytes :: ScrubbedBytes -> IO ()
touchScrubbedBytes (ScrubbedBytes mba) = IO $ \s -> case touch# mba s of s' -> (# s', () #)

scrubbedBytesEq :: ScrubbedBytes -> ScrubbedBytes -> Bool
scrubbedBytesEq a b
    | l1 /= l2  = False
    | otherwise = unsafeDoIO $ withPtr a $ \p1 -> withPtr b $ \p2 -> memConstEqual p1 p2 l1
  where
        l1 = sizeofScrubbedBytes a
        l2 = sizeofScrubbedBytes b

scrubbedBytesCompare :: ScrubbedBytes -> ScrubbedBytes -> Ordering
scrubbedBytesCompare b1@(ScrubbedBytes m1) b2@(ScrubbedBytes m2) = unsafeDoIO $ loop 0
  where
    !l1  = sizeofScrubbedBytes b1
    !l2  = sizeofScrubbedBytes b2
    !len = min l1 l2

    loop !i
        | i == len =
            if l1 == l2
                then pure EQ
                else if l1 > l2 then pure GT
                                else pure LT
        | otherwise = do
            e1 <- read8 m1 i
            e2 <- read8 m2 i
            if e1 == e2
                then loop (i+1)
                else if e1 < e2 then pure LT
                                else pure GT

    read8 m (I# i) = IO $ \s -> case readWord8Array# m i s of
                                    (# s2, e #) -> (# s2, W8# e #)

scrubbedFromChar8 :: [Char] -> ScrubbedBytes
scrubbedFromChar8 l = unsafeDoIO $ scrubbedBytesAlloc len (fill l)
  where
    len = Prelude.length l
    fill :: [Char] -> Ptr Word8 -> IO ()
    fill []     _  = return ()
    fill (x:xs) !p = poke p (fromIntegral $ fromEnum x) >> fill xs (p `plusPtr` 1)
