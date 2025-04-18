{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Module      : Data.ByteArray.Bytes
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : stable
-- Portability : Good
--
-- Simple and efficient byte array types
--
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Data.ByteArray.Bytes
    ( Bytes
    ) where

#if MIN_VERSION_base(4,15,0)
import           GHC.Exts (unsafeCoerce#)
#endif
import           GHC.Word
import           GHC.Char (chr)
import           GHC.Types
import           GHC.Prim
import           GHC.Ptr
import           Data.Semigroup
import           Data.Foldable (toList)
import           Data.Memory.PtrMethods
import           Data.Memory.Internal.Imports
import           Data.Memory.Internal.CompatPrim
import           Data.Memory.Internal.Compat      (unsafeDoIO)
import           Data.ByteArray.Types

import           Basement.NormalForm
import           Basement.IntegralConv

-- | Simplest Byte Array
data Bytes = Bytes (MutableByteArray# RealWorld)

instance Show Bytes where
    showsPrec p b = showsPrec p (bytesUnpackChars b [])
instance Eq Bytes where
    (==) = bytesEq
instance Ord Bytes where
    compare = bytesCompare
instance Semigroup Bytes where
    b1 <> b2      = unsafeDoIO $ bytesAppend b1 b2
    sconcat       = unsafeDoIO . bytesConcat . toList
instance Monoid Bytes where
    mempty        = unsafeDoIO (newBytes 0)
instance NFData Bytes where
    rnf b = b `seq` ()
instance NormalForm Bytes where
    toNormalForm b = b `seq` ()
instance ByteArrayAccess Bytes where
    length        = bytesLength
    withByteArray = withBytes
instance ByteArray Bytes where
    allocRet = bytesAllocRet

------------------------------------------------------------------------
newBytes :: Int -> IO Bytes
newBytes (I# sz)
    | booleanPrim (sz <# 0#) = error "Bytes: size must be >= 0"
    | otherwise              = IO $ \s ->
        case newAlignedPinnedByteArray# sz 8# s of
            (# s', mbarr #) -> (# s', Bytes mbarr #)

touchBytes :: Bytes -> IO ()
touchBytes (Bytes mba) = IO $ \s -> case touch# mba s of s' -> (# s', () #)
{-# INLINE touchBytes #-}

sizeofBytes :: Bytes -> Int
sizeofBytes (Bytes mba) = I# (sizeofMutableByteArray# mba)
{-# INLINE sizeofBytes #-}

withPtr :: Bytes -> (Ptr p -> IO a) -> IO a
withPtr b@(Bytes mba) f = do
    a <- f (Ptr (byteArrayContents# (unsafeCoerce# mba)))
    touchBytes b
    return a
------------------------------------------------------------------------

bytesAlloc :: Int -> (Ptr p -> IO ()) -> IO Bytes
bytesAlloc sz f = do
    ba <- newBytes sz
    withPtr ba f
    return ba

bytesConcat :: [Bytes] -> IO Bytes
bytesConcat l = bytesAlloc retLen (copy l)
  where
    !retLen = sum $ map bytesLength l

    copy []     _   = return ()
    copy (x:xs) dst = do
        withPtr x $ \src -> memCopy dst src chunkLen
        copy xs (dst `plusPtr` chunkLen)
      where
        !chunkLen = bytesLength x

bytesAppend :: Bytes -> Bytes -> IO Bytes
bytesAppend b1 b2 = bytesAlloc retLen $ \dst -> do
    withPtr b1 $ \s1 -> memCopy dst                  s1 len1
    withPtr b2 $ \s2 -> memCopy (dst `plusPtr` len1) s2 len2
  where
    !len1   = bytesLength b1
    !len2   = bytesLength b2
    !retLen = len1 + len2

bytesAllocRet :: Int -> (Ptr p -> IO a) -> IO (a, Bytes)
bytesAllocRet sz f = do
    ba <- newBytes sz
    r <- withPtr ba f
    return (r, ba)

bytesLength :: Bytes -> Int
bytesLength = sizeofBytes
{-# LANGUAGE bytesLength #-}

withBytes :: Bytes -> (Ptr p -> IO a) -> IO a
withBytes = withPtr

bytesEq :: Bytes -> Bytes -> Bool
bytesEq b1@(Bytes m1) b2@(Bytes m2)
    | l1 /= l2  = False
    | otherwise = unsafeDoIO $ IO $ \s -> loop 0# s
  where
    !l1@(I# len) = bytesLength b1
    !l2          = bytesLength b2

    loop i s
        | booleanPrim (i ==# len) = (# s, True #)
        | otherwise               =
            case readWord8Array# m1 i s of
                (# s', e1 #) -> case readWord8Array# m2 i s' of
                    (# s'', e2 #) ->
                        if W8# e1 == W8# e2
                            then loop (i +# 1#) s''
                            else (# s'', False #)
    {-# INLINE loop #-}

bytesCompare :: Bytes -> Bytes -> Ordering
bytesCompare b1@(Bytes m1) b2@(Bytes m2) = unsafeDoIO $ loop 0
  where
    !l1  = bytesLength b1
    !l2  = bytesLength b2
    !len = min l1 l2

    loop !i
        | i == len =
            if l1 == l2
                then pure EQ
                else if l1 > l2 then pure GT
                                else pure LT
        | otherwise               = do
            e1 <- read8 m1 i
            e2 <- read8 m2 i
            if e1 == e2
                then loop (i+1)
                else if e1 < e2 then pure LT
                                else pure GT

    read8 m (I# i) = IO $ \s -> case readWord8Array# m i s of
                                    (# s2, e #) -> (# s2, W8# e #)

bytesUnpackChars :: Bytes -> String -> String
bytesUnpackChars (Bytes mba) xs = chunkLoop 0#
  where
    !len = sizeofMutableByteArray# mba
    -- chunk 64 bytes at a time
    chunkLoop :: Int# -> [Char]
    chunkLoop idx
        | booleanPrim (len ==# idx) = []
        | booleanPrim ((len -# idx) ># 63#) =
            bytesLoop idx 64# (chunkLoop (idx +# 64#))
        | otherwise =
            bytesLoop idx (len -# idx) xs

    bytesLoop idx chunkLenM1 paramAcc = unsafeDoIO $
        loop (idx +# chunkLenM1 -# 1#) paramAcc
      where loop i acc
                | booleanPrim (i ==# idx) = do
                    c <- rChar i
                    return (c : acc)
                | otherwise = do
                    c <- rChar i
                    loop (i -# 1#) (c : acc)

    rChar :: Int# -> IO Char
    rChar idx = IO $ \s ->
        case readWord8Array# mba idx s of
            (# s2, w #) -> (# s2, chr (integralUpsize (W8# w)) #)

{-
bytesShowHex :: Bytes -> String
bytesShowHex b = showHexadecimal (withPtr b) (bytesLength b)
{-# NOINLINE bytesShowHex #-}
-}
