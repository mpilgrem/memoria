{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TypeFamilies               #-}

-- |
-- Module      : Basement.String
-- License     : BSD-style
-- Maintainer  : Foundation
--
-- A String type backed by a UTF8 encoded byte array and all the necessary
-- functions to manipulate the string.
--
module Basement.UTF8.Base
  ( String (..)
  , sToList
  , size
  ) where

import           Basement.Block.Base as BLK ( Block (..), MutableBlock (..) )
import qualified Basement.Block.Base as BLK
import           Basement.Compat.ExtList as List ( sum )
import           Basement.FinalPtr ( withFinalPtr )
import           Basement.Monad
import           Basement.NormalForm
import           Basement.Numerical.Additive
import           Basement.PrimType
import           Basement.Types.OffsetSize
import           Basement.UTF8.Helper ( charToBytes )
import           Basement.UTF8.Types ( Step (..) )
import qualified Basement.Alg.UTF8 as UTF8 ( next, writeUTF8 )
import           Basement.UArray.Base ( UArray )
import qualified Basement.UArray.Base as Vec
import qualified Basement.UArray.Base as C
import           Basement.UArray.Base as Vec
                   ( MUArray, pureST, onBackend, ValidRange(..)
                   , offsetsValidRange
                   )
import qualified Basement.UArray.Mutable as MVec
import           Data.Data
                   ( Data (..), DataType, Fixity (..),  mkConstr, mkNoRepType )
import           Data.String ( IsString (..) )
import           Data.Typeable ( Typeable )
import           Data.Word ( Word8 )
import           GHC.CString (unpackCString#, unpackCStringUtf8#)
import           GHC.Exts ( build )
import           GHC.IsList ( IsList (..) )
import           GHC.Prim
import           GHC.Ptr ( Ptr (..) )
import           GHC.ST ( ST, runST )
import           Prelude
                   ( Applicative (..), Char, Enum (..), Eq (..), Functor (..)
                   , Monad (..), Monoid (..), Ord (..), Semigroup (..)
                   , Show (..), ($), (.), (<$>), error, otherwise
                   )

-- | Opaque packed array of characters in the UTF8 encoding
newtype String = String { toBytes :: UArray Word8 }
    deriving (Typeable, Semigroup, Monoid, Eq, Ord)

-- | Mutable String Buffer.
--
-- Use as an *append* buffer, as UTF8 variable encoding
-- doesn't really allow to change previously written
-- character without potentially shifting bytes.
newtype MutableString st = MutableString (MUArray Word8 st)
    deriving (Typeable)

instance Show String where
    show = show . sToList
instance IsString String where
    fromString = sFromList
instance IsList String where
    type Item String = Char
    fromList = sFromList
    toList = sToList

instance Data String where
    toConstr s   = mkConstr stringType (show s) [] Prefix
    dataTypeOf _ = stringType
    gunfold _ _  = error "gunfold"

instance NormalForm String where
    toNormalForm (String ba) = toNormalForm ba

stringType :: DataType
stringType = mkNoRepType "Foundation.String"

-- | size in bytes.
--
-- this size is available in o(1)
size :: String -> CountOf Word8
size (String ba) = Vec.length ba

-- | Convert a String to a list of characters
--
-- The list is lazily created as evaluation needed
sToList :: String -> [Char]
sToList (String arr) = Vec.onBackend onBA onAddr arr
  where
    (Vec.ValidRange !start !end) = Vec.offsetsValidRange arr
    onBA ba@(BLK.Block _) = loop start
      where
        loop !idx
            | idx == end = []
            | otherwise  = let !(Step c idx') = UTF8.next ba idx in c : loop idx'
    onAddr _ ptr@(Ptr _) = pureST (loop start)
      where
        loop !idx
            | idx == end = []
            | otherwise  = let !(Step c idx') = UTF8.next ptr idx in c : loop idx'
{-# NOINLINE sToList #-}

sToListStream :: String -> (Char -> a -> a) -> a -> a
sToListStream (String arr) k z = Vec.onBackend onBA onAddr arr
  where
    (Vec.ValidRange !start !end) = Vec.offsetsValidRange arr
    onBA ba@(BLK.Block _) = loop start
      where
        loop !idx
            | idx == end = z
            | otherwise  = let !(Step c idx') = UTF8.next ba idx in c `k` loop idx'
    onAddr _ ptr@(Ptr _) = pureST (loop start)
      where
        loop !idx
            | idx == end = z
            | otherwise  = let !(Step c idx') = UTF8.next ptr idx in c `k` loop idx'

{-# RULES "String sToList" [~1] forall s . sToList s = build (sToListStream s) #-}
{-# RULES "String toList" [~1] forall s . toList s = build (sToListStream s) #-}

{-# RULES "String sFromList" forall s .  sFromList (unpackCString# s) = fromModified s #-}
{-# RULES "String sFromList" forall s .  sFromList (unpackCStringUtf8# s) = fromModified s #-}

-- | assuming the given Addr# is a valid modified UTF-8 sequence of bytes
--
-- We only modify the given Unicode Null-character (0xC080) into a null bytes
--
-- FIXME: need to evaluate the kind of modified UTF8 GHC is actually expecting
-- it is plausible they only handle the Null Bytes, which this function actually
-- does.
fromModified :: Addr# -> String
fromModified addr = countAndCopy 0 0
  where
    countAndCopy :: CountOf Word8 -> Offset Word8 -> String
    countAndCopy count ofs =
        case primAddrIndex addr ofs of
            0x00 -> runST $ do
                        mb <- C.newNative_ count (copy count)
                        String <$> Vec.unsafeFreeze mb
            0xC0 -> case primAddrIndex addr (ofs+1) of
                        0x80 -> countAndCopy (count+1) (ofs+2)
                        _    -> countAndCopy (count+2) (ofs+2)
            _    -> countAndCopy (count+1) (ofs+1)

    copy :: CountOf Word8 -> MutableBlock Word8 st -> ST st ()
    copy count mba = loop 0 0
      where loop o i
                | o .==# count = pure ()
                | otherwise    =
                    case primAddrIndex addr i of
                        0xC0 -> case primAddrIndex addr (i+1) of
                                    0x80 -> BLK.unsafeWrite mba o 0x00 >> loop (o+1) (i+2)
                                    b2   -> BLK.unsafeWrite mba o 0xC0 >> BLK.unsafeWrite mba (o+1) b2 >> loop (o+2) (i+2)
                        b1   -> BLK.unsafeWrite mba o b1 >> loop (o+1) (i+1)


-- | Create a new String from a list of characters
--
-- The list is strictly and fully evaluated before
-- creating the new String, as the size need to be
-- computed before filling.
sFromList :: [Char] -> String
sFromList l = runST (new bytes >>= startCopy)
  where
    -- count how many bytes
    !bytes = List.sum $ fmap (charToBytes . fromEnum) l

    startCopy :: MutableString (PrimState (ST st)) -> ST st String
    startCopy ms = loop 0 l
      where
        loop _   []     = freeze ms
        loop idx (c:xs) = write ms idx c >>= \idx' -> loop idx' xs
{-# INLINE [0] sFromList #-}

write :: PrimMonad prim => MutableString (PrimState prim) -> Offset8 -> Char -> prim Offset8
write (MutableString marray) ofs c =
    C.onMutableBackend (\mba@(BLK.MutableBlock _) -> UTF8.writeUTF8 mba (start + ofs) c)
                          (\fptr -> withFinalPtr fptr $ \ptr@(Ptr _) -> UTF8.writeUTF8 ptr (start + ofs) c)
                          marray
  where start = MVec.mutableOffset marray

-- | Allocate a MutableString of a specific size in bytes.
new :: PrimMonad prim
    => CountOf Word8 -- ^ in number of bytes, not of elements.
    -> prim (MutableString (PrimState prim))
new n = MutableString `fmap` Vec.new n

freeze :: PrimMonad prim => MutableString (PrimState prim) -> prim String
freeze (MutableString mba) = String `fmap` C.unsafeFreeze mba
{-# INLINE freeze #-}
