{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE UnboxedTuples       #-}

module Basement.Block.Base
  ( Block (..)
  , MutableBlock (..)
  -- * Basic accessor
  , unsafeNew
  , unsafeThaw
  , unsafeFreeze
  , unsafeCopyBytes
  , unsafeWrite
  , unsafeIndex
    -- * Properties
  , length
  , lengthBytes
  , mutableLengthBytes
    -- * Other methods
  , new
  , newPinned
  , withPtr
  , withMutablePtrHint
  ) where

import           Basement.Bindings.Memory ( sysHsMemcmpBaBa )
import           Basement.Compat.Primitive
                   ( PinnedStatus (..), compatIsByteArrayPinned#
                   , compatIsMutableByteArrayPinned#, toPinnedStatus#
                   , unsafeCoerce#
                   )
import           Basement.Monad ( PrimMonad (..), unsafePrimFromIO )
import           Basement.NormalForm ( NormalForm (..) )
import           Basement.Numerical.Additive ( Additive (..) )
import           Basement.PrimType
                   ( PrimMemoryComparable, PrimType (..), primBaIndex
                   , primMbaWrite
                   )
import           Basement.Types.OffsetSize
                   ( CountOf (..), Offset (..), (.==#), offsetPlusE
                   , sizeAsOffset, sizeOfE
                   )
import           Control.Monad ( unless )
import           Data.Data ( Data (..), DataType, mkNoRepType )
import qualified Data.List
import           Data.Proxy ( Proxy(..) )
import           Data.Semigroup ( Semigroup )
import           Data.Typeable ( Typeable )
import           Data.Word ( Word8 )
import           GHC.IO ( unsafeDupablePerformIO )
import           GHC.Int ( Int (..) )
import           GHC.IsList ( IsList (..) )
import           GHC.Prim
                   ( ByteArray#, MutableByteArray#, byteArrayContents#
                   , copyByteArray#,copyMutableByteArray#
                   , newAlignedPinnedByteArray#, newByteArray#
                   , sizeofByteArray#, sizeofMutableByteArray#, touch#
                   , uncheckedIShiftRL#, unsafeFreezeByteArray#
                   )
import           GHC.Ptr ( Ptr (..) )
import           GHC.ST ( runST )
import           Prelude
                   ( Applicative (..), Bool (..), Eq (..), Monad (..)
                   , Monoid (..), Ord (..), Ordering (..), Semigroup (..)
                   , Show (..), ($), (&&), error, otherwise
                   )

-- | A block of memory containing unpacked bytes representing values of type 'ty'
data Block ty = Block ByteArray#
    deriving (Typeable)

unsafeBlockPtr :: Block ty -> Ptr ty
unsafeBlockPtr (Block arrBa) = Ptr (byteArrayContents# arrBa)
{-# INLINE unsafeBlockPtr #-}

instance Data ty => Data (Block ty) where
    dataTypeOf _ = blockType
    toConstr _   = error "toConstr"
    gunfold _ _  = error "gunfold"

blockType :: DataType
blockType = mkNoRepType "Basement.Block"

instance NormalForm (Block ty) where
    toNormalForm (Block !_) = ()
instance (PrimType ty, Show ty) => Show (Block ty) where
    show v = show (toList v)
instance (PrimType ty, Eq ty) => Eq (Block ty) where
    {-# SPECIALIZE instance Eq (Block Word8) #-}
    (==) = equal
instance (PrimType ty, Ord ty) => Ord (Block ty) where
    compare = internalCompare

instance PrimType ty => Semigroup (Block ty) where
    (<>) = append
instance PrimType ty => Monoid (Block ty) where
    mempty  = empty
    mconcat = concat

instance PrimType ty => IsList (Block ty) where
    type Item (Block ty) = ty
    fromList = internalFromList
    toList = internalToList

-- | A Mutable block of memory containing unpacked bytes representing values of type 'ty'
data MutableBlock ty st = MutableBlock (MutableByteArray# st)

isPinned :: Block ty -> PinnedStatus
isPinned (Block ba) = toPinnedStatus# (compatIsByteArrayPinned# ba)

isMutablePinned :: MutableBlock s ty -> PinnedStatus
isMutablePinned (MutableBlock mba) = toPinnedStatus# (compatIsMutableByteArrayPinned# mba)

length :: forall ty . PrimType ty => Block ty -> CountOf ty
length (Block ba) =
    case primShiftToBytes (Proxy :: Proxy ty) of
        0           -> CountOf (I# (sizeofByteArray# ba))
        (I# szBits) -> CountOf (I# (uncheckedIShiftRL# (sizeofByteArray# ba) szBits))
{-# INLINE[1] length #-}
{-# SPECIALIZE [2] length :: Block Word8 -> CountOf Word8 #-}

lengthBytes :: Block ty -> CountOf Word8
lengthBytes (Block ba) = CountOf (I# (sizeofByteArray# ba))
{-# INLINE[1] lengthBytes #-}

mutableLengthBytes :: MutableBlock ty st -> CountOf Word8
mutableLengthBytes (MutableBlock mba) = CountOf (I# (sizeofMutableByteArray# mba))
{-# INLINE[1] mutableLengthBytes #-}

-- | Create an empty block of memory
empty :: Block ty
empty = Block ba where !(Block ba) = empty_

empty_ :: Block ()
empty_ = runST $ primitive $ \s1 ->
    case newByteArray# 0# s1           of { (# s2, mba #) ->
    case unsafeFreezeByteArray# mba s2 of { (# s3, ba  #) ->
        (# s3, Block ba #) }}

-- | Return the element at a specific index from an array without bounds checking.
--
-- Reading from invalid memory can return unpredictable and invalid values.
-- use 'index' if unsure.
unsafeIndex :: forall ty . PrimType ty => Block ty -> Offset ty -> ty
unsafeIndex (Block ba) = primBaIndex ba
{-# SPECIALIZE unsafeIndex :: Block Word8 -> Offset Word8 -> Word8 #-}
{-# INLINE unsafeIndex #-}

-- | make a block from a list of elements.
internalFromList :: PrimType ty => [ty] -> Block ty
internalFromList l = runST $ do
    ma <- new (CountOf len)
    iter azero l $ \i x -> unsafeWrite ma i x
    unsafeFreeze ma
  where
    !len = Data.List.length l

    iter _  []     _ = return ()
    iter !i (x:xs) z = z i x >> iter (i+1) xs z

-- | transform a block to a list.
internalToList :: forall ty . PrimType ty => Block ty -> [ty]
internalToList blk@(Block ba)
    | len == azero = []
    | otherwise    = loop azero
  where
    !len = length blk
    loop !i | i .==# len = []
            | otherwise  = primBaIndex ba i : loop (i+1)

-- | Check if two blocks are identical
equal :: (PrimType ty, Eq ty) => Block ty -> Block ty -> Bool
equal a b
    | la /= lb  = False
    | otherwise = loop azero
  where
    !la = lengthBytes a
    !lb = lengthBytes b
    lat = length a

    loop !n | n .==# lat = True
            | otherwise  = (unsafeIndex a n == unsafeIndex b n) && loop (n+o1)
    o1 = Offset (I# 1#)
{-# RULES "Block/Eq/Word8" [3]
   forall (a :: Block Word8) b . equal a b = equalMemcmp a b #-}
{-# INLINEABLE [2] equal #-}
-- {-# SPECIALIZE equal :: Block Word8 -> Block Word8 -> Bool #-}

equalMemcmp :: PrimMemoryComparable ty => Block ty -> Block ty -> Bool
equalMemcmp b1@(Block a) b2@(Block b)
    | la /= lb  = False
    | otherwise = unsafeDupablePerformIO (sysHsMemcmpBaBa a 0 b 0 la) == 0
  where
    la = lengthBytes b1
    lb = lengthBytes b2
{-# SPECIALIZE equalMemcmp :: Block Word8 -> Block Word8 -> Bool #-}

-- | Compare 2 blocks
internalCompare :: (Ord ty, PrimType ty) => Block ty -> Block ty -> Ordering
internalCompare a b = loop azero
  where
    !la = length a
    !lb = length b
    !end = sizeAsOffset (min la lb)
    loop !n
        | n == end  = la `compare` lb
        | v1 == v2  = loop (n + Offset (I# 1#))
        | otherwise = v1 `compare` v2
      where
        v1 = unsafeIndex a n
        v2 = unsafeIndex b n
{-# RULES "Block/Ord/Word8" [3] forall (a :: Block Word8) b . internalCompare a b = compareMemcmp a b #-}
{-# NOINLINE internalCompare #-}

compareMemcmp :: PrimMemoryComparable ty => Block ty -> Block ty -> Ordering
compareMemcmp b1@(Block a) b2@(Block b) =
    case unsafeDupablePerformIO (sysHsMemcmpBaBa a 0 b 0 sz) of
        0             -> la `compare` lb
        n | n > 0     -> GT
          | otherwise -> LT
  where
    la = lengthBytes b1
    lb = lengthBytes b2
    sz = min la lb
{-# SPECIALIZE [3] compareMemcmp :: Block Word8 -> Block Word8 -> Ordering #-}

-- | Append 2 blocks together by creating a new bigger block
append :: Block ty -> Block ty -> Block ty
append a b
    | la == azero = b
    | lb == azero = a
    | otherwise = runST $ do
        r  <- unsafeNew Unpinned (la+lb)
        unsafeCopyBytesRO r 0                 a 0 la
        unsafeCopyBytesRO r (sizeAsOffset la) b 0 lb
        unsafeFreeze r
  where
    !la = lengthBytes a
    !lb = lengthBytes b

concat :: forall ty . [Block ty] -> Block ty
concat original = runST $ do
    r <- unsafeNew Unpinned total
    goCopy r zero original
    unsafeFreeze r
  where
    !total = size 0 original
    -- size
    size !sz []     = sz
    size !sz (x:xs) = size (lengthBytes x + sz) xs

    zero = Offset 0

    goCopy r = loop
      where
        loop _  []      = pure ()
        loop !i (x:xs) = do
            unsafeCopyBytesRO r i x zero lx
            loop (i `offsetPlusE` lx) xs
          where !lx = lengthBytes x

-- | Freeze a mutable block into a block.
--
-- If the mutable block is still use after freeze,
-- then the modification will be reflected in an unexpected
-- way in the Block.
unsafeFreeze :: PrimMonad prim => MutableBlock ty (PrimState prim) -> prim (Block ty)
unsafeFreeze (MutableBlock mba) = primitive $ \s1 ->
    case unsafeFreezeByteArray# mba s1 of
        (# s2, ba #) -> (# s2, Block ba #)
{-# INLINE unsafeFreeze #-}

-- | Thaw an immutable block.
--
-- If the immutable block is modified, then the original immutable block will
-- be modified too, but lead to unexpected results when querying
unsafeThaw :: (PrimType ty, PrimMonad prim) => Block ty -> prim (MutableBlock ty (PrimState prim))
unsafeThaw (Block ba) = primitive (#, MutableBlock (unsafeCoerce# ba) #)

-- | Create a new mutable block of a specific size in bytes.
--
-- Note that no checks are made to see if the size in bytes is compatible with the size
-- of the underlaying element 'ty' in the block.
--
-- use 'new' if unsure
unsafeNew :: PrimMonad prim
          => PinnedStatus
          -> CountOf Word8
          -> prim (MutableBlock ty (PrimState prim))
unsafeNew pinSt (CountOf (I# bytes)) = case pinSt of
    Unpinned -> primitive $ \s1 -> case newByteArray# bytes s1 of { (# s2, mba #) -> (# s2, MutableBlock mba #) }
    _        -> primitive $ \s1 -> case newAlignedPinnedByteArray# bytes 8# s1 of { (# s2, mba #) -> (# s2, MutableBlock mba #) }

-- | Create a new unpinned mutable block of a specific N size of 'ty' elements
--
-- If the size exceeds a GHC-defined threshold, then the memory will be
-- pinned. To be certain about pinning status with small size, use 'newPinned'
new :: forall prim ty . (PrimMonad prim, PrimType ty) => CountOf ty -> prim (MutableBlock ty (PrimState prim))
new n = unsafeNew Unpinned (sizeOfE (primSizeInBytes (Proxy :: Proxy ty)) n)

-- | Create a new pinned mutable block of a specific N size of 'ty' elements
newPinned :: forall prim ty . (PrimMonad prim, PrimType ty) => CountOf ty -> prim (MutableBlock ty (PrimState prim))
newPinned n = unsafeNew Pinned (sizeOfE (primSizeInBytes (Proxy :: Proxy ty)) n)

-- | Copy a number of bytes from a MutableBlock to another MutableBlock with specific byte offsets
unsafeCopyBytes :: forall prim ty . PrimMonad prim
                => MutableBlock ty (PrimState prim) -- ^ destination mutable block
                -> Offset Word8                     -- ^ offset at destination
                -> MutableBlock ty (PrimState prim) -- ^ source mutable block
                -> Offset Word8                     -- ^ offset at source
                -> CountOf Word8                       -- ^ number of elements to copy
                -> prim ()
unsafeCopyBytes (MutableBlock dstMba) (Offset (I# d)) (MutableBlock srcBa) (Offset (I# s)) (CountOf (I# n)) =
    primitive $ \st -> (# copyMutableByteArray# srcBa s dstMba d n st, () #)
{-# INLINE unsafeCopyBytes #-}

-- | Copy a number of bytes from a Block to a MutableBlock with specific byte offsets
unsafeCopyBytesRO :: forall prim ty . PrimMonad prim
                  => MutableBlock ty (PrimState prim) -- ^ destination mutable block
                  -> Offset Word8                     -- ^ offset at destination
                  -> Block ty                         -- ^ source block
                  -> Offset Word8                     -- ^ offset at source
                  -> CountOf Word8                       -- ^ number of elements to copy
                  -> prim ()
unsafeCopyBytesRO (MutableBlock dstMba) (Offset (I# d)) (Block srcBa) (Offset (I# s)) (CountOf (I# n)) =
    primitive $ \st -> (# copyByteArray# srcBa s dstMba d n st, () #)
{-# INLINE unsafeCopyBytesRO #-}

-- | write to a cell in a mutable block without bounds checking.
--
-- Writing with invalid bounds will corrupt memory and your program will
-- become unreliable. use 'write' if unsure.
unsafeWrite :: (PrimMonad prim, PrimType ty) => MutableBlock ty (PrimState prim) -> Offset ty -> ty -> prim ()
unsafeWrite (MutableBlock mba)  = primMbaWrite mba
{-# INLINE unsafeWrite #-}

-- | Get a Ptr pointing to the data in the Block.
--
-- Since a Block is immutable, this Ptr shouldn't be
-- to use to modify the contents
--
-- If the Block is pinned, then its address is returned as is,
-- however if it's unpinned, a pinned copy of the Block is made
-- before getting the address.
withPtr :: PrimMonad prim
        => Block ty
        -> (Ptr ty -> prim a)
        -> prim a
withPtr x@(Block ba) f
    | isPinned x == Pinned = f (Ptr (byteArrayContents# ba)) <* touch x
    | otherwise            = do
        arr <- makeTrampoline
        f (unsafeBlockPtr arr) <* touch arr
  where
    makeTrampoline = do
        trampoline <- unsafeNew Pinned (lengthBytes x)
        unsafeCopyBytesRO trampoline 0 x 0 (lengthBytes x)
        unsafeFreeze trampoline

touch :: PrimMonad prim => Block ty -> prim ()
touch (Block ba) =
    unsafePrimFromIO $ primitive $ \s -> case touch# ba s of { s2 -> (# s2, () #) }

-- | Same as 'withMutablePtr' but allow to specify 2 optimisations
-- which is only useful when the MutableBlock is unpinned and need
-- a pinned trampoline to be called safely.
--
-- If skipCopy is True, then the first copy which happen before
-- the call to 'f', is skipped. The Ptr is now effectively
-- pointing to uninitialized data in a new mutable Block.
--
-- If skipCopyBack is True, then the second copy which happen after
-- the call to 'f', is skipped. Then effectively in the case of a
-- trampoline being used the memory changed by 'f' will not
-- be reflected in the original Mutable Block.
--
-- If using the wrong parameters, it will lead to difficult to
-- debug issue of corrupted buffer which only present themselves
-- with certain Mutable Block that happened to have been allocated
-- unpinned.
--
-- If unsure use 'withMutablePtr', which default to *not* skip
-- any copy.
withMutablePtrHint :: forall ty prim a . PrimMonad prim
                   => Bool -- ^ hint that the buffer doesn't need to have the same value as the mutable block when calling f
                   -> Bool -- ^ hint that the buffer is not supposed to be modified by call of f
                   -> MutableBlock ty (PrimState prim)
                   -> (Ptr ty -> prim a)
                   -> prim a
withMutablePtrHint skipCopy skipCopyBack mb f
    | isMutablePinned mb == Pinned = callWithPtr mb
    | otherwise                    = do
        trampoline <- unsafeNew Pinned vecSz
        unless skipCopy $
            unsafeCopyBytes trampoline 0 mb 0 vecSz
        r <- callWithPtr trampoline
        unless skipCopyBack $
            unsafeCopyBytes mb 0 trampoline 0 vecSz
        pure r
  where
    vecSz = mutableLengthBytes mb
    callWithPtr pinnedMb = do
        b <- unsafeFreeze pinnedMb
        f (unsafeBlockPtr b) <* touch b
