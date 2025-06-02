{-# OPTIONS_GHC -fno-prof-auto #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

-- |
-- Module      : Basement.Types.OffsetSize
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
module Basement.Types.OffsetSize
    ( Offset (..)
    , Offset8
    , sentinel
    , offsetOfE
    , offsetPlusE
    , offsetMinusE
    , offsetCast
    , offsetSub
    , offsetShiftL
    , offsetShiftR
    , sizeCast
    , sizeLastOffset
    , sizeAsOffset
    , sizeSub
    , offsetAsSize
    , (.==#)
    , CountOf(..)
    , sizeOfE
    , Countable
    , natValCountOf
    ) where

#include "MachDeps.h"

import           Basement.Numerical.Number
import           Basement.Numerical.Additive
import           Basement.Numerical.Subtractive
import           Basement.Numerical.Multiplicative ( Multiplicative (..) )
import           Basement.Numerical.Conversion ( intToWord )
import           Basement.Nat
import           Data.Bits
import           Data.Semigroup ( Semigroup )
import           Data.Proxy
import           Data.List ( foldl' )
import           Data.Int ( Int,  )
import           Data.Typeable ( Typeable )
import           Data.Word ( Word64, Word8 )
#if WORD_SIZE_IN_BITS < 64
import           GHC.IntWord64
#endif
import qualified Prelude
import           Prelude
                   ( Bool (..), Enum (..), Eq (..), Maybe (..), Monoid (..)
                   , Num (fromInteger), Ord (..), Show (..), ($), (.), error
                   , otherwise
                   )


-- | File size in bytes
newtype FileSize = FileSize Word64
    deriving (Show,Eq,Ord)

-- | Offset in bytes used for memory addressing (e.g. in a vector, string, ..)
type Offset8 = Offset Word8

-- | Offset in a data structure consisting of elements of type 'ty'.
--
-- Int is a terrible backing type which is hard to get away from,
-- considering that GHC/Haskell are mostly using this for offset.
-- Trying to bring some sanity by a lightweight wrapping.
newtype Offset ty = Offset Int
    deriving (Show, Eq, Ord, Enum, Additive, Typeable, Num)

sentinel :: Offset ty
sentinel = Offset (-1)

instance IsIntegral (Offset ty) where
    toInteger (Offset i) = toInteger i
instance IsNatural (Offset ty) where
    toNatural (Offset i) = toNatural (intToWord i)
instance Subtractive (Offset ty) where
    type Difference (Offset ty) = CountOf ty
    (Offset a) - (Offset b) = CountOf (a-b)

-- . is offset (as a pointer from a beginning), and # is the size (amount of data)
(.==#) :: Offset ty -> CountOf ty -> Bool
(.==#) (Offset ofs) (CountOf sz) = ofs == sz
{-# INLINE (.==#) #-}

offsetOfE :: CountOf Word8 -> Offset ty -> Offset8
offsetOfE (CountOf sz) (Offset ty) = Offset (ty * sz)

offsetPlusE :: Offset ty -> CountOf ty -> Offset ty
offsetPlusE (Offset ofs) (CountOf sz) = Offset (ofs + sz)

offsetMinusE :: Offset ty -> CountOf ty -> Offset ty
offsetMinusE (Offset ofs) (CountOf sz) = Offset (ofs - sz)

-- | subtract 2 CountOf values of the same type.
--
-- m need to be greater than n, otherwise negative count error ensue
-- use the safer (-) version if unsure.
offsetSub :: Offset a -> Offset a -> Offset a
offsetSub (Offset m) (Offset n) = Offset (m - n)

offsetShiftR :: Int -> Offset ty -> Offset ty2
offsetShiftR n (Offset o) = Offset (o `unsafeShiftR` n)

offsetShiftL :: Int -> Offset ty -> Offset ty2
offsetShiftL n (Offset o) = Offset (o `unsafeShiftL` n)

offsetCast :: Proxy (a -> b) -> Offset a -> Offset b
offsetCast _ (Offset o) = Offset o
{-# INLINE offsetCast #-}

sizeCast :: Proxy (a -> b) -> CountOf a -> CountOf b
sizeCast _ (CountOf sz) = CountOf sz
{-# INLINE sizeCast #-}

-- | subtract 2 CountOf values of the same type.
--
-- m need to be greater than n, otherwise negative count error ensue
-- use the safer (-) version if unsure.
sizeSub :: CountOf a -> CountOf a -> CountOf a
sizeSub (CountOf m) (CountOf n)
    | diff >= 0 = CountOf diff
    | otherwise = error "sizeSub negative size"
  where
    diff = m - n

-- TODO add a callstack, or a construction to prevent size == 0 error
sizeLastOffset :: CountOf a -> Offset a
sizeLastOffset (CountOf s)
    | s > 0     = Offset (pred s)
    | otherwise = error "last offset on size 0"

sizeAsOffset :: CountOf a -> Offset a
sizeAsOffset (CountOf a) = Offset a
{-# INLINE sizeAsOffset #-}

offsetAsSize :: Offset a -> CountOf a
offsetAsSize (Offset a) = CountOf a
{-# INLINE offsetAsSize #-}

-- | CountOf of a data structure.
--
-- More specifically, it represents the number of elements of type `ty` that fit
-- into the data structure.
--
-- >>> length (fromList ['a', 'b', 'c', '🌟']) :: CountOf Char
-- CountOf 4
--
-- Same caveats as 'Offset' apply here.
newtype CountOf ty = CountOf Int
    deriving (Show, Eq, Ord, Enum, Typeable)

instance Num (CountOf ty) where
    fromInteger a = CountOf (fromInteger a)
    (+) (CountOf a) (CountOf b) = CountOf (a+b)
    (-) (CountOf a) (CountOf b)
        | b > a     = CountOf 0
        | otherwise = CountOf (a - b)
    (*) (CountOf a) (CountOf b) = CountOf (a*b)
    abs a = a
    negate _ = error "cannot negate CountOf: use Foundation Numerical hierarchy for this function to not be exposed to CountOf"
    signum (CountOf a) = CountOf (Prelude.signum a)

instance IsIntegral (CountOf ty) where
    toInteger (CountOf i) = toInteger i
instance IsNatural (CountOf ty) where
    toNatural (CountOf i) = toNatural (intToWord i)

instance Additive (CountOf ty) where
    azero = CountOf 0
    (+) (CountOf a) (CountOf b) = CountOf (a+b)
    scale n (CountOf a) = CountOf (scale n a)

instance Subtractive (CountOf ty) where
    type Difference (CountOf ty) = Maybe (CountOf ty)
    (CountOf a) - (CountOf b) | a >= b    = Just . CountOf $ a - b
                              | otherwise = Nothing

instance Semigroup (CountOf ty) where
    (<>) = (+)

instance Monoid (CountOf ty) where
    mempty = azero
    mconcat = foldl' (+) 0

sizeOfE :: CountOf Word8 -> CountOf ty -> CountOf Word8
sizeOfE (CountOf sz) (CountOf ty) = CountOf (ty * sz)

natValCountOf :: forall n ty proxy . (KnownNat n, NatWithinBound (CountOf ty) n) => proxy n -> CountOf ty
natValCountOf n = CountOf $ Prelude.fromIntegral (natVal n)

type instance NatNumMaxBound (CountOf x) = NatNumMaxBound Int
type instance NatNumMaxBound (Offset x) = NatNumMaxBound Int

type Countable ty n = NatWithinBound (CountOf ty) n
