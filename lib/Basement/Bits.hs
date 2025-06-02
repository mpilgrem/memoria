{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE NegativeLiterals     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Basement.Bits
-- License     : BSD-style
-- Maintainer  : Haskell Foundation
-- Stability   : experimental
-- Portability : portable
--
#include "MachDeps.h"

module Basement.Bits
  ( BitOps (..)
  ) where

import           Basement.Nat
                   ( KnownNat, Nat, NatWithinBound, type (<=), natVal )
import           Basement.Numerical.Additive ( Additive (..) )
import           Basement.Numerical.Subtractive ( Subtractive (..) )
import           Basement.Numerical.Multiplicative
                   ( IDivisible (..), Multiplicative (..) )
import           Basement.Types.OffsetSize
                   ( CountOf (..), Offset (..), offsetAsSize )
import           Basement.Types.Word128 ( Word128 )
import qualified Basement.Types.Word128 as Word128
                   ( bitwiseAnd, bitwiseOr, bitwiseXor, complement, shiftL
                   , shiftR
                   )
import           Basement.Types.Word256 ( Word256 )
import qualified Basement.Types.Word256 as Word256
                   ( bitwiseAnd, bitwiseOr, bitwiseXor, complement, shiftL
                   , shiftR
                   )
import qualified Data.Bits as OldBits
import           Data.Int ( Int, Int16, Int32, Int64, Int8 )
import           Data.Proxy
import           Data.Typeable ( Typeable )
import           Data.Word ( Word, Word16, Word32, Word64, Word8 )
#if WORD_SIZE_IN_BITS < 64
import           GHC.IntWord64
#endif
import           Numeric.Natural ( Natural )
import qualified Prelude
import           Prelude
                   ( Bool (..), Bounded (..), Enum (..), Eq (..), Num, Ord (..)
                   , Show (..), (&&), (||), error, maybe, not, otherwise
                   )

-- | operation over finite bits
class FiniteBitsOps bits where

    -- | reverse all bits in the argument
    bitFlip   :: bits -> bits

-- | operation over bits
class BitOps bits where
    (.&.)     :: bits -> bits -> bits
    (.|.)     :: bits -> bits -> bits
    (.^.)     :: bits -> bits -> bits
    (.<<.)    :: bits -> CountOf Bool -> bits
    (.>>.)    :: bits -> CountOf Bool -> bits
    -- | construct a bit set with the bit at the given index set.
    bit       :: Offset Bool -> bits
    default bit :: Num bits => Offset Bool -> bits
    bit n = 1 .<<. offsetAsSize n

    -- | test the bit at the given index is set
    isBitSet  :: bits -> Offset Bool -> Bool
    default isBitSet ::
         (Eq bits, Num bits)
      => bits -> Offset Bool -> Bool
    isBitSet x n = x .&. bit n /= 0

    -- | set the bit at the given index
    setBit    :: bits -> Offset Bool -> bits
    default setBit :: Num bits => bits -> Offset Bool -> bits
    setBit x n = x .|. bit n

    -- | clear the bit at the given index
    clearBit  :: bits -> Offset Bool -> bits
    default clearBit :: FiniteBitsOps bits => bits -> Offset Bool -> bits
    clearBit x n = x .&. bitFlip (bit n)

infixl 8 .<<., .>>.
infixl 7 .&.
infixl 6 .^.
infixl 5 .|.

-- | Bool set of 'n' bits.
--
newtype Bits (n :: Nat) = Bits { bitsToNatural :: Natural }
  deriving (Show, Eq, Ord, Typeable)

-- | convenient Type Constraint Alias fot 'Bits' functions
type SizeValid n = (KnownNat n, 1 <= n)

-- convert an 'Int' into a 'Natural'.
-- This functions is not meant to be exported
lift :: Int -> Natural
lift = Prelude.fromIntegral
{-# INLINABLE lift #-}

-- | convert the given 'Natural' into a 'Bits' of size 'n'
--
-- if bits that are not within the boundaries of the 'Bits n' will be truncated.
toBits :: SizeValid n => Natural -> Bits n
toBits nat = Bits nat .&. allOne

-- | construct a 'Bits' with all bits set.
--
-- this function is equivalet to 'maxBound'
allOne :: forall n . SizeValid n => Bits n
allOne = Bits (2 Prelude.^ n Prelude.- midentity)
  where
    n = natVal (Proxy @n)

instance SizeValid n => Enum (Bits n) where
    toEnum i | i < 0 && lift i > bitsToNatural maxi = error "Bits n not within bound"
             | otherwise                            = Bits (lift i)
      where maxi = allOne :: Bits n
    fromEnum (Bits n) = fromEnum n
instance SizeValid n => Bounded (Bits n) where
    minBound = azero
    maxBound = allOne
instance SizeValid n => Additive (Bits n) where
    azero = Bits 0
    (+) (Bits a) (Bits b) = toBits (a + b)
    scale n (Bits a) = toBits (scale n a)
instance SizeValid n => Subtractive (Bits n) where
    type Difference (Bits n) = Bits n
    (-) (Bits a) (Bits b) = maybe azero toBits (a - b)
instance SizeValid n => Multiplicative (Bits n) where
    midentity = Bits 1
    (*) (Bits a) (Bits b) = Bits (a Prelude.* b)
instance SizeValid n => IDivisible (Bits n) where
    div (Bits a) (Bits b) = Bits (a `Prelude.div` b)
    mod (Bits a) (Bits b) = Bits (a `Prelude.mod` b)
    divMod (Bits a) (Bits b) = let (q, r) = Prelude.divMod a b in (Bits q, Bits r)

instance SizeValid n => BitOps (Bits n) where
    (.&.)    (Bits a) (Bits b)    = Bits (a OldBits..&. b)
    (.|.)    (Bits a) (Bits b)    = Bits (a OldBits..|. b)
    (.^.)    (Bits a) (Bits b)    = Bits (a `OldBits.xor` b)
    (.<<.)   (Bits a) (CountOf w) = Bits (a `OldBits.shiftL` w)
    (.>>.)   (Bits a) (CountOf w) = Bits (a `OldBits.shiftR` w)
    bit               (Offset w)  = Bits (OldBits.bit w)
    isBitSet (Bits a) (Offset w)  = OldBits.testBit a w
    setBit   (Bits a) (Offset w)  = Bits (OldBits.setBit a w)
    clearBit (Bits a) (Offset w)  = Bits (OldBits.clearBit a w)
instance (SizeValid n, NatWithinBound (CountOf Bool) n) => FiniteBitsOps (Bits n) where
    bitFlip (Bits a) = Bits (OldBits.complement a)

-- Bool ------------------------------------------------------------------------

instance FiniteBitsOps Bool where
    bitFlip  = not
instance BitOps Bool where
    (.&.) = (&&)
    (.|.) = (||)
    (.^.) = (/=)
    x .<<. 0 = x
    _ .<<. _ = False
    x .>>. 0 = x
    _ .>>. _ = False
    bit 0 = True
    bit _ = False
    isBitSet x 0 = x
    isBitSet _ _ = False
    setBit _ 0 = True
    setBit _ _ = False
    clearBit _ 0 = False
    clearBit x _ = x

-- Word8 ----------------------------------------------------------------------

instance FiniteBitsOps Word8 where
    bitFlip = OldBits.complement
instance BitOps Word8 where
    (.&.)    a b    = a OldBits..&. b
    (.|.)    a b    = a OldBits..|. b
    (.^.)    a b    = a `OldBits.xor` b
    (.<<.)   a (CountOf w) = a `OldBits.shiftL` w
    (.>>.)   a (CountOf w) = a `OldBits.shiftR` w

-- Word16 ---------------------------------------------------------------------

instance FiniteBitsOps Word16 where
    bitFlip = OldBits.complement
instance BitOps Word16 where
    (.&.)    a b    = a OldBits..&. b
    (.|.)    a b    = a OldBits..|. b
    (.^.)    a b    = a `OldBits.xor` b
    (.<<.)   a (CountOf w) = a `OldBits.shiftL` w
    (.>>.)   a (CountOf w) = a `OldBits.shiftR` w

-- Word32 ---------------------------------------------------------------------

instance FiniteBitsOps Word32 where
    bitFlip = OldBits.complement
instance BitOps Word32 where
    (.&.)    a b    = a OldBits..&. b
    (.|.)    a b    = a OldBits..|. b
    (.^.)    a b    = a `OldBits.xor` b
    (.<<.)   a (CountOf w) = a `OldBits.shiftL` w
    (.>>.)   a (CountOf w) = a `OldBits.shiftR` w

-- Word ---------------------------------------------------------------------

instance FiniteBitsOps Word where
    bitFlip = OldBits.complement

instance BitOps Word where
    (.&.)    a b    = a OldBits..&. b
    (.|.)    a b    = a OldBits..|. b
    (.^.)    a b    = a `OldBits.xor` b
    (.<<.)   a (CountOf w) = a `OldBits.shiftL` w
    (.>>.)   a (CountOf w) = a `OldBits.shiftR` w

-- Word64 ---------------------------------------------------------------------

instance FiniteBitsOps Word64 where
    bitFlip = OldBits.complement
instance BitOps Word64 where
    (.&.)    a b    = a OldBits..&. b
    (.|.)    a b    = a OldBits..|. b
    (.^.)    a b    = a `OldBits.xor` b
    (.<<.)   a (CountOf w) = a `OldBits.shiftL` w
    (.>>.)   a (CountOf w) = a `OldBits.shiftR` w

-- Word128 --------------------------------------------------------------------

instance FiniteBitsOps Word128 where
    bitFlip = Word128.complement
instance BitOps Word128 where
    (.&.) = Word128.bitwiseAnd
    (.|.) = Word128.bitwiseOr
    (.^.) = Word128.bitwiseXor
    (.<<.) w (CountOf n) = Word128.shiftL w n
    (.>>.) w (CountOf n) = Word128.shiftR w n

-- Word256 --------------------------------------------------------------------

instance FiniteBitsOps Word256 where
    bitFlip = Word256.complement
instance BitOps Word256 where
    (.&.) = Word256.bitwiseAnd
    (.|.) = Word256.bitwiseOr
    (.^.) = Word256.bitwiseXor
    (.<<.) w (CountOf n) = Word256.shiftL w n
    (.>>.) w (CountOf n) = Word256.shiftR w n

-- Int8 -----------------------------------------------------------------------
instance FiniteBitsOps Int8 where
    bitFlip = OldBits.complement
instance BitOps Int8 where
    (.&.)    a b    = a OldBits..&. b
    (.|.)    a b    = a OldBits..|. b
    (.^.)    a b    = a `OldBits.xor` b
    (.<<.)   a (CountOf w) = a `OldBits.shiftL` w
    (.>>.)   a (CountOf w) = a `OldBits.shiftR` w

-- Int16 ----------------------------------------------------------------------

instance FiniteBitsOps Int16 where
    bitFlip = OldBits.complement
instance BitOps Int16 where
    (.&.)    a b    = a OldBits..&. b
    (.|.)    a b    = a OldBits..|. b
    (.^.)    a b    = a `OldBits.xor` b
    (.<<.)   a (CountOf w) = a `OldBits.shiftL` w
    (.>>.)   a (CountOf w) = a `OldBits.shiftR` w

-- Int32 ----------------------------------------------------------------------

instance FiniteBitsOps Int32 where
    bitFlip = OldBits.complement
instance BitOps Int32 where
    (.&.)    a b    = a OldBits..&. b
    (.|.)    a b    = a OldBits..|. b
    (.^.)    a b    = a `OldBits.xor` b
    (.<<.)   a (CountOf w) = a `OldBits.shiftL` w
    (.>>.)   a (CountOf w) = a `OldBits.shiftR` w
-- Int64 ----------------------------------------------------------------------

instance FiniteBitsOps Int64 where
    bitFlip = OldBits.complement
instance BitOps Int64 where
    (.&.)    a b    = a OldBits..&. b
    (.|.)    a b    = a OldBits..|. b
    (.^.)    a b    = a `OldBits.xor` b
    (.<<.)   a (CountOf w) = a `OldBits.shiftL` w
    (.>>.)   a (CountOf w) = a `OldBits.shiftR` w
