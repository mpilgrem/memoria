{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoStarIsType              #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Basement.Nat
  ( Nat
  , KnownNat
  , natVal
  , type (<=)
  , type (+)
  , type (*)
  , type (-)
  , CmpNat
    -- * Nat convertion
  , natValNatural
  , natValInt
  , natValWord64
    -- * Maximum bounds
  , NatNumMaxBound
    -- * Constraint
  , NatWithinBound
  ) where

#include "MachDeps.h"

import           Basement.Types.Word128 ( Word128 )
import           Basement.Types.Word256 ( Word256 )
import           Data.Int ( Int, Int16, Int32, Int64, Int8 )
import           Data.Type.Bool
import           Data.Word ( Word, Word16, Word32, Word64, Word8 )
import           GHC.TypeLits
import qualified Prelude ( fromIntegral )
import           Prelude ( Bool (..), Char, Integer )

natValNatural :: forall n proxy . KnownNat n => proxy n -> Natural
natValNatural n = Prelude.fromIntegral (natVal n)

natValInt :: forall n proxy . (KnownNat n, NatWithinBound Int n) => proxy n -> Int
natValInt n = Prelude.fromIntegral (natVal n)

natValWord64 :: forall n proxy . (KnownNat n, NatWithinBound Word64 n) => proxy n -> Word64
natValWord64 n = Prelude.fromIntegral (natVal n)

-- | Get Maximum bounds of different Integral / Natural types related to Nat
type family NatNumMaxBound ty :: Nat

type instance NatNumMaxBound Char   = 0x10ffff
type instance NatNumMaxBound Int64  = 0x7fffffffffffffff
type instance NatNumMaxBound Int32  = 0x7fffffff
type instance NatNumMaxBound Int16  = 0x7fff
type instance NatNumMaxBound Int8   = 0x7f
type instance NatNumMaxBound Word256 = 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
type instance NatNumMaxBound Word128 = 0xffffffffffffffffffffffffffffffff
type instance NatNumMaxBound Word64 = 0xffffffffffffffff
type instance NatNumMaxBound Word32 = 0xffffffff
type instance NatNumMaxBound Word16 = 0xffff
type instance NatNumMaxBound Word8  = 0xff
#if WORD_SIZE_IN_BITS == 64
type instance NatNumMaxBound Int    = NatNumMaxBound Int64
type instance NatNumMaxBound Word   = NatNumMaxBound Word64
#else
type instance NatNumMaxBound Int    = NatNumMaxBound Int32
type instance NatNumMaxBound Word   = NatNumMaxBound Word32
#endif

-- | Check if a Nat is in bounds of another integral / natural types
type family NatInBoundOf ty n where
    NatInBoundOf Integer n = 'True
    NatInBoundOf Natural n = 'True
    NatInBoundOf ty      n = n <=? NatNumMaxBound ty

-- | Constraint to check if a natural is within a specific bounds of a type.
--
-- i.e. given a Nat `n`, is it possible to convert it to `ty` without losing information
type family NatWithinBound ty (n :: Nat) where
    NatWithinBound ty n = If (NatInBoundOf ty n)
        (() ~ ())
        (TypeError ('Text "Natural " ':<>: 'ShowType n ':<>: 'Text " is out of bounds for " ':<>: 'ShowType ty))
