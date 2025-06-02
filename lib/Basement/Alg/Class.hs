{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Basement.Alg.Class
  ( Indexable (..)
  , RandomAccess (..)
  ) where

import           Basement.Block.Base ( Block (..), MutableBlock (..) )
import           Basement.Monad ( PrimMonad, PrimState )
import           Basement.PrimType
                   ( PrimType (..), primBaIndex, primMbaRead, primMbaWrite )
import           Basement.Types.OffsetSize ( Offset )
import           Data.Word ( Word64, Word8 )
import           GHC.Ptr ( Ptr (..) )

class Indexable container ty where
  index :: container -> Offset ty -> ty

instance PrimType ty => Indexable (Ptr ty) ty where
  index (Ptr addr) = primAddrIndex addr

instance (PrimType ty) => Indexable (Block ty) ty where
  index (Block ba) = primBaIndex ba
  {-# INLINE index #-}

instance Indexable (Block Word8) Word64 where
  index (Block ba) = primBaIndex ba
  {-# INLINE index #-}

instance Indexable (Ptr Word8) Word64 where
  index (Ptr addr) = primAddrIndex addr

class RandomAccess container prim ty where
  read  :: container -> Offset ty       -> prim ty
  write :: container -> Offset ty -> ty -> prim ()

instance (PrimMonad prim, st ~ PrimState prim, PrimType ty)
         => RandomAccess (MutableBlock ty st) prim ty where
  read (MutableBlock mba) = primMbaRead mba
  write (MutableBlock mba) = primMbaWrite mba

instance (PrimMonad prim, PrimType ty)
         => RandomAccess (Ptr ty) prim ty where
  read (Ptr addr) = primAddrRead addr
  write (Ptr addr) = primAddrWrite addr
