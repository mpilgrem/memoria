{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnliftedFFITypes  #-}

module Basement.Bindings.Memory
  ( sysHsMemcmpBaBa
  , sysHsMemcmpPtrBa
  , sysHsMemcmpPtrPtr
  , sysHsMemcmpBaPtr
  ) where

import           Basement.Types.OffsetSize ( CountOf (..), Offset (..) )
import           Data.Word ( Word8 )
import           Foreign.Ptr ( Ptr )
import           Foreign.C.Types ( CInt (..) )
import           GHC.Prim ( ByteArray# )
import           Prelude ( IO )

foreign import ccall unsafe "_foundation_memcmp" sysHsMemcmpBaBa ::
    ByteArray# -> Offset Word8 -> ByteArray# -> Offset Word8 -> CountOf Word8 -> IO CInt

foreign import ccall unsafe "_foundation_memcmp" sysHsMemcmpBaPtr ::
    ByteArray# -> Offset Word8 -> Ptr a -> Offset Word8 -> CountOf Word8 -> IO CInt

foreign import ccall unsafe "_foundation_memcmp" sysHsMemcmpPtrBa ::
    Ptr a -> Offset Word8 -> ByteArray# -> Offset Word8 -> CountOf Word8 -> IO CInt

foreign import ccall unsafe "_foundation_memcmp" sysHsMemcmpPtrPtr ::
    Ptr a -> Offset Word8 -> Ptr b -> Offset Word8 -> CountOf Word8 -> IO CInt
