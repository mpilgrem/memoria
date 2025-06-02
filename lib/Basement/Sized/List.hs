{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

-- |
-- Module      : Basement.Sized.List
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- A Nat-sized list abstraction
--
-- Using this module is limited to GHC 7.10 and above.
--
module Basement.Sized.List
    ( ListN
    , toListN
    , unListN
    ) where

import           Basement.Nat
import           Basement.NormalForm
import           Data.Int ( Int )
import           Data.Proxy
import           Data.Typeable ( Typeable )
import           GHC.Generics ( Generic (..) )
import qualified Prelude
import           Prelude ( Eq (..), Maybe (..), Ord (..), Show (..), otherwise )

-- | A Typed-level sized List equivalent to [a]
newtype ListN (n :: Nat) a = ListN { unListN :: [a] }
    deriving (Eq,Ord,Typeable,Generic)

instance Show a => Show (ListN n a) where
    show (ListN l) = show l

instance NormalForm a => NormalForm (ListN n a) where
    toNormalForm (ListN l) = toNormalForm l

-- | Try to create a ListN from a List, succeeding if the length is correct
toListN ::
     forall (n :: Nat) a . (KnownNat n, NatWithinBound Int n)
  => [a] -> Maybe (ListN n a)
toListN l
    | expected == Prelude.fromIntegral (Prelude.length l) = Just (ListN l)
    | otherwise                                           = Nothing
  where
    expected = natValInt (Proxy :: Proxy n)
