{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MagicHash          #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE TypeFamilies       #-}

module Basement.Numerical.Conversion
  ( intToInt64
  , int64ToInt
  , intToWord
  , wordToWord64
  , word64ToWord
  , wordToInt
  , charToInt
  , int64ToWord64
  ) where

#include "MachDeps.h"

import           GHC.Int
import           GHC.Prim hiding ( word64ToWord# )
import qualified GHC.Prim
import           GHC.Types
import           GHC.Word

#if WORD_SIZE_IN_BITS < 64
import GHC.IntWord64
#endif

intToInt64 :: Int -> Int64
#if WORD_SIZE_IN_BITS == 64
#if __GLASGOW_HASKELL__ >= 904
intToInt64 (I# i) = I64# (intToInt64# i)
#else
intToInt64 (I# i) = I64# i
#endif
#else
intToInt64 (I# i) = I64# (intToInt64# i)
#endif

int64ToInt :: Int64 -> Int
#if WORD_SIZE_IN_BITS == 64
#if __GLASGOW_HASKELL__ >= 904
int64ToInt (I64# i) = I# (int64ToInt# i)
#else
int64ToInt (I64# i) = I# i
#endif
#else
int64ToInt (I64# i) = I# (int64ToInt# i)
#endif

wordToWord64 :: Word -> Word64
#if WORD_SIZE_IN_BITS == 64
#if __GLASGOW_HASKELL__ >= 904
wordToWord64 (W# i) = W64# (wordToWord64# i)
#else
wordToWord64 (W# i) = W64# i
#endif
#else
wordToWord64 (W# i) = W64# (wordToWord64# i)
#endif

word64ToWord :: Word64 -> Word
#if WORD_SIZE_IN_BITS == 64
#if __GLASGOW_HASKELL__ >= 904
word64ToWord (W64# i) = W# (GHC.Prim.word64ToWord# i)
#else
word64ToWord (W64# i) = W# i
#endif
#else
word64ToWord (W64# i) = W# (word64ToWord# i)
#endif

int64ToWord64 :: Int64 -> Word64
#if WORD_SIZE_IN_BITS == 64
#if __GLASGOW_HASKELL__ >= 904
int64ToWord64 (I64# i) = W64# (int64ToWord64# i)
#else
int64ToWord64 (I64# i) = W64# (int2Word# i)
#endif
#else
int64ToWord64 (I64# i) = W64# (int64ToWord64# i)
#endif

wordToInt :: Word -> Int
wordToInt (W# word) = I# (word2Int# word)

intToWord :: Int -> Word
intToWord (I# i) = W# (int2Word# i)

charToInt :: Char -> Int
charToInt (C# x) = I# (ord# x)
