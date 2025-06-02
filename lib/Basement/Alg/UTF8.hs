{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE MagicHash          #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE TypeFamilies       #-}

module Basement.Alg.UTF8
  ( next
  , writeUTF8
  ) where

import           Basement.Alg.Class ( Indexable (..), RandomAccess (..) )
import           Basement.Compat.Primitive ( bool# )
import           Basement.Monad ( PrimMonad )
import           Basement.Numerical.Additive ( (+) )
import           Basement.Types.OffsetSize ( Offset (..), Offset8 )
import           Basement.UTF8.Helper
                   ( toChar1, toChar2, toChar3, toChar4 )
import           Basement.UTF8.Table ( getNbBytes )
import           Basement.UTF8.Types ( Step (..), StepASCII (..) )
import           GHC.Int ( Int (..) )
import           GHC.Prim
                   ( Word#, wordToWord8#, int2Word#, and#, or#
                   , uncheckedShiftRL#, ltWord#
                   )
import           GHC.Word ( Word8 (..) )
import           Prelude
                   ( Applicative (..), Char, Enum (..), Monad (..)
                   , Semigroup (..), Show (..), error, otherwise
                   )

nextAscii :: Indexable container Word8 => container -> Offset Word8 -> StepASCII
nextAscii ba n = StepASCII w
  where
    !w = index ba n
{-# INLINE nextAscii #-}

next :: Indexable container Word8 => container -> Offset8 -> Step
next ba n =
    case getNbBytes h of
        0 -> Step (toChar1 h) (n + Offset 1)
        1 -> Step (toChar2 h (index ba (n + Offset 1))) (n + Offset 2)
        2 -> Step (toChar3 h (index ba (n + Offset 1))
                             (index ba (n + Offset 2))) (n + Offset 3)
        3 -> Step (toChar4 h (index ba (n + Offset 1))
                             (index ba (n + Offset 2))
                             (index ba (n + Offset 3))) (n + Offset 4)
        r -> error ("next: internal error: invalid input: offset=" <> show n <> " table=" <> show r <> " h=" <> show (stepAsciiRawValue h))
  where
    !h = nextAscii ba n
{-# INLINE next #-}

writeUTF8 :: (PrimMonad prim, RandomAccess container prim Word8)
          => container -> Offset8 -> Char -> prim Offset8
writeUTF8 mba !i !c
    | bool# (ltWord# x 0x80##   ) = encode1
    | bool# (ltWord# x 0x800##  ) = encode2
    | bool# (ltWord# x 0x10000##) = encode3
    | otherwise                   = encode4
  where
    !(I# xi) = fromEnum c
    !x       = int2Word# xi

    encode1 = write mba i (W8# (wordToWord8# x)) >> pure (i + Offset 1)
    encode2 = do
        let x1  = or# (uncheckedShiftRL# x 6#) 0xc0##
            x2  = toContinuation x
        write mba i     (W8# (wordToWord8# x1))
        write mba (i+1) (W8# (wordToWord8# x2))
        pure (i + Offset 2)

    encode3 = do
        let x1  = or# (uncheckedShiftRL# x 12#) 0xe0##
            x2  = toContinuation (uncheckedShiftRL# x 6#)
            x3  = toContinuation x
        write mba i            (W8# (wordToWord8# x1))
        write mba (i+Offset 1) (W8# (wordToWord8# x2))
        write mba (i+Offset 2) (W8# (wordToWord8# x3))
        pure (i + Offset 3)

    encode4 = do
        let x1  = or# (uncheckedShiftRL# x 18#) 0xf0##
            x2  = toContinuation (uncheckedShiftRL# x 12#)
            x3  = toContinuation (uncheckedShiftRL# x 6#)
            x4  = toContinuation x
        write mba i            (W8# (wordToWord8# x1))
        write mba (i+Offset 1) (W8# (wordToWord8# x2))
        write mba (i+Offset 2) (W8# (wordToWord8# x3))
        write mba (i+Offset 3) (W8# (wordToWord8# x4))
        pure (i + Offset 4)

    toContinuation :: Word# -> Word#
    toContinuation w = or# (and# w 0x3f##) 0x80##
{-# INLINE writeUTF8 #-}
