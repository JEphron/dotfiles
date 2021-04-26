{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ultrawide (UltrawideLayout (..)) where

import Control.Monad
import Data.Ratio
import XMonad
import qualified XMonad.StackSet as W

-- A modified version of ThreeColumns which always keeps a window in the center
-- Designed for very wide monitors where you want to avoid twisting your neck
data UltrawideLayout a = UltrawideLayout
  { threeColNMaster :: !Int,
    threeColDelta :: !Rational,
    threeColFrac :: !Rational
  }
  deriving (Show, Read)

instance LayoutClass UltrawideLayout a where
  pureLayout (UltrawideLayout nMaster _ fraction) rect =
    ap zip (tile3 fraction rect nMaster . length) . W.integrate

  handleMessage l m =
    return $
      msum
        [ fmap resize (fromMessage m),
          fmap incmastern (fromMessage m)
        ]
    where
      resize Shrink = l {threeColFrac = max (-0.5) $ f - d}
      resize Expand = l {threeColFrac = min 1 $ f + d}
      incmastern (IncMasterN x) = l {threeColNMaster = max 0 (n + x)}
      n = threeColNMaster l
      d = threeColDelta l
      f = threeColFrac l
  description _ = "UltraWide"

tile3 :: Rational -> Rectangle -> Int -> Int -> [Rectangle]
tile3 fraction rect nMaster nWindows =
  splitVertically nMaster r1 ++ splitVertically nSlave1 r2 ++ splitVertically nSlave2 r3
  where
    (r1, r2, r3) = split3HorizontallyBy fraction rect
    (s1, s2) = splitHorizontallyBy fraction rect
    nSlave = (nWindows - nMaster)
    nSlave1 = ceiling (nSlave % 2)
    nSlave2 = (nWindows - nMaster - nSlave1)

split3HorizontallyBy :: Rational -> Rectangle -> (Rectangle, Rectangle, Rectangle)
split3HorizontallyBy f (Rectangle sx sy sw sh) =
  ( Rectangle (sx + fromIntegral r3w) sy r1w sh,
    Rectangle sx sy r3w sh,
    Rectangle (sx + fromIntegral r3w + fromIntegral r1w) sy r2w sh
  )
  where
    r1w = ceiling $ fromIntegral sw * f
    r2w = ceiling ((sw - r1w) % 2)
    r3w = sw - r1w - r2w
