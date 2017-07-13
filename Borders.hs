{-|
Module      : Borders
Description : Tools for determining borders from a bitmap world map
Copyright   : (c) Matthew Bauer, 2016
Maintainer  : mjbauer95@gmail.com
Stability   : experimental

This is a collection of helper functions from dealing with a
"color-coded" bitmap map. Each feature of the map is designated
by a different triple. This module helps in determining "borders"
coordinates from each paritcular color.
-}
module Borders where

import Map
import Data.Map hiding (filter)
import Data.Foldable
import Data.ByteString (ByteString)
import Data.Word

-- |The 'filterAllPixels' function generates a list of coordinates for each color
-- that are on the border of each province. All interior pixels are filtered out.
filterAllPixels :: (ByteString, (Int, Int)) -- ^ Image data in the form of (data, (width, height))
                -> Map (Word8, Word8, Word8) [(Int, Int)] -- ^ A map of colors to pixel locations
filterAllPixels (s,(w, h)) = Data.Foldable.foldr' filterAllPixels' empty $
  [(x,y) | x <- [0..w-1], y <- [0..h-1]]
  where
    filterAllPixels' x = unionWith (++) (singleton (pixel' s (w, h) x) [x])

-- |The 'isNotMeaningfulPixel' function determines whether a pixel is useful in
-- determining the shape of the feature. It does this by looking at the surrounding
-- pixels and deciding whether they match. For instance, if a pixel is surrounded by
-- same color pixels, it is considered not meaningful. Likewise, a pixel that has
-- forms part of a horizontal border is also not considered meaningful. On the other
-- hand, a pixel bordering just one other pixel of the same color is considered meaningful.
isNotMeaningfulPixel :: ((Int, Int) -> (Word8, Word8, Word8)) -- ^ A way to get pixel input
                     -> (Int, Int) -- ^ A pixel to test
                     -> Bool -- ^ Whether the pixel is meaningful
isNotMeaningfulPixel p (x, y) = isNotMeaningfulPixel'
  (p (x-1, y-1)) (p (x, y-1)) (p (x+1, y-1))
  (p (x-1, y))   (p (x, y))   (p (x+1, y))
  (p (x-1, y+1)) (p (x, y+1)) (p (x+1, y+1))
  where
    isNotMeaningfulPixel' a b c d e f g h i =
      -- | compare each surrounding pixel to the center pixel as a bool
      fmap (e==) [a, b, c, d, f, g, h, i] `elem` [
        -- | any of the below possibilities are not meaningful
        [True,  True,  True,
         True,         True,
         True,  True,  True],

        [True,  True,  True,
         True,         True,
         False, False, False],

        [False, False, False,
         True,         True,
         True,  True,  True],

        [True,  True,  False,
         True,         False,
         True,  True,  False],

        [False, True,  True,
         False,        True,
         False, True,  True],

        [True,  True,  True,
         False,        True,
         False, False, True],

        [True,  False, False,
         True,         False,
         True,  True,  True],

        [True,  True,  True,
         True,         False,
         True,  False, False],

        [False, False, True,
         False,        True,
         True,  True,  True],

        [True, True,  True,
         True,        True,
         False, True, True],

        [False, True, True,
         True,        True,
         True,  True, True],

        [True, True, True,
         True,       True,
         True, True, False],

        [True, True, False,
         True,       True,
         True, True, True],

        [True, True, False,
         True,       True,
         True, True, False],

        [False, True, False,
         True,        True,
         True,  True, True],

        [False, True, True,
         True,        True,
         False, True, True],

        [True,  True, True,
         True,        True,
         False, True, False],

        [True, True, False,
         True,       True,
         True, True, False],

        [True, True,  False,
         True,        True,
         False, True, True],

        [False, True, True,
         True,        True,
         True,  True, False],

        [True, True,  False,
         True,        True,
         False, True, False],

        [False, True, True,
         True,        True,
         False, True, False],

        [False, True, False,
         True,        True,
         True,  True, False],

        [False, True, False,
         True,        True,
         False, True, True],

        [False, True, False,
         True,        True,
         False, True, False]
      ]

-- |The 'loopPointsOnce' function starts with a point on the border
-- of a feature and loops around the entire feature until it lands
-- back at its starting point.
loopPointsOnce :: ((Int, Int) -> (Word8, Word8, Word8)) -- ^ A way to get pixel data
               -> (Int, Int) -- ^ The starting point
               -> [(Int, Int)] -- ^ A list of points it passed along the way including the starting point
loopPointsOnce p (x, y) = takeWhile (head points /=) $ tail points
  where
    -- | infinite list of looped points
    loopPoints = iterate $ nextPoint p

    -- | find the first point in the infinite list
    points = fst Prelude.<$> loopPoints ((x, y), (-1,-1))

-- |The 'nextPoint' function finds the next point going clockwise
-- around a features border. The predicted next point is determined by
-- current point + direction. If the next point matches the color
-- then that point is returned and direction is turned back two times.
-- Otherwise, the direction is turned clockwise and the next point is
-- unchanged.
nextPoint :: ((Int, Int) -> (Word8, Word8, Word8)) -- ^ a way to get pixel data
          -> ((Int, Int), (Int, Int)) -- ^ a tuple of (coordinate, direction)
          -> ((Int, Int), (Int, Int)) -- ^ a tuple of (coordinate, direction)
nextPoint p ((x, y), (a, b))
  | p (x+a, y+b) == p (x, y) = ((x+a, y+b), counterclockwise (counterclockwise (a, b)))
  | otherwise = nextPoint p ((x, y), clockwise (a, b))
  where
    -- | give next direction going clockwise
    clockwise :: (Int, Int) -> (Int, Int)
    clockwise (1, 0) = (1, -1)
    clockwise (1, -1) = (0, -1)
    clockwise (0, -1) = (-1, -1)
    clockwise (-1, -1) = (-1, 0)
    clockwise (-1, 0) = (-1, 1)
    clockwise (-1, 1) = (0, 1)
    clockwise (0, 1) = (1, 1)
    clockwise (1, 1) = (1, 0)
    clockwise _ = (1, 0)

    -- | give next direction going counter clockwise
    counterclockwise :: (Int, Int) -> (Int, Int)
    counterclockwise (1, 0) = (1, 1)
    counterclockwise (1, 1) = (0, 1)
    counterclockwise (0, 1) = (-1, 1)
    counterclockwise (-1, 1) = (-1, 0)
    counterclockwise (-1, 0) = (-1, -1)
    counterclockwise (-1, -1) = (0, -1)
    counterclockwise (0, -1) = (1, -1)
    counterclockwise (1, -1) = (1, 0)
    counterclockwise _ = (1, 0)
