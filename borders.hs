module Borders where

import Codec.BMP
import Map
import Data.Word

-- http://stackoverflow.com/a/4119758/166289
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

filterPixels :: BMP -> (Word8, Word8, Word8) -> [(Int, Int)]
filterPixels bmp = filterPixels' (unpackBMPToRGBA32 bmp) $ bmpDimensions bmp
  where
    filterPixels' s (width, height) (r, g, b) = filter (((==) [r, g, b]) .
      (pixel' s (width, height))) $ cartProd [0..width-1] [0..height-1]

lookupProvince (_, r, g, b, _) ps = filterPixels ps (r, g, b)

findPoints ps p = loopPointsOnce (pixel ps) $ head startPoints
  where startPoints = lookupProvince p ps

neighbors (x, y) = [ (x-1, y-1), (x, y-1), (x+1, y-1),
                     (x-1, y),             (x+1, y),
                     (x-1, y+1), (x, y+1), (x+1, y+1) ]

isNotMeaningfulPixel p (x, y) = isNotMeaningfulPixel'
  (p (x-1, y-1)) (p (x, y-1)) (p (x+1, y-1))
  (p (x-1, y))   (p (x, y))   (p (x+1, y))
  (p (x-1, y+1)) (p (x, y+1)) (p (x+1, y+1))
  where
    isNotMeaningfulPixel' a b c d e f g h i =
      any ((fmap (e==) [a, b, c, d, f, g, h, i])==) [
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
         True,  True,  True]
      ]

neighborsDiff p (x, y) = fmap ((p (x, y))==) $ fmap p (neighbors (x, y))

nextPoint p (x, y) = addPoints (x, y) $ followBorders (neighborsDiff p (x, y))
  where addPoints (x, y) (a, b) = (x+a, y+b)

loopPoints p = iterate $ nextPoint p
loopPointsOnce p (x, y) = takeWhile (not . ((head points)==)) $ tail points
  where points = loopPoints p (x, y)

-- clockwise search
followBorders
  [True,  True,  True,
   True,         True,
   True,  True,  True] = (-1, 0)
followBorders
  [True,True,False,
   True,     False,
   True,True,True] = (1,1)
followBorders
  [False,False,False,
   True,       True,
   True, True, True] = (1,0)
followBorders
  [False,False,False,
   True,       False,
   True, True, True] = (1,1)
followBorders
  [True,False,False,
   True,      True,
   True,True, True] = (1,0)
followBorders
  [True,False,False,
   True,      False,
   True,True, False] = (0,1)
followBorders
  [True,True,False,
   True,     False,
   True,True,False] = (0,1)
followBorders
  [True,True, False,
   True,      False,
   True,False,False] = (-1,1)
followBorders
  [True,True,True,
   True,     False,
   True,True,False] = (0,1)
followBorders
  [True,True, True,
   True,      False,
   True,False,False] = (-1,1)
followBorders
  [True,True,  True,
   True,       False,
   False,False,False] = (-1,0)
followBorders
  [True,True, True,
   True,      True,
   True,False,False] = (-1,1)
followBorders
  [True, True, True,
   False,      False,
   False,False,False] = (-1,-1)
followBorders
  [True, True, True,
   True,       True,
   False,False,True] = (-1,0)
followBorders
  [False,False,False,
   True,       False,
   True, True, False] = (0,1)
followBorders
  [True, True, False,
   True,       False,
   False,False,False] = (-1,0)
followBorders
  [True, True, True,
   True,       True,
   False,False,False] = (-1,0)
followBorders
  [True, True, True,
   False,      True,
   False,False,False] = (-1,-1)
followBorders
  [True,True, True,
   True,      True,
   True,False,True] = (-1,1)
followBorders
  [True, True, True,
   False,      True,
   False,False,True] = (-1,-1)
followBorders
  [False,True, True,
   False,      True,
   False,False,True] = (0,-1)
followBorders
  [False,True,True,
   False,     True,
   False,True,True] = (0,-1)
followBorders
  [True, True,True,
   False,     True,
   False,True,True] = (-1,-1)
followBorders
  [False,True, True,
   False,      True,
   False,False,False] = (0,-1)
followBorders
  [False,False,True,
   False,      True,
   False,False,True] = (1,-1)
followBorders
  [False,False,True,
   False,      True,
   True, True, True] = (1,-1)
followBorders
  [False,True,True,
   False,     True,
   True, True,True] = (0,-1)
followBorders
  [False,False,False,
   False,      True,
   False,True, True] = (1,0)
followBorders
  [False,False,True,
   True,       True,
   True, True, True] = (1,-1)
followBorders
  [False,False,True,
   False,      True,
   False,True, True] = (1,-1)
followBorders
  [False,False,False,
   False,      True,
   True, True, True] = (1,0)
followBorders
  [True,True,True,
   False,    True,
   True,True,True] = (-1,-1)
followBorders
  [False,True, True,
   False,      False,
   False,False,False] = (0,-1)
followBorders
  [False,True,False,
   False,     True,
   False,True,True] = (0,-1)
followBorders
  [False,False,False,
   False,      False,
   False,True, True] = (1,1)
followBorders
  [False,True,True,
   True,      True,
   True, True,True] = (0,-1)
followBorders
  [True,False,False,
   True,      False,
   True,True, True] = (1,1)
followBorders
  [True,False,False,
   True,      False,
   True,False,False] = (-1,1)
followBorders
  [True, True,True,
   True,      True,
   False,True,True] = (-1,0)
followBorders
  [False,False,False,
   False,      True,
   False,False,True] = (1,0)
followBorders
  [True,False,True,
   True,      True,
   True,True, True] = (1,-1)
followBorders
  [False,False,False,
   False,      False,
   True,  True,False] = (0,1)

-- the rest are "iffy" directions
--  they may get stuck in peninsulas
--  messing with these values can make it "unstuck"
followBorders
  [False,True,True,
   False,     False,
   True, True,False] = (0,-1)
followBorders
  [False,True,False,
   True,      False,
   True, True,True] = (1,1)
followBorders
  [True,True, False,
   True,      False,
   False,True,True] = (-1,0)
followBorders
 [False,False,True,
  True,       True,
  False,True, True] = (1,-1)
followBorders
  [False,True,True,
   True,      True,
   True,False,False] = (0,-1)
followBorders
 [True, True,True,
  False,     True,
  False,True,False] = (-1,-1)
followBorders
  [False,False,True,
   False,      True,
   True,True,  False] = (1,-1)
followBorders
 [False,True,True,
  False,     True,
  False,True,False] = (0,-1)
followBorders
  [True, True,False,
   True,      False,
   False,True,False] = (-1,0)
followBorders
  [False,False,False,
   True,       True,
   True, True, False] = (0,1)
followBorders
  [True,True,True,
   True,     False,
   True,True,True] = (1,1)
followBorders
  [False,True, True,
   True,       True,
   False,False,False] = (-1,0)
followBorders
  [False,False,True,
   False,      True,
   False,False,False] = (1,-1)
followBorders
  [False,False,False,
   False,      False,
   True,  True,True] = (1,1)
followBorders
  [False,False,False,
   True,       True,
   False,False,True] = (1,0)
followBorders
  [True,True,True,
   True,     False,
   False,True,True] = (-1,0)
followBorders
  [False,True,False,
   False,     False,
   True, True,True] = (1,1)
followBorders
  [True,False,True,
   True,      True,
   True,True, False] = (0,1)
followBorders
  [True,False, False,
   True,       False,
   False,False,False] = (-1,0)
followBorders
  [True, True, False,
   True,       True,
   False,False,False] = (-1,0)
followBorders
  [False,True,True,
   False,     True,
   True, True,False] = (0,-1)
followBorders
  [False,True,True,
   True,      False,
   True, True,True] = (0,-1)
followBorders
  [True,False,False,
   True,      True,
   True, True,False] = (0,-1)
followBorders
  [True,True, False,
   False,     True,
   False,True,True] = (-1,-1)
followBorders
  [True,False,False,
   True,      True,
   True,False,True] = (1,0)
followBorders
  [False,False,False,
   True,       False,
   False,True, False] = (0,-1)
followBorders
  [False,True,False,
   False,     True,
   True, True,True] = (0,-1)
followBorders
  [False,True,False,
   True,      False,
   True,True, False] = (0,1)
followBorders
  [False,False,False,
   True,       True,
   False, True,True] = (1,0)
followBorders
  [True,False,False,
   True,      True,
   False,True,True] = (1,0)
followBorders
  [False,False,True,
   True,       False,
   True,True,False] = (0,1)
followBorders
  [True,True, True,
   True,      False,
   False,True,False] = (1,0)
followBorders
  [True,False,False,
   False,     True,
   False,True,True] = (1,0)
followBorders
  [True, False,False,
   True,       True,
   False,False,False] = (-1,0)
followBorders
  [True,True, False,
   True,      True,
   True,False,False] = (-1,1)
followBorders
  [False,False,True,
   True,       True,
   True, True, False] = (1,-1)
followBorders
  [False,False,False,
   False,      True,
   True, True, False] = (1,0)
followBorders
  [False,False,False,
   True,       True,
   True, False,False] = (1,0)
followBorders
  [False,False,False,
   True,       False,
   False,True, True] = (1,1)
followBorders
  [True,False, False,
   True,       True,
   False,False,True] = (1,0)
followBorders
  [True, True, False,
   False,      True,
   False,False,False] = (1,0)
followBorders
  [True,True,  False,
   True,       True,
   False,False,True] = (-1,0)
followBorders
  [False,True, True,
   True,       True,
   False,False,True] = (-1,0)
followBorders
  [False,True,True,
   False,     False,
   False,True,False] = (0,-1)
followBorders
  [False,True, False,
   True,       True,
   False,False,True] = (0,-1)
followBorders
  [True,True,False,
   False,    False,
   True,True,True] = (1,1)
followBorders
  [False,True,True,
   False,     False,
   False,True,True] = (0,-1)
followBorders
  [False,False,True,
   True,       True,
   False,False,True] = (1,-1)
followBorders
  [True,False,False,
   True,      True,
   True,False,False] = (-1,1)
followBorders
  [False,True, False,
   False,      True,
   False,False,True] = (0,-1)
followBorders
  [True,True, True,
   False,     False,
   False,True,False] = (-1,-1)
followBorders
  [True,True,False,
   False,    True,
   True,True,True] = (-1,-1)
followBorders
  [False,False,True,
   False,      True,
   False,True, False] = (1,-1)
followBorders
  [False,True,True,
   True,      True,
   True,False,True] = (0,-1)
followBorders
  [False,True,False,
   True,      True,
   False,False,False] = (-1,0)
followBorders
  [True,True, False,
   True,      True,
   True,False,True] = (-1,1)
followBorders
  [False,True,False,
   True,      False,
   False,True,True] = (1,1)
followBorders
  [True,True,False,
   False,    False,
   True,True,False] = (0,1)
followBorders
  [True,True,  False,
   False,      False,
   False,False,False] = (-1,-1)
followBorders
  [False,False,False,
   True,       False,
   True,False, False] = (-1,1)
followBorders
  [False,False,True,
   True,       True,
   True,False, True] = (1,-1)
followBorders
  [True,False, True,
   True,       True,
   False,False,True] = (-1,0)
followBorders
  [False,True,True,
   False,     False,
   True, True,True] = (0,-1)
followBorders
  [False,False,True,
   True,       True,
   False,False,False] = (1,-1)
followBorders
  [False,True, True,
   True,       False,
   False,False,False] = (-1,0)
followBorders
  [True,False,False,
   True,      False,
   False,True,False] = (0,1)
followBorders
  [False,True,True,
   False,     True,
   True,False,False] = (-1,1)
followBorders
  [False,False,False,
   True,       True,
   False,True, False] = (1,0)
followBorders
  [True,False,True,
   True,      False,
   True,False,False] = (-1,1)
followBorders
  [False,True,False,
   True,      False,
   True,False,False] = (-1,1)
followBorders
  [False,True,False,
   False,     False,
   True, True,False] = (0,1)
followBorders
  [False,True,True,
   True,      False,
   True,False,False] = (-1,1)
followBorders
  [False,False,True,
   True,       False,
   True, True, True] = (1,-1)
followBorders
  [True,False,False,
   True,      False,
   False,True,True] = (1,1)
followBorders
  [True,False, True,
   True,       False,
   False,False,False] = (0,-1)
followBorders
  [True,True, False,
   False,     False,
   False,True,True] = (-1,-1)
followBorders
  [True,True,True,
   False,    False,
   True,True,False] = (0,1)
followBorders
  [True, True, True,
   True,       False,
   False,False,True] = (1,1)
followBorders b = (0,0)
