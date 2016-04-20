module Borders where

import Codec.BMP
import Map
import Data.Word

-- http://stackoverflow.com/a/4119758/166289
cartProd :: [t] -> [t1] -> [(t, t1)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

filterPixels :: BMP -> (Word8, Word8, Word8) -> [(Int, Int)]
filterPixels bmp = filterPixels' (unpackBMPToRGBA32 bmp) $ bmpDimensions bmp
  where
    filterPixels' s (w, h) (r, g, b) = filter (([r, g, b] ==) .
      pixel' s (w, h)) $ cartProd [0..w-1] [0..h-1]

lookupProvince :: (t, Word8, Word8, Word8, t1) -> BMP -> [(Int, Int)]
lookupProvince (_, r, g, b, _) ps = filterPixels ps (r, g, b)

neighbors :: (Num t, Num t1) => (t, t1) -> [(t, t1)]
neighbors (x, y) = [ (x-1, y-1), (x, y-1), (x+1, y-1),
                     (x-1, y),             (x+1, y),
                     (x-1, y+1), (x, y+1), (x+1, y+1) ]

isNotMeaningfulPixel :: (Eq a, Num t, Num t1) => ((t, t1) -> a) -> (t, t1) -> Bool
isNotMeaningfulPixel p (x, y) = isNotMeaningfulPixel'
  (p (x-1, y-1)) (p (x, y-1)) (p (x+1, y-1))
  (p (x-1, y))   (p (x, y))   (p (x+1, y))
  (p (x-1, y+1)) (p (x, y+1)) (p (x+1, y+1))
  where
    isNotMeaningfulPixel' a b c d e f g h i =
      fmap (e==) [a, b, c, d, f, g, h, i] `elem` [
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

neighborsDiff :: (Eq a, Num t, Num t1) => ((t, t1) -> a) -> (t, t1) -> [Bool]
neighborsDiff p (x, y) = fmap ((p (x, y) ==) . p) (neighbors (x, y))

nextPoint :: (Eq a, Num t, Num t1) => ((t, t1) -> a) -> (t, t1) -> (t, t1)
nextPoint p (x, y) = addPoints (x, y) $ followBorders (neighborsDiff p (x, y))
  where addPoints (x', y') (a, b) = (x'+a, y'+b)

loopPoints :: (Eq a, Num t, Num t1) => ((t, t1) -> a) -> (t, t1) -> [(t, t1)]
loopPoints p = iterate $ nextPoint p

loopPointsOnce :: (Eq t, Eq t1, Eq a, Num t, Num t1) => ((t, t1) -> a) -> (t, t1) -> [(t, t1)]
loopPointsOnce p (x, y) = takeWhile (head points /=) $ tail points
  where points = loopPoints p (x, y)

followBorders :: (Num t, Num t1) => [Bool] -> (t, t1)

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
followBorders
  [False,True,False,
   False,     True,
   False,True,False] = (0,-1)
followBorders
  [True,False,True,
   True,      True,
   False,True,True] = (1,-1)
followBorders
  [True, True, False,
   False,      True,
   False,False,True] = (-1,-1)
followBorders
  [False,False,False,
   True,       False,
   False,False,False] = (-1,0)
followBorders
  [False,True,True,
   True,      False,
   True,True, False] = (0,-1)
followBorders
  [False,True,False,
   True,      True,
   True,False,False] = (-1,1)
followBorders
  [True,True, False,
   True,      False,
   True,False,True] = (-1,1)
followBorders
  [False,True,False,
   False,     True,
   True, True,False] = (0,-1)
followBorders
  [True,False,True,
   False,     True,
   False,True,True] = (-1,-1)
followBorders
  [False,True, False,
   False,      False,
   False,False,False] = (0,-1)
followBorders
  [True, True,True,
   False,     False,
   False,True,True] = (-1,-1)
followBorders
  [True,False,True,
   True,      False,
   True,True, True] = (1,1)
followBorders
  [False,True,False,
   False,     False,
   False,True,True] = (0,-1)
followBorders _ = (0,0)
