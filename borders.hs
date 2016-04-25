module Borders where

import Map
import Data.Map hiding (filter)
import Data.Foldable
import Data.ByteString (ByteString)
import Data.Word

-- http://stackoverflow.com/a/4119758/166289
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

filterAllPixels :: (ByteString, (Int, Int)) -> Map (Word8, Word8, Word8) [(Int, Int)]
filterAllPixels (s,(w, h)) = Data.Foldable.foldr' filterAllPixels' empty $
  cartProd [0..w-1] [0..h-1]
  where
    filterAllPixels' x = unionWith (++) (singleton (pixel' s (w, h) x) [x])

isNotMeaningfulPixel :: ((Int, Int) -> (Word8, Word8, Word8)) -> (Int, Int) -> Bool
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
         True,  True, False]
      ]

loopPointsOnce :: ((Int, Int) -> (Word8, Word8, Word8)) -> (Int, Int) -> [(Int, Int)]
loopPointsOnce p (x, y) = takeWhile (head points /=) $ tail points
  where points = fst Prelude.<$> loopPoints ((x, y), (-1,-1))
        loopPoints = iterate $ nextPoint p

nextPoint :: ((Int, Int) -> (Word8, Word8, Word8)) -> ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
nextPoint p ((x, y), (a, b))
  | p (x+a, y+b) == p (x, y) = ((x+a, y+b), counterclockwise (counterclockwise (a, b)))
  | otherwise = nextPoint p ((x, y), clockwise (a, b))
  where
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
