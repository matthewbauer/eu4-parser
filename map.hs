{-|
Module      : Map
Description : Parsing map assets from Clausewitz Engine
Copyright   : (c) Matthew Bauer, 2016
Maintainer  : mjbauer95@gmail.com
Stability   : experimental

This modules provides a top-level way to use data from
the "default.map" file that point to all of the other
assets used by Clausewitz Engine.
-}
module Map where

import Data.Word
import Data.Char
import Codec.BMP
import Data.Vector (Vector)
import qualified ClausewitzText
import qualified Data.Csv
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Text.ICU.Convert

-- |read and parse the file path
-- "cp1252" is needed to prevent encoding issues
readAndParse :: String -> IO [ClausewitzText.Value]
readAndParse path = do
  conv <- open "cp1252" Nothing
  text <- toUnicode conv <$> BS.readFile path
  let Right v = ClausewitzText.decode path text
  return $! v

-- |lookup a key from the parsed clausewitztext value list
lookup' :: String -> [ClausewitzText.Value] -> ClausewitzText.Value
lookup' _ [] = ClausewitzText.Undefined
lookup' k (x:xs) = case x of
  ClausewitzText.Assignment a b ->
    if a == k
      then b
      else lookup' k xs
  _ -> lookup' k xs

-- |lookup a key from the default map
lookupDefaultMap :: String -> IO ClausewitzText.Value
lookupDefaultMap k = lookup' k <$> defaultMap

-- |used to determine location of bmp assets
mapPath :: String -> String
mapPath = ("map/"++)

-- |read the default map attribute file
readDefaultMapAttributeFile :: String -> IO [ClausewitzText.Value]
readDefaultMapAttributeFile k = do
  ClausewitzText.String v <- lookupDefaultMap k
  readAndParse $! mapPath v

-- |read the default map "definitions.csv" file
-- that lists all of the provinces
readDefaultMapCSVFile :: Data.Csv.FromRecord a => String -> IO (Vector a)
readDefaultMapCSVFile k = do
  ClausewitzText.String k' <- lookupDefaultMap k
  text <- BSL.readFile $ mapPath k'
  let Right v = Data.Csv.decodeWith ssvOptions Data.Csv.HasHeader text
  return $! v

-- |read the default bmp file
readDefaultMapBMPFile :: String -> IO BMP
readDefaultMapBMPFile k = do
  ClausewitzText.String v <- lookupDefaultMap k
  Right bmp <- readBMP $ mapPath v
  return $! bmp

-- |give a pixel color from image data, image width, and pixel coordinates
pixel' :: BS.ByteString -> (Int, Int) -> (Int, Int) -> (Word8, Word8, Word8)
pixel' p (w, h) (x, y)
  | x < 0 = (0,0,0)
  | y < 0 = (0,0,0)
  | x >= w = (0,0,0)
  | y >= h = (0,0,0)
  | otherwise = pixel'' (y * w + x)
  where pixel'' n = (BS.index p (n*4), BS.index p (n*4+1), BS.index p (n*4+2))

-- |give a pixel color from an image tuple and pixel coordinates
pixel :: (BS.ByteString, (Int, Int)) -> (Int, Int) -> (Word8, Word8, Word8)
pixel (s, d) = pixel' s d

-- |semicolon separated values
ssvOptions :: Data.Csv.DecodeOptions
ssvOptions = Data.Csv.defaultDecodeOptions {
  Data.Csv.decDelimiter = fromIntegral $ ord ';'
}

-- |top-level access to default map
defaultMap :: IO [ClausewitzText.Value]
defaultMap = readAndParse $ mapPath "default.map"

-- |provinces file from the above map
provinces :: IO BMP
provinces = readDefaultMapBMPFile "provinces"

-- |terrain file from the above map
terrain :: IO BMP
terrain = readDefaultMapBMPFile "terrain"

-- |rivers file from the above map
rivers :: IO BMP
rivers = readDefaultMapBMPFile "rivers"

-- |heightmap file from the above map
heightmap :: IO BMP
heightmap = readDefaultMapBMPFile "heightmap"

-- |tree file from the above map
treeDefinition :: IO BMP
treeDefinition = readDefaultMapBMPFile "tree_definition"

-- |area file from the above map
area :: IO [ClausewitzText.Value]
area = readDefaultMapAttributeFile "area"

-- |climate file from the above map
climate :: IO [ClausewitzText.Value]
climate = readDefaultMapAttributeFile "climate"

-- |continent file from the above map
continent :: IO [ClausewitzText.Value]
continent = readDefaultMapAttributeFile "continent"

-- |positions file from the above map
positions :: IO [ClausewitzText.Value]
positions = readDefaultMapAttributeFile "positions"

-- |region file from the above map
region :: IO [ClausewitzText.Value]
region = readDefaultMapAttributeFile "region"

-- |seasons file from the above map
seasons :: IO [ClausewitzText.Value]
seasons = readDefaultMapAttributeFile "seasons"

-- |super region filefrom the above map
superregion :: IO [ClausewitzText.Value]
superregion = readDefaultMapAttributeFile "superregion"

-- |trade winds file from the above map
tradeWinds :: IO [ClausewitzText.Value]
tradeWinds = readDefaultMapAttributeFile "trade_winds"

-- |width of the bitmap
width :: IO ClausewitzText.Value
width = lookupDefaultMap "width"

-- |height of the bitmap
height :: IO ClausewitzText.Value
height = lookupDefaultMap "height"

-- |sea starts data
seaStarts :: IO ClausewitzText.Value
seaStarts = lookupDefaultMap "sea_starts"

-- |max provinces data
maxProvinces :: IO ClausewitzText.Value
maxProvinces = lookupDefaultMap "max_provinces"

-- |lakes data
lakes :: IO ClausewitzText.Value
lakes = lookupDefaultMap "only_used_for_random"

-- |adjacencies file from the above map
adjacencies :: IO (Vector (Vector BS.ByteString))
adjacencies = readDefaultMapCSVFile "adjacencies"

-- |definitions file from the above map
definitions :: IO (Vector (Vector BS.ByteString))
definitions = readDefaultMapCSVFile "definitions"
