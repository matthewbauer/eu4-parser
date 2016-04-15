module Map where

import qualified ClausewitzText
import Data.Word
import Data.Char
import qualified Data.Csv

import qualified Data.ByteString.Lazy
import Data.ByteString.Lazy (ByteString)
import Data.ByteString (index)

import Data.Vector ((!),Vector)
import Codec.BMP

readAndParse :: String -> IO [ClausewitzText.Value]
readAndParse path = assert .
  ClausewitzText.decode path =<< readFile path
    where assert (Right v) = return v

lookup' :: String -> [ClausewitzText.Value] -> ClausewitzText.Value
lookup' k [] = ClausewitzText.Undefined
lookup' k (x:xs) = case x of
  ClausewitzText.Assignment a b ->
    if a == k
      then b
      else lookup' k xs
  _ -> lookup' k xs

lookupDefaultMap :: String -> IO ClausewitzText.Value
lookupDefaultMap k = lookup' k <$> defaultMap

map_path :: String -> String
map_path = ("map/"++)

readDefaultMapAttributeFile :: String -> IO [ClausewitzText.Value]
readDefaultMapAttributeFile k = do
  ClausewitzText.String v <- lookupDefaultMap k
  readAndParse $ map_path v

-- readDefaultMapCSVFile :: String -> (ByteString -> ) ->
readDefaultMapCSVFile k d = do
  ClausewitzText.String k <- lookupDefaultMap k
  b <- Data.ByteString.Lazy.readFile $ map_path k
  let Right v = d b
  return v

readDefaultMapBMPFile :: String -> IO BMP
readDefaultMapBMPFile k = do
  ClausewitzText.String v <- lookupDefaultMap k
  Right bmp <- readBMP $ map_path v
  return bmp

pixel' p (width, height) (x, y)
  | x < 0 = [0,0,0]
  | y < 0 = [0,0,0]
  | x >= width = [0,0,0]
  | y >= height = [0,0,0]
  | otherwise = pixel'' p (y * width + x)
  where pixel'' s n = map ((index s) . ((n * 4) +)) [0,1,2]
pixel bmp = pixel' (unpackBMPToRGBA32 bmp) (bmpDimensions bmp)

-- semicolon separated values
ssvOptions = Data.Csv.defaultDecodeOptions {
  Data.Csv.decDelimiter = fromIntegral $ ord ';'
}

defaultMap = readAndParse $ map_path "default.map"

provinces = readDefaultMapBMPFile "provinces"
-- terrain = readDefaultMapBMPFile "terrain"
-- rivers = readDefaultMapBMPFile "rivers"
-- heightmap = readDefaultMapBMPFile "heightmap"
-- tree_definition = readDefaultMapBMPFile "tree_definition"

area = readDefaultMapAttributeFile "area"
climate = readDefaultMapAttributeFile "climate"
continent = readDefaultMapAttributeFile "continent"
positions = readDefaultMapAttributeFile "positions"
region = readDefaultMapAttributeFile "region"
seasons = readDefaultMapAttributeFile "seasons"
superregion = readDefaultMapAttributeFile "superregion"
trade_winds = readDefaultMapAttributeFile "trade_winds"

width = lookupDefaultMap "width"
height = lookupDefaultMap "height"
sea_starts = lookupDefaultMap "sea_starts"
max_provinces = lookupDefaultMap "max_provinces"
lakes = lookupDefaultMap "only_used_for_random"

adjacencies = readDefaultMapCSVFile "adjacencies"
  (Data.Csv.decodeWith ssvOptions Data.Csv.HasHeader ::
  ByteString -> Either String
  (Vector (Int, Int, ByteString, Int, Int, Int, Int, Int, ByteString)))

definitions = readDefaultMapCSVFile "definitions"
  (Data.Csv.decodeWith ssvOptions Data.Csv.HasHeader ::
  ByteString -> Either String (Vector (Int, Word8, Word8, Word8, ByteString)))
