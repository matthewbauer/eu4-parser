module Map where

import Data.Word
import Data.Char
import Codec.BMP
import Data.Vector (Vector)

import qualified ClausewitzText
import qualified Data.Csv

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC

readAndParse :: String -> IO [ClausewitzText.Value]
readAndParse path = assert .
  ClausewitzText.decode path =<< Prelude.readFile path
    where assert (Right v) = return v
          assert (Left _) = return []

lookup' :: String -> [ClausewitzText.Value] -> ClausewitzText.Value
lookup' _ [] = ClausewitzText.Undefined
lookup' k (x:xs) = case x of
  ClausewitzText.Assignment a b ->
    if a == k
      then b
      else lookup' k xs
  _ -> lookup' k xs

lookupDefaultMap :: String -> IO ClausewitzText.Value
lookupDefaultMap k = lookup' k <$> defaultMap

mapPath :: String -> String
mapPath = ("map/"++)

readDefaultMapAttributeFile :: String -> IO [ClausewitzText.Value]
readDefaultMapAttributeFile k = do
  ClausewitzText.String v <- lookupDefaultMap k
  readAndParse $ mapPath v

readDefaultMapCSVFile :: String -> (BSL.ByteString -> Either t b) -> IO b
readDefaultMapCSVFile k d = do
  ClausewitzText.String k' <- lookupDefaultMap k
  b <- BSL.readFile $ mapPath k'
  let Right v = d b
  return v

readDefaultMapBMPFile :: String -> IO BMP
readDefaultMapBMPFile k = do
  ClausewitzText.String v <- lookupDefaultMap k
  Right bmp <- readBMP $ mapPath v
  return bmp

pixel' :: BS.ByteString -> (Int, Int) -> (Int, Int) -> [Word8]
pixel' p (w, h) (x, y)
  | x < 0 = [0,0,0]
  | y < 0 = [0,0,0]
  | x >= w = [0,0,0]
  | y >= h = [0,0,0]
  | otherwise = pixel'' p (y * w + x)
  where pixel'' s n = map (BS.index s . ((n * 4) +)) [0,1,2]

pixel :: BMP -> (Int, Int) -> [Word8]
pixel bmp = pixel' (unpackBMPToRGBA32 bmp) (bmpDimensions bmp)

-- semicolon separated values
ssvOptions :: Data.Csv.DecodeOptions
ssvOptions = Data.Csv.defaultDecodeOptions {
  Data.Csv.decDelimiter = fromIntegral $ ord ';'
}

defaultMap :: IO [ClausewitzText.Value]
defaultMap = readAndParse $ mapPath "default.map"

provinces :: IO BMP
provinces = readDefaultMapBMPFile "provinces"

terrain :: IO BMP
terrain = readDefaultMapBMPFile "terrain"

rivers :: IO BMP
rivers = readDefaultMapBMPFile "rivers"

heightmap :: IO BMP
heightmap = readDefaultMapBMPFile "heightmap"

treeDefinition :: IO BMP
treeDefinition = readDefaultMapBMPFile "tree_definition"

area :: IO [ClausewitzText.Value]
area = readDefaultMapAttributeFile "area"

climate :: IO [ClausewitzText.Value]
climate = readDefaultMapAttributeFile "climate"

continent :: IO [ClausewitzText.Value]
continent = readDefaultMapAttributeFile "continent"

positions :: IO [ClausewitzText.Value]
positions = readDefaultMapAttributeFile "positions"

region :: IO [ClausewitzText.Value]
region = readDefaultMapAttributeFile "region"

seasons :: IO [ClausewitzText.Value]
seasons = readDefaultMapAttributeFile "seasons"

superregion :: IO [ClausewitzText.Value]
superregion = readDefaultMapAttributeFile "superregion"

tradeWinds :: IO [ClausewitzText.Value]
tradeWinds = readDefaultMapAttributeFile "trade_winds"

width :: IO ClausewitzText.Value
width = lookupDefaultMap "width"

height :: IO ClausewitzText.Value
height = lookupDefaultMap "height"

seaStarts :: IO ClausewitzText.Value
seaStarts = lookupDefaultMap "sea_starts"

maxProvinces :: IO ClausewitzText.Value
maxProvinces = lookupDefaultMap "max_provinces"

lakes :: IO ClausewitzText.Value
lakes = lookupDefaultMap "only_used_for_random"

adjacencies :: IO (Vector (Int, Int, BSL.ByteString, Int, Int, Int, Int, Int, BSL.ByteString))
adjacencies = readDefaultMapCSVFile "adjacencies"
  (Data.Csv.decodeWith ssvOptions Data.Csv.HasHeader ::
  BSL.ByteString -> Either String
  (Vector (Int, Int, BSL.ByteString, Int, Int, Int, Int, Int, BSL.ByteString)))

definitions :: IO (Vector (Int, Word8, Word8, Word8, BS.ByteString))
definitions = readDefaultMapCSVFile "definitions"
  (Data.Csv.decodeWith ssvOptions Data.Csv.HasHeader ::
  BSL.ByteString -> Either String (Vector (Int, Word8, Word8, Word8, BSC.ByteString)))
