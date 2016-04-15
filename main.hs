{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
-- paradox uses miller cylindrical projection with south america shifted up

import GHC.Generics
import ClausewitzText
import Data.Map (Map, fromList, mapWithKey, empty, (!))
import qualified Data.Map as Map
import qualified Data.Csv as CSV
import Data.Char
import Data.Vector (Vector)
import qualified Data.Text as T
import Data.Monoid
import Data.Maybe
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import Data.Either
import Data.String
import Data.Int
import Control.Monad
import qualified Data.Yaml as YAML
import qualified Codec.BMP as BMP

provincesBMP = do
  BS.readFile "map/provinces.bmp"

-- parse
ssvOptions = CSV.defaultDecodeOptions { CSV.decDelimiter = fromIntegral $ ord ';' }

adjacencies = (CSV.decodeWith ssvOptions CSV.HasHeader :: ByteString ->
            Either String (Vector (Int, Int, ByteString,
            Int, Int, Int, Int, Int, ByteString)))
            <$> BS.readFile "map/adjacencies.csv"
definition = (CSV.decodeWith ssvOptions CSV.HasHeader :: ByteString ->
          Either String (Vector (Int, Int, Int, Int, ByteString)))
          <$> BS.readFile "map/definition.csv"

-- turn list of value into map (ignoring everything but assignment)
toMap l = fromList $ mapMaybe toTuple l
  where
    toTuple (ClausewitzText.Assignment a b) = Just (a, b)
    toTuple _ = Nothing

readAndParseC path = ClausewitzText.decode path <$> readFile path

ambientObject = readAndParseC "map/ambient_object.txt"
area = readAndParseC "map/area.txt"
climate = readAndParseC "map/climate.txt"
continent = readAndParseC "map/continent.txt"
defaultMap = readAndParseC "map/default.map"
positions = readAndParseC "map/positions.txt"
provincegroup = readAndParseC "map/provincegroup.txt"
region = readAndParseC "map/region.txt"
seasons = readAndParseC "map/seasons.txt"
superregion = readAndParseC "map/superregion.txt"
terrain = readAndParseC "map/terrain.txt"
tradeWinds = readAndParseC "map/trade_winds.txt"

width = do
  m <- defaultMap
  return $ case (case m of Right results -> toMap results) ! "width" of ClausewitzText.Float j -> j

height = do
  m <- defaultMap
  return $ case (case m of Right results -> toMap results) ! "height" of ClausewitzText.Float j -> j

data LinearString = LinearString [(Float, Float)] deriving (Generic, Show)
data Polygon = Polygon {
  coordinates :: [LinearString]
} deriving (Generic, Show)

data Feature = Feature {
  geometry :: Polygon,
  id' :: Float
} deriving (Generic, Show)

data FeatureCollection = FeatureCollection {
  features :: [Feature]
} deriving (Generic, Show)

instance ToJSON Feature where
  toEncoding (Feature geometry id') = pairs ("geometry" .= geometry <>
                                        "type" .= Data.Aeson.String "Feature" <>
                                        "properties" .= () <>
                                        "id" .= id')
instance ToJSON FeatureCollection where
  toEncoding (FeatureCollection features) = pairs ("features" .= features <>
                                        "type" .= Data.Aeson.String "FeatureCollection")
instance ToJSON LinearString where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON Polygon where
  toEncoding (Polygon coordinates) = pairs ("coordinates" .= coordinates <> "type" .= Data.Aeson.String "Polygon")

-- convert province to geojson featured
geojson = do
  width <- width
  height <- height
  positions <- positions
  let toFeature key (ClausewitzText.List province) = toFeature' key $ toMap province
      toFeature' key province = Feature {
        geometry = Polygon {
          coordinates = [case province ! "position" of List p -> getLS $ getXY $ map toFloat p]
        },
        id' = key
      }
      coord :: (Float, Float) -> (Float, Float)
      coord (a, b) = (((a - width / 2) / width) * 360, ((b - height / 2) / height) * 134)
      getLS :: [(Float, Float)] -> LinearString
      getLS positions = getLS' $ take (length positions - 1) positions
      getLS' positions = getLS'' $ map coord positions
      getLS'' p = LinearString $ p ++ [head p]
      toFloat (Float v) = v
      getXY :: [Float] -> [(Float, Float)]
      getXY [] = []
      getXY (k:v:t) = (k, v) : getXY t
  return $ case positions of Right result -> Map.elems (Map.mapKeys toFeature $ toMap result)

main = do
  geojson <- geojson
  BS.writeFile "out.json" $ encode $ FeatureCollection geojson
