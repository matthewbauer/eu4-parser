import Map
import Borders
import GeoJSON
import Data.Vector (toList)
import Data.Aeson
import qualified Data.Text
import Data.Char
import qualified Data.ByteString.Lazy
import Data.ByteString.Lazy (unpack)
import Codec.BMP

import Data.Vector ((!),Vector)
import Data.Word
import Data.ByteString.Lazy (ByteString)

-- usefulPoints :: BMP -> [(Int,Int)]
usefulPoints provinces = (filter
  (not . (isNotMeaningfulPixel (pixel provinces)))) .
  findPoints provinces

findPoints ps p = loopPointsOnce (pixel ps) $ head startPoints
  where startPoints = lookupProvince p ps

provincePoints provinces definition =
    (definition, take 500 (fmap toPoint (usefulPoints provinces definition)))
  where toPoint (x,y) = [fromIntegral x, fromIntegral y]

isGoodProvince (_, points) = (length points < 500) && (length points > 0)

provinceFeature ((i,r,g,b,name), points) = Feature
  (GeometryCollection [LineString (points ++ [head points])])
  (show i)
  (object [
    (Data.Text.pack "name") .= tostr name
  ])
  where tostr = String . Data.Text.pack . (map (chr . fromIntegral)) . unpack

feature definitions provinces = fmap provinceFeature $
  filter isGoodProvince $ fmap (provincePoints provinces) (toList definitions)

-- geojson :: IO (FeatureCollection)
geojson = do
  definitions <- definitions
  provinces <- provinces
  return $ FeatureCollection $ feature definitions provinces

main = (Data.ByteString.Lazy.writeFile "out.json") . encode =<< geojson
