import Map
import Borders
import GeoJSON
import Data.Vector (toList)
import Data.Aeson
import qualified Data.Text
import Data.Char
import Data.ByteString.Lazy (unpack)

provincePoints provinces = fmap $ usefulPoints provinces

usefulPoints provinces = filter
  (not . (isNotMeaningfulPixel $ pixel provinces)) . findPoints provinces

provinceGeometry provinces definition = GeometryCollection [LineString (points ++ [head points])]
  where points = fmap toPoint (usefulPoints provinces definition)
        toPoint (x,y) = [fromIntegral x, fromIntegral y]

bytesToString = map (chr . fromIntegral)

provinceFeature provinces (i,r,g,b,name) = Feature
  (provinceGeometry provinces (i,r,g,b,name))
  (show i)
  (object [ (Data.Text.pack "name") .= String (Data.Text.pack (bytesToString (unpack name))) ])

geojson = do
  definitions <- definitions
  provinces <- provinces
  return $ FeatureCollection $ fmap (provinceFeature provinces) (toList definitions)

main = (print . encode) =<< geojson
