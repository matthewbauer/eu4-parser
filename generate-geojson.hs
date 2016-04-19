import Map
import Borders
import Geojson
import Data.Vector (toList)
import Data.Aeson
import qualified Data.Text
import Data.Char
import qualified Data.ByteString.Lazy
import Data.ByteString.Lazy (unpack)
import Codec.BMP

import Data.Vector (Vector)
import Data.Word
import Data.ByteString.Lazy (ByteString)

usefulPoints :: BMP -> (t, Word8, Word8, Word8, t1) -> [(Int, Int)]
usefulPoints provinces' = (filter
  (not . (isNotMeaningfulPixel (pixel provinces')))) .
  findPoints provinces'

findPoints :: BMP -> (t, Word8, Word8, Word8, t1) -> [(Int, Int)]
findPoints ps p = if length startPoints == 0
    then []
    else loopPointsOnce (pixel ps) $ head startPoints
  where startPoints = lookupProvince p ps

provincePoints :: Num t => BMP -> (t1, Word8, Word8, Word8, t2) -> ((t1, Word8, Word8, Word8, t2), [[t]])
provincePoints provinces' definition =
    (definition, take 500 (fmap toPoint (usefulPoints provinces' definition)))
  where toPoint (x,y) = [fromIntegral x, fromIntegral y]

isGoodProvince :: Foldable t1 => (t, t1 a) -> Bool
isGoodProvince (_, points) = (length points < 500) && (length points > 0)

provinceFeature :: (Show a, ToJSON t) => ((a, t, t, t, ByteString), [Position]) -> Feature
provinceFeature ((i,r,g,b,name), points) = Feature
  (GeometryCollection [Polygon ([points ++ [head points]])])
  ("province" ++ show i)
  (object [
    (Data.Text.pack "name") .= tostr name,
    (Data.Text.pack "color") .= [r,g,b]
  ])
  where tostr = String . Data.Text.pack . (map (chr . fromIntegral)) . unpack

feature :: Show a => Vector (a, Word8, Word8, Word8, ByteString) -> BMP -> [Feature]
feature definitions' provinces' = fmap provinceFeature $
  filter isGoodProvince $ fmap (provincePoints provinces') (toList definitions')

geojson :: IO FeatureCollection
geojson = do
  definitions' <- definitions
  provinces' <- provinces
  return $ FeatureCollection $ feature definitions' provinces'

main :: IO ()
main = (Data.ByteString.Lazy.writeFile "out.json") . encode =<< geojson
