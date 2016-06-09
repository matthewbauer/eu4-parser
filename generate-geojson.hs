{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (putStrLn)
import Map
import Borders
import Geojson
import Data.Aeson
import qualified Data.Vector as Vector
import Codec.BMP
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString.Lazy.Char8 (putStrLn)
import Data.Text.ICU.Convert
import Data.Map ((!),Map,member)
import Data.Word
import Data.List

findPoints :: Map (Word8, Word8, Word8) [(Int, Int)] -> (BSC.ByteString, (Int, Int)) -> [BSC.ByteString] -> [[(Int, Int)]]
findPoints xs ps p = case rgb p of
      (Just (r, _), Just (g, _), Just (b, _)) ->
        if rgb `member` xs
          then if null pixels
            then []
            else findPoints' pixels
        else []
        where
          rgb = (fromIntegral r, fromIntegral g, fromIntegral b)
          pixels = goodPixels $ xs ! rgb
      _ -> []
  where
    rgb (_:b:c:d:_) = (BSC.readInt b, BSC.readInt c, BSC.readInt d)
    rgb _ = (Nothing, Nothing, Nothing)
    goodPixels = filter $ not . isNotMeaningfulPixel (pixel ps)
    -- findPoints' = foldr (loopPointsOnce (pixel ps))
    findPoints' (x:xs') = points : findPoints' (xs' \\ points)
      where points = loopPointsOnce (pixel ps) x
    findPoints' [] = []

provinceFeature :: Converter -> ([BSC.ByteString], [[Position]]) -> Feature
provinceFeature conv (i':r':g':b':name':_, points) = provinceFeature'
  (BSC.readInt i', BSC.readInt r', BSC.readInt g', BSC.readInt b', toUnicode conv name')
  where
    provinceFeature' (Just (i,_), Just (r,_), Just (g,_), Just (b,_), name) =
      provinceFeature'' (i, r, g, b, name)
    provinceFeature' _ = Feature (GeometryCollection []) "" Null
    provinceFeature'' (i, r, g, b, name) = Feature
      (GeometryCollection $ fmap pointPolygon points)
      (show i)
      (object [
        "name" .= name,
        "color" .= [r, g, b]
      ])
    pointPolygon :: [Position] -> Geometry
    pointPolygon points' = Polygon [points' `mappend` [head points']]
provinceFeature _ _ = Feature (GeometryCollection []) "" Null

feature :: Map (Word8, Word8, Word8) [(Int, Int)] -> (BSC.ByteString, (Int, Int)) -> Converter -> Vector.Vector (Vector.Vector BSC.ByteString) -> Vector.Vector Feature
feature xs provinces' conv = Vector.filter (Feature (GeometryCollection []) "" Null /=) .
  Vector.map (provinceFeature conv . provincePoints . Vector.toList)
  where
    provincePoints definition = (definition, toPoints $ findPoints xs provinces' definition)
    toPoint (x,y) = [fromIntegral x, fromIntegral y]
    toPoints = fmap $ fmap toPoint

geojson :: IO FeatureCollection
geojson = do
  definitions' <- definitions
  provinces' <- provinces
  let rgba = unpackBMPToRGBA32 provinces'
  let dims = bmpDimensions provinces'
  let xs = filterAllPixels (rgba, dims)
  conv <- open "cp1252" Nothing
  return . FeatureCollection $! feature xs (rgba, dims) conv definitions'

main :: IO ()
main = putStrLn . encode =<< geojson
