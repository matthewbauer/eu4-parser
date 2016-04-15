{-# LANGUAGE OverloadedStrings #-}
module GeoJSON where
import Control.Applicative
import Control.Monad
import Data.Aeson

type Position = [Double]

data Geometry = Point Position
              | MultiPoint [Position]
              | LineString [Position]
              | MultiLineString [Position]
              | Polygon [[Position]]
              | MultiPolygon [[[Position]]]
data GeometryCollection = GeometryCollection [Geometry]
data Feature = Feature GeometryCollection String Value
data FeatureCollection = FeatureCollection [Feature]

instance ToJSON Geometry where
  toJSON (Point p) = object [
    "type" .= String "Point",
    "coordinates" .= p ]
  toJSON (MultiPoint p) = object [
    "type" .= String "MultiPoint",
    "coordinates" .= p ]
  toJSON (LineString p) = object [
    "type" .= String "LineString",
    "coordinates" .= p ]
  toJSON (MultiLineString p) = object [
    "type" .= String "MultiLineString",
    "coordinates" .= p ]
  toJSON (Polygon p) = object [
    "type" .= String "Polygon",
    "coordinates" .= p ]
  toJSON (MultiPolygon p) = object [
    "type" .= String "MultiPolygon",
    "coordinates" .= p ]
instance ToJSON GeometryCollection where
  toJSON (GeometryCollection p) = object [
    "type" .= String "GeometryCollection",
    "geometries" .= p ]
instance ToJSON Feature where
  toJSON (Feature p i v) = object [
    "type" .= String "Feature",
    "id" .= i,
    "geometry" .= p,
    "properties" .= v ]
instance ToJSON FeatureCollection where
  toJSON (FeatureCollection p) = object [
    "type" .= String "FeatureCollection",
    "features" .= p ]
