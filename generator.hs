module Generator where

import qualified ClausewitzText
import Data.Aeson
import Data.Map hiding (null, foldr')
import Data.Text hiding (empty, takeWhile, singleton, foldr, null)
import qualified Data.HashMap.Lazy
import Data.Foldable

toMapO :: [ClausewitzText.Value] -> Value
toMapO s = Object $ Data.HashMap.Lazy.fromList (Data.Map.toList (toMap s))

toMap :: [ClausewitzText.Value] -> Map Text Value
toMap = foldr' (union . toMap') empty

toMap' :: ClausewitzText.Value -> Map Text Value
toMap' (ClausewitzText.Identifier a) = singleton (pack a) (String $ pack a)
toMap' (ClausewitzText.Float a) = singleton (pack $ show a)
  (Number (fromRational (toRational a)))
toMap' (ClausewitzText.Assignment a b) = singleton (pack a) (toMap'' b)
toMap' _ = empty

toMap'' :: ClausewitzText.Value -> Value
toMap'' (ClausewitzText.Identifier s) = String (pack s)
toMap'' (ClausewitzText.Float f) = Number (fromRational (toRational f))
toMap'' (ClausewitzText.Bool b) = Bool b
toMap'' (ClausewitzText.String s) = String (pack s)
toMap'' (ClausewitzText.List s) = toMapO s
toMap'' _ = Null

genmapForwards :: [ClausewitzText.Value] -> Map String Value
genmapForwards = foldr' (union . genmapForwards') empty
  where
    genmapForwards' (ClausewitzText.Assignment n (ClausewitzText.List b)) =
      singleton n (toMapO b)
    genmapForwards' _ = empty

genmap :: [ClausewitzText.Value] -> Map String String
genmap = foldr' (union . genmap') empty
  where
    genmap' (ClausewitzText.Assignment n (ClausewitzText.List b)) = each n b
    genmap' _ = empty
    each n = foldr' (union . each' n) empty
    each' n (ClausewitzText.Float b) =
      singleton (show (truncate b :: Integer)) n
    each' _ _ = empty

genmapS :: [ClausewitzText.Value] -> Map String String
genmapS = foldr' (union . genmap') empty
  where
    genmap' (ClausewitzText.Assignment n (ClausewitzText.List b)) = each n b
    genmap' _ = empty
    each n = foldr' (union . each' n) empty
    each' n (ClausewitzText.String b) = singleton b n
    each' _ _ = empty

genmapI :: [ClausewitzText.Value] -> Map String String
genmapI = foldr' (union . genmap') empty
  where
    genmap' (ClausewitzText.Assignment n (ClausewitzText.List b)) = each n b
    genmap' _ = empty
    each n = foldr' (union . each' n) empty
    each' n (ClausewitzText.Identifier b) = singleton b n
    each' _ _ = empty
