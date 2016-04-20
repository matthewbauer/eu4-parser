module Generator where

import Data.Map
import qualified ClausewitzText

genmap :: [ClausewitzText.Value] -> Map String String
genmap [] = empty
genmap (x:xs) = genmap' x `union` genmap xs
  where
    genmap' (ClausewitzText.Assignment n (ClausewitzText.List b)) = each n b
    genmap' _ = empty
    each n (y:ys) = each' n y `union` each n ys
    each _ _ = empty
    each' n (ClausewitzText.Float b) = singleton (show $ truncate b) n
    each' _ _ = empty

genmapS :: [ClausewitzText.Value] -> Map String String
genmapS [] = empty
genmapS (x:xs) = genmapS' x `union` genmapS xs
  where
    genmapS' (ClausewitzText.Assignment n (ClausewitzText.List b)) = each n b
    genmapS' _ = empty
    each n (y:ys) = each' n y `union` each n ys
    each _ _ = empty
    each' n (ClausewitzText.String b) = singleton b n
    each' _ _ = empty


genmapI :: [ClausewitzText.Value] -> Map String String
genmapI [] = empty
genmapI (x:xs) = genmapI' x `union` genmapI xs
  where
    genmapI' (ClausewitzText.Assignment n (ClausewitzText.List b)) = each n b
    genmapI' _ = empty
    each n (y:ys) = each' n y `union` each n ys
    each _ _ = empty
    each' n (ClausewitzText.Identifier b) = singleton b n
    each' _ _ = empty
