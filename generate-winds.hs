-- generate a map of features to "winds" from the parsed assets

import Prelude hiding (putStrLn)
import Data.Aeson
import Data.ByteString.Lazy.Char8 (putStrLn)
import Data.Map
import qualified ClausewitzText
import Map

genmap :: [ClausewitzText.Value] -> Map String String
genmap [] = empty
genmap (x:xs) = genmap' x `union` genmap xs
  where
    genmap' (ClausewitzText.Assignment n (ClausewitzText.Float b)) =
      singleton n (show (truncate b :: Integer))
    genmap' _ = empty

main :: IO ()
main = putStrLn . encode =<< (genmap <$> tradeWinds)
