-- generate a map of features to "positions" from the parsed assets

import Prelude hiding (putStrLn)
import Data.Aeson
import Data.ByteString.Lazy.Char8 (putStrLn)
import Map
import Generator

main :: IO ()
main = putStrLn . encode =<< (genmapForwards <$> positions)
