-- generate a map of features to "region" from the parsed assets

import Prelude hiding (putStrLn)
import Map
import Generator
import Data.Aeson
import Data.ByteString.Lazy.Char8 (putStrLn)

main :: IO ()
main = putStrLn . encode =<< (genmapI <$> region)
