-- generate a map of features to "area" from the parsed assets

import Prelude hiding (putStrLn)
import Map
import Generator
import Data.Aeson
import Data.ByteString.Lazy.Char8 (putStrLn)

main :: IO ()
main = putStrLn . encode =<< (genmap <$> area)
