import Prelude hiding (putStrLn)
import Data.Aeson
import Data.ByteString.Lazy.Char8 (putStrLn)
import Map
import Generator

-- instead of list this will generate dictionary for each position

main :: IO ()
main = putStrLn . encode =<< (genmapForwards <$> positions)
