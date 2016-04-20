import Prelude hiding (putStrLn)
import qualified ClausewitzText
import Data.Aeson
import System.Directory
import Data.Map hiding (foldr, null)
import Data.Char
import Map
import Data.ByteString.Lazy.Char8 (putStrLn)
import Data.Text hiding (empty, takeWhile, singleton, foldr, null)
import Data.Vector hiding (empty, singleton, (++), takeWhile, (!), foldr, null)
import qualified Data.HashMap.Lazy

provinceHistories :: IO [FilePath]
provinceHistories = getDirectoryContents "history/provinces"

provincePathMap :: [String] -> Map Int String
provincePathMap = foldr (union . provincePath) empty

provincePath :: String -> Map Int String
provincePath "." = empty
provincePath ".." = empty
provincePath x = provincePath' (takeWhile isDigit x) x

provincePath' :: String -> a -> Map Int a
provincePath' n = singleton (read n :: Int)

readCountryHistoryFile :: Int -> IO [ClausewitzText.Value]
readCountryHistoryFile n = do
  provinceHistoryPath' <- provincePathMap <$> provinceHistories
  if n `member` provinceHistoryPath'
    then readAndParse ("history/provinces/" ++ provinceHistoryPath' ! n)
  else return []

toMap :: [ClausewitzText.Value] -> Map Text Value
toMap = foldr (union . toMap') empty

toMap' :: ClausewitzText.Value -> Map Text Value
toMap' (ClausewitzText.Assignment a b) = singleton (pack a) (toMap'' b)
toMap' _ = empty

toMap'' :: ClausewitzText.Value -> Value
toMap'' (ClausewitzText.Identifier s) = String (pack s)
toMap'' (ClausewitzText.Float f) = Number (fromRational (toRational f))
toMap'' (ClausewitzText.Bool b) = Bool b
toMap'' (ClausewitzText.String s) = String (pack s)
toMap'' (ClausewitzText.List s) = Object (Data.HashMap.Lazy.fromList (Data.Map.toList (toMap s)))
toMap'' _ = Null

definitionsToMap :: [(Int, t, t1, t2, t3)] -> IO (Map Text (Map Text Value))
definitionsToMap (x:xs) = do
  m <- definitionsToMap' x
  m' <- definitionsToMap xs
  return $ m `union` m'
definitionsToMap [] = return empty

definitionsToMap' :: (Int, t, t1, t2, t3) -> IO (Map Text (Map Text Value))
definitionsToMap' (n, _, _, _, _) = do
  file <- readCountryHistoryFile n
  if null file
    then return empty
    else return $ singleton (pack (show n)) (toMap file)

main :: IO ()
main = putStrLn . encode =<< definitionsToMap =<< (Data.Vector.toList <$> definitions)
