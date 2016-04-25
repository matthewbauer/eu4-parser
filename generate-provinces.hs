import Prelude hiding (putStrLn)
import qualified ClausewitzText
import Data.Aeson
import System.Directory
import Data.Map hiding (foldr, null)
import Data.Char
import Map
import Generator
import Data.ByteString.Lazy.Char8 (putStrLn)
import Data.ByteString.Char8 (readInt)
import Data.Text hiding (empty, takeWhile, singleton, foldr, null)
import Data.Vector hiding (empty, singleton, (++), takeWhile, (!), foldr, null)
import qualified Data.ByteString.Char8 as BS

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
    then readAndParse
      ("history/provinces/" `mappend` (provinceHistoryPath' ! n))
  else return []

definitionsToMap :: [Vector BS.ByteString] -> IO (Map Text (Map Text Value))
definitionsToMap (x:xs) = do
  m <- definitionsToMap' (Data.Vector.toList x)
  m' <- definitionsToMap xs
  return $ m `union` m'
  where
    definitionsToMap' :: [BS.ByteString] -> IO (Map Text (Map Text Value))
    definitionsToMap' (a:_) = do
      let Just (n', _) = readInt a
      file <- readCountryHistoryFile n'
      if null file
        then return empty
        else return $ singleton (pack (show n')) (toMap file)
    definitionsToMap' [] = return empty
definitionsToMap [] = return empty

main :: IO ()
main = putStrLn . encode =<< definitionsToMap =<< (Data.Vector.toList <$> definitions)
