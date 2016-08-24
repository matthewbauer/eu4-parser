-- generate a map of features to "countries" from the parsed assets

import Prelude hiding (putStrLn)
import Data.Aeson
import System.Directory
import Data.Map hiding (foldr, null)
import Data.Char
import Generator
import Map
import Data.ByteString.Lazy.Char8 (putStrLn)

skip :: String -> Bool
skip "." = True
skip ".." = True
skip _ = False

historyDir :: String
historyDir = "history/countries/"

countryId :: String -> String
countryId = takeWhile isAlpha

countryName :: String -> String
countryName = takeWhile isAlpha . drop 5

countryPath :: String -> String
countryPath = (++) historyDir

countryHistories :: IO [FilePath]
countryHistories = getDirectoryContents historyDir

countryMap' :: String -> IO Value
countryMap' x = do
  file <- readAndParse (countryPath x)
  return $ toMapO file

countryMap :: [String] -> IO (Map String Value)
countryMap (x:xs) = do
  let i = countryId x
  m <- countryMap' x
  m' <- countryMap xs
  return $ singleton i m `union` m'
countryMap [] = return empty

countries :: IO (Map String Value)
countries = do
  c <- countryHistories
  countryMap $ Prelude.filter (not . skip) c

main :: IO ()
main = putStrLn . encode =<< countries
