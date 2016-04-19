-- parses Clausewitz Engine Text Files (CETF)
module ClausewitzText where

import Text.Parsec
import Text.Parsec.String

import Data.Functor.Identity (Identity)

-- underlying value piece in all text files
data Value = String String
           | Float Float
           | Bool Bool
           | List [Value]
           | Comment
           | Assignment String Value
           | Identifier {s :: String}
           | Date {year :: Integer, month :: Integer, day :: Integer}
           | Undefined
           deriving Show

parser :: ParsecT [Char] u Identity [Value]
parser = many $ try value

decode :: SourceName -> [Char] -> Either ParseError [Value]
decode = parse parser

value :: GenParser Char st Value
value = do
  v <- try list <|> try string' <|> try date <|> try bool <|> try assignment <|>
       try float <|> try comment <|> try identifier
  spaces
  return v

date :: ParsecT [Char] u Identity Value
date = do
  a <- digit `manyTill` try (char '.')
  b <- digit `manyTill` try (char '.')
  c <- many digit
  return $ Date (read a :: Integer) (read b :: Integer) (read c :: Integer)

identifier :: ParsecT [Char] u Identity Value
identifier = do
  a <- many1 $ alphaNum <|> char '_'
  return $ Identifier a

float :: ParsecT [Char] u Identity Value
float = do
  n <- many1 $ digit <|> char '.' <|> char '-'
  return $ Float (read n :: Float)

bool :: ParsecT [Char] u Identity Value
bool = fmap Bool $ (try (string "yes") >> return True) <|>
  (try (string "no") >> return False)

string' :: ParsecT [Char] u Identity Value
string' = do
  _ <- char '"'
  s' <- anyChar `manyTill` try (char '"')
  return $ String s'

list :: ParsecT [Char] u Identity Value
list = do
  _ <- char '{'
  spaces
  r <- many $ try value
  spaces
  _ <- char '}'
  return $ List r

assignment :: ParsecT [Char] u Identity Value
assignment = do
  i <- identifier
  spaces
  _ <- char '='
  spaces
  n <- value
  return $ Assignment (s i) n

comment :: ParsecT [Char] u Identity Value
comment = do
  _ <- char '#'
  _ <- anyChar `manyTill` try endOfLine
  return Comment
