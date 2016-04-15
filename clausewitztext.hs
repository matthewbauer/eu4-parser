-- parses Clausewitz Engine Text Files (CETF)
module ClausewitzText where

import Text.Parsec
import Text.Parsec.String

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

parser = many $ try value
decode = parse parser

value :: GenParser Char st Value
value = do
  v <- try list <|> try string' <|> try date <|> try bool <|> try assignment <|>
       try float <|> try comment <|> try identifier
  spaces
  return v

date = do
  a <- digit `manyTill` try (char '.')
  b <- digit `manyTill` try (char '.')
  c <- many digit
  return $ Date (read a :: Integer) (read b :: Integer) (read c :: Integer)

identifier = do
  a <- many1 $ alphaNum <|> char '_'
  return $ Identifier a

float = do
  n <- many1 $ digit <|> char '.' <|> char '-'
  return $ Float (read n :: Float)

bool = fmap Bool $ (try (string "yes") >> return True) <|> (try (string "no") >> return False)

string' = do
  char '"'
  s <- anyChar `manyTill` try (char '"')
  return $ String s

list = do
  char '{'
  spaces
  r <- many $ try value
  spaces
  char '}'
  return $ List r

assignment = do
  i <- identifier
  spaces
  char '='
  spaces
  n <- value
  return $ Assignment (s i) n

comment = do
  char '#'
  anyChar `manyTill` try endOfLine
  return Comment
