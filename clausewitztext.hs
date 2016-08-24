{-|
Module      : Borders
Description : Parses Clausewitz Engine Text Files (CETF)
Copyright   : (c) Matthew Bauer, 2016
Maintainer  : mjbauer95@gmail.com
Stability   : experimental
This module is used for parsing text file assets from the
Clausewitz game engine used by Paradox Interactive. The method
for parsing has been reverse-engineered using game assets provided
by them Paradox.
-}
module ClausewitzText where

import Text.Parsec
import Text.Parsec.Text
import Data.Text
import Data.Functor.Identity (Identity)

-- |'Value' stores a parsed piece of data
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

-- |The 'parser' is derived from the value above.
parser :: Parser [Value]
parser = many $ try value

-- |The 'decode' function gives a parsed Value from Text.
decode :: SourceName -> Text -> Either ParseError [Value]
decode = parse parser

-- |'value' tries different value pieces, ignoring spaces.
value :: ParsecT Text u Identity Value
value = do
  v <- try list <|> try string' <|> try bool <|> try assignment <|>
       try date <|> try float <|> try comment <|> try identifier
  spaces
  return v

-- |'data' matches a date value.
date :: ParsecT Text u Identity Value
date = do
  a <- digit `manyTill` try (char '.')
  b <- digit `manyTill` try (char '.')
  c <- many digit
  return $ Date (read a :: Integer) (read b :: Integer) (read c :: Integer)

-- |'identifier' matches an identifier value.
identifier :: ParsecT Text u Identity Value
identifier = do
  a <- many1 $ alphaNum <|> char '_' <|> char '.'
  return $ Identifier a

-- |'float' matches a float value of digits and decimals.
float :: ParsecT Text u Identity Value
float = do
  n <- many1 $ digit <|> char '.' <|> char '-'
  return $ Float (read n :: Float)

-- |'bool' matches a bool value (yes or no).
bool :: ParsecT Text u Identity Value
bool = fmap Bool $ (try (string "yes") >> return True) <|>
  (try (string "no") >> return False)

-- |'string' matches a string value enclosed in "".
string' :: ParsecT Text u Identity Value
string' = do
  _ <- char '"'
  s' <- anyChar `manyTill` try (char '"')
  return $ String s'

-- |'list' matches a list of values.
list :: ParsecT Text u Identity Value
list = do
  _ <- char '{'
  spaces
  r <- many $ try value
  spaces
  _ <- char '}'
  return $ List r

-- |'assignment' matches an assignment value of the form a=b.
assignment :: ParsecT Text u Identity Value
assignment = do
  i <- identifier
  spaces
  _ <- char '='
  spaces
  n <- value
  return $ Assignment (s i) n

-- |'comment' matches a comment value designated by "#".
comment :: ParsecT Text u Identity Value
comment = do
  _ <- char '#'
  _ <- anyChar `manyTill` try endOfLine
  return Comment
