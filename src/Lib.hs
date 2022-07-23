{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lib where

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Map                      as Map
import qualified Data.Text                     as T

import           Data.Char                     (digitToInt)
import           Numeric
import           Text.Parsec                   (Parsec)
import           Text.ParserCombinators.Parsec hiding (spaces)

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | Float Float
  | String String
  | Char Char
  | Bool Bool
  deriving (Eq, Show)

run :: IO ()
run = putStrLn "main running..."

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"" <|> escapeChar)
  char '"'
  return $ String x

escapeChar :: Parser Char
escapeChar = do
  char '\\'
  e <- oneOf "\"nt\\"
  return $
    case e of
      '"'  -> '\"'
      '\\' -> '\\'
      'n'  -> '\n'
      't'  -> '\t'

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  return $ case first:rest of
    "#t" -> Bool True
    "#f" -> Bool False
    atom -> Atom atom

-- parseChar :: Parser LispVal
-- parseChar = do
--   string "#\\"
--   c <- (specialized <$> string "") <|> letter <|> digit <|> symbol <|> (specialized <$> (string "space" <|> string "newline"))
--   return . Char $ c
--   where
--     specialized "space"  = ' '
--     specialized ""        = ' '
--     specialized "newline" = '\n'

parseChar :: Parser LispVal
parseChar = do
  string "#\\"
  c <- specializedCharString "space" <|> specializedCharString "newline" <|> letter <|> digit <|> symbol <|> specializedCharString ""
  return . Char $ c
  where
    charString :: String -> Char
    charString s =
      case s of
        "newline" -> '\n'
        "space"   -> ' '
        -- TODO: Maybe should fix this
        ""        -> ' '
    specializedCharString :: String -> Parsec String () Char
    specializedCharString s = do
      ss <- string s
      return . charString $ ss

parseNumber :: Parser LispVal
parseNumber = do
  (Number . read <$> many1 digit) <|> parsePrefixedNumber

parsePrefixedNumber :: Parser LispVal
parsePrefixedNumber = do
  char '#'
  r <- oneOf "bodx"
  ((n, _) : _) <- case r of
    'b' -> readBin <$> many1 (oneOf "01")
    'o' -> readOct <$> many1 (oneOf "01234567")
    'd' -> readDec <$> many1 digit
    'x' -> readHex <$> many1 (digit <|> oneOf "abcdef")
  return $ Number n

readBin :: String -> [(Integer, String)]
readBin s = return ((fromIntegral . T.foldl' (\acc x -> acc * 2 + digitToInt x) 0 . T.pack)  s, "")

parseFloat :: Parser LispVal
parseFloat = do
  n <- many1 digit
  dot <- char '.'
  m <- many1 digit
  let ((f, _): _) = readFloat $ n ++ dot : m
  return . Float $ f


parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

-- | >>> readExpr " %"
-- WAS WAS WAS "Found: '$'"
-- WAS WAS NOW "No match found: \"lisp\" (line 1, column 1):\nunexpected \"a\""
-- WAS NOW "No match found: \"lisp\" (line 1, column 1):\nunexpected \" \""
-- NOW "Found: '%'"
-- readExpr :: String -> String
-- readExpr input = case parse (spaces >> symbol) "lisp" input of
--   Left err -> "No match found: " ++ show err
--   Right v  -> "Found: " ++ show v


readExpr :: String -> String
readExpr input = case parse parseString "lisp" input of
  Left err -> "No match found: " ++ show err
  Right v  -> "Found: " ++ show v
