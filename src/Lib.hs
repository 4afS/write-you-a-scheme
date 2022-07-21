{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Lib where

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Map                      as Map
import           Data.Text                     as T

import           Text.ParserCombinators.Parsec hiding (spaces)
data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool

run :: IO ()
run = putStrLn "main running..."

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  return $ case first:rest of
    "#t" -> Bool True
    "#f" -> Bool False
    atom -> Atom atom

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

-- | >>> readExpr " %"
-- WAS WAS WAS "Found: '$'"
-- WAS WAS NOW "No match found: \"lisp\" (line 1, column 1):\nunexpected \"a\""
-- WAS NOW "No match found: \"lisp\" (line 1, column 1):\nunexpected \" \""
-- NOW "Found: '%'"
readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
  Left err -> "No match found: " ++ show err
  Right v  -> "Found: " ++ show v


