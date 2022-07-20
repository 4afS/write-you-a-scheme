{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Parser where

import LispVal
import Text.Parsec
import Text.Parsec.Text
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Token
import qualified Text.Parsec.Language as Language
import qualified Data.Text as T
import Control.Applicative hiding ((<|>))
import Data.Functor.Identity (Identity)

lexer :: Token.GenTokenParser T.Text () Identity
lexer = Token.makeTokenParser style

style :: Token.GenLanguageDef T.Text () Identity
style = Language.emptyDef {
  Token.commentStart = "{-"
  , Token.commentEnd = "-}"
  , Token.commentLine = "--"
  , Token.opStart = Token.opLetter style
  , Token.opLetter = oneOf ":!#$%%&*+./<=>?@\\^|-~"
  , Token.identStart = letter <|>  oneOf "-+/*=|&><"
  , Token.identLetter = digit <|> letter <|> oneOf "?+=|&-/"
  , Token.reservedOpNames = [ "'", "\""]
  }

m_parens :: ParsecT T.Text () Identity a -> ParsecT T.Text () Identity a
m_identifier :: ParsecT T.Text () Identity String
Token.TokenParser { Token.parens = m_parens, Token.identifier = m_identifier } = Token.makeTokenParser style

reservedOp :: T.Text -> Parser ()
reservedOp = Token.reservedOp lexer . T.unpack

parseAtom :: Parser LispVal
parseAtom = Atom . T.pack <$> m_identifier

parseText :: Parser LispVal
parseText = do
  reservedOp "\""
  p <- many1 $ noneOf"\""
  reservedOp "\""
  return $ String . T.pack $ p

parseNumber :: Parser LispVal
parseNumber  = do
  Number . read <$> many1 digit

parseNegNumber :: Parser LispVal
parseNegNumber  = do
  char '-'
  d <- many1 digit
  return $ Number . negate . read $ d

parseList :: Parser LispVal
parseList = List . concat <$> Text.Parsec.many parseExpr `sepBy` (char ' ' <|> char '\n')

parseSExp :: Parser LispVal
parseSExp = List . concat <$> m_parens (Text.Parsec.many parseExpr `sepBy` (char ' ' <|> char '\n'))

parseQuote :: Parser LispVal
parseQuote = do
  reservedOp "\'"
  x <- parseExpr
  return $ List [Atom "quote", x]

parseReserved :: Parser LispVal
parseReserved = do
  reservedOp "Nil" >> return Nil
  <|> (reservedOp "#t" >> return (Bool True))
  <|> (reservedOp "#f" >> return (Bool False))

parseExpr :: Parser LispVal
parseExpr 
  = parseReserved
  <|> parseNumber
  <|> try parseNegNumber
  <|> parseAtom
  <|> parseText
  <|> parseQuote
  <|> parseSExp

