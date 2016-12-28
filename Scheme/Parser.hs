{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Scheme.Parser(readExpr,
                     readExprFile)
 where

import Scheme.LispVal(LispVal(..))

import           Control.Monad(void)
import           Data.Functor.Identity(Identity)
import           Text.Parsec
import           Text.Parsec.Text(Parser)
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Lang
import qualified Data.Text as T

lexer :: Tok.GenTokenParser T.Text () Identity
lexer = Tok.makeTokenParser style

style :: Tok.GenLanguageDef T.Text () Identity
style = Lang.emptyDef { Tok.commentStart = "{-",
                        Tok.commentEnd = "-}",
                        Tok.commentLine = ";",
                        Tok.opStart = Tok.opLetter style,
                        Tok.opLetter = oneOf ":!#$%%&*+./<=>?@\\^|-~",
                        Tok.identStart = letter <|>  oneOf "-+/*=|&><",
                        Tok.identLetter = alphaNum <|> oneOf "?+<=>|&-/",
                        Tok.reservedOpNames = [ "'", "\""] }

-- pattern binding using record destructing !
Tok.TokenParser { Tok.parens = m_parens,
                  Tok.identifier = m_identifier } = Tok.makeTokenParser style

reservedOp :: T.Text -> Parser ()
reservedOp op = Tok.reservedOp lexer $ T.unpack op

parseAtom :: Parser LispVal
parseAtom = do
  p <- m_identifier
  return $ Atom $ T.pack p

parseText :: Parser LispVal
parseText = do
  reservedOp "\""
  p <- many1 $ noneOf "\""
  reservedOp "\""
  return $ String . T.pack $  p

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseNegNum :: Parser LispVal
parseNegNum = do
  void $ char '-'
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

parseExpr :: Parser LispVal
parseExpr = parseReserved <|> parseNumber
                          <|> try parseNegNum
                          <|> parseAtom
                          <|> parseText
                          <|> parseQuote
                          <|> parseSExp

parseReserved :: Parser LispVal
parseReserved = (reservedOp "Nil" >> return Nil)
            <|> (reservedOp "#t" >> return (Bool True))
            <|> (reservedOp "#f" >> return (Bool False))

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

readExpr :: T.Text -> Either ParseError LispVal
readExpr = parse (contents parseExpr) "<stdin>"

readExprFile :: T.Text -> Either ParseError LispVal
readExprFile = parse (contents parseList) "<file>"
