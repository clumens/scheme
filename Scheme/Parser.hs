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
style = Lang.emptyDef { Tok.commentStart    = "#|",
                        Tok.commentEnd      = "|#",
                        Tok.commentLine     = ";",
                        Tok.nestedComments  = True,
                        Tok.identStart      = letter    <|> oneOf "_-+/*=|&<>",
                        Tok.identLetter     = alphaNum  <|> oneOf "_-+/=|&<>?",
                        Tok.opStart         = Tok.opLetter style,
                        Tok.opLetter        = oneOf ":!#$%%&*+./<=>?@\\^|-~",
                        Tok.reservedNames   = ["Nil", "#t", "#f", "else", ".", "-", "+"],
                        Tok.reservedOpNames = [],
                        Tok.caseSensitive   = True }

reservedName :: T.Text -> Parser ()
reservedName name = Tok.reserved lexer $ T.unpack name

parseChar :: Parser LispVal
parseChar = do
    -- FIXME:  This needs to support the following:
    --     #\<char name>
    --     #\x<hex value>
    void $ string "#\\"
    c <- anyChar
    return $ Character c

parseAtom :: Parser LispVal
parseAtom = Atom . T.pack <$> Tok.identifier lexer

parseText :: Parser LispVal
parseText = String . T.pack <$> Tok.stringLiteral lexer

parseNumber :: Parser LispVal
parseNumber = Number <$> Tok.integer lexer

parseFloat :: Parser LispVal
parseFloat = Float <$> Tok.float lexer

parseList :: Parser LispVal
parseList = List . concat <$> many parseExpr `sepBy` (char ' ' <|> char '\n')

parseSExp :: Parser LispVal
parseSExp = do
    sexp <- Tok.parens lexer (many parseExpr `sepBy` (char ' ' <|> char '\n'))
    return $ List $ concat sexp

parseQuote :: Parser LispVal
parseQuote = do
    void $ char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr =  parseReserved
         <|> try parseFloat
         <|> try parseNumber
         <|> parseChar
         <|> parseAtom
         <|> parseText
         <|> parseQuote
         <|> parseSExp

parseReserved :: Parser LispVal
parseReserved = (reservedName "Nil" >> return Nil)
            <|> (reservedName "#t" >> return (Bool True))
            <|> (reservedName "#f" >> return (Bool False))
            <|> (reservedName "else" >> return (Bool True))
            <|> (reservedName "." >> return (Atom "."))
            <|> (reservedName "-" >> return (Atom "-"))
            <|> (reservedName "+" >> return (Atom "+"))

contents :: Parser a -> Parser a
contents p = do
    Tok.whiteSpace lexer
    p <* eof

readExpr :: T.Text -> Either ParseError LispVal
readExpr = parse (contents parseExpr) "<stdin>"

readExprFile :: T.Text -> Either ParseError LispVal
readExprFile = parse (contents parseList) "<file>"
