module Parsers where

import Control.Monad
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces)

import LispData
import LispError

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
                rest <- many (letter <|> symbol <|> digit)
                let atom = first:rest
                return $ case atom of
                        "#t" -> Bool True
                        "#f" -> Bool False
                        _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

-- endBy p sep parses zero or more occurrences of p, separated and ended by sep. Returns a list of values returned by p.
-- https://hackage.haskell.org/package/parsec-3.1.9/docs/src/Text-Parsec-Combinator.html#endBy

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

-- http://blog.ezyang.com/2014/05/parsec-try-a-or-b-considered-harmful/
-- http://stackoverflow.com/questions/20020350/parsec-difference-between-try-and-lookahead
-- related:
--   https://hackage.haskell.org/package/parsec-3.1.9/docs/src/Text-Parsec-Combinator.html#option
--   http://stackoverflow.com/questions/20020350/parsec-difference-between-try-and-lookahead
parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
                    Left err -> throwError $ Parser err
                    Right val -> return val
