module Main where

import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

instance Show LispVal where
    show (String contents) = "\"" ++ contents ++ "\""
    show (Atom name) = name
    show (Number contents) = show contents
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (List contents) = "(" ++ unwordsList contents ++ ")"
    show (DottedList x xs) = "(" ++ unwordsList x ++ " . " ++ show xs ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

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

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
                    Left err -> String $ "No match: " ++ show err
                    Right val -> val

-----------------------------------------------------------------------
-- Eval
-----------------------------------------------------------------------
eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

-- https://www.haskell.org/onlinereport/standard-prelude.html#$vmaybe
-- maybe n f Nothing = n
-- maybe n f (Just x) = f x
apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop f xs = Number $ foldr1 f $ map unpacknum xs

-- TODO: improve unpacknum for string
unpacknum :: LispVal -> Integer
unpacknum (Number n) = n
unpacknum (String s)
    | null parsed   = 0
    | otherwise     = fst . head $ parsed
    where parsed = reads s :: [(Integer, String)]
unpacknum (List [n]) = unpacknum n
unpacknum _ = 0

main :: IO ()
main = getArgs >>= (print . eval . readExpr . head)
