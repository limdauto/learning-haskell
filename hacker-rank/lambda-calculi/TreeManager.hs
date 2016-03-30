{-# Language NamedFieldPuns #-}
import Control.Monad
import Text.ParserCombinators.Parsec

data Command = ChangeValue Int
             | Visit String Int
             | Insert String Int
             | Delete
             | Print
    deriving Show

main :: IO ()
main = undefined

parseNumber :: Parser Int
parseNumber = read <$> many1 digit

parseDirection :: Parser String
parseDirection = string "left" <|> string "right" <|> string "child"

parseChangeValue :: Parser Command
parseChangeValue = ChangeValue <$> (string "change" *> spaces *> parseNumber)

parseVisit :: Parser Command
parseVisit = Visit <$> (string "visit" *> spaces *> parseDirection)
                   <*> (spaces *> (parseNumber <|> pure 0))

parseInsert :: Parser Command
parseInsert = Insert <$> (string "insert" *> spaces *> parseDirection)
                     <*> (spaces *> parseNumber)

parsePrint :: Parser Command
parsePrint = Print <$ string "print"

parseDelete :: Parser Command
parseDelete = Delete <$ string "delete"

parseExpr :: Parser Command
parseExpr = parseChangeValue
            <|> parseVisit
            <|> parseInsert
            <|> parsePrint
            <|> parseDelete

readCommand :: String -> Command
readCommand input = case parse parseExpr "tree" input of
    Left err -> undefined
    Right val -> val
