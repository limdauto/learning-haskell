{-# Language NamedFieldPuns #-}
import Control.Monad
import Text.ParserCombinators.Parsec

data Command = ChangeValue Int
             | Visit String Int
             | Insert String Int
             | Delete
             | Print
    deriving Show

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)
data Crumb a = LeftCrumb a (Tree a)
             | RightCrumb a (Tree a)
             deriving (Show)
type Breadcrumbs a = [Crumb a]
type Zipper a = (Tree a, Breadcrumbs a)

root :: Zipper Int
root = (Node 0 Empty Empty, [])

main :: IO ()
main = do
    n <- liftM read getLine
    commands <- mapM (\_ -> liftM readCommand getLine) [1..n]
    foldM_ go root commands

go :: Zipper Int -> Command -> IO (Zipper Int)
go z (ChangeValue x) = return $ changeValue z x
go z (Visit direction n)
    | direction == "left"   = return $ visitLeft z
    | direction == "right"  = return $ visitRight z
    | direction == "parent" = return $ visitParent z
    | direction == "child"  = return $ visitChild z n


visitLeft :: Zipper a -> Zipper a
visitLeft (Node x l r, bs) = (l, LeftCrumb x r:bs)

visitRight :: Zipper a -> Zipper a
visitRight (Node x l r, bs) = (r, RightCrumb x l:bs)

visitParent :: Zipper a -> Zipper a
visitParent (t, LeftCrumb x r:bs) = (Node x t r, bs)
visitParent (t, RightCrumb x l:bs) = (Node x l t, bs)

visitChild :: Zipper a -> Int -> Zipper a
visitChild = undefined

changeValue :: Zipper a -> a -> Zipper a
changeValue (Empty, bs) a = (Empty, bs)
changeValue (Node _ l r, bs) x = (Node x l r, bs)

parseNumber :: Parser Int
parseNumber = read <$> many1 digit

parseDirection :: Parser String
parseDirection = string "left"
                 <|> string "right"
                 <|> string "child"
                 <|> string "parent"

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
