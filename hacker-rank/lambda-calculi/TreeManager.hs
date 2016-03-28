{-# Language NamedFieldPuns #-}
import Control.Monad
import Text.ParserCombinators.Parsec

data Tree = Tree {
    root :: Int,
    children :: [Tree],
    parent :: Tree,
    left :: Tree,
    right :: Tree
} | Empty deriving Show

data Command = ChangeValue Int
             | Visit String Int
             | Insert String Int
             | Delete
             | Print
    deriving Show

main :: IO ()
main = do
    n <- liftM read getLine
    commands <- mapM (\_ -> liftM readCommand getLine) [1..n]
    foldM_ go r commands
    where go tree command = case eval command tree of
                                Left r -> print r >> return tree
                                Right v -> return v

r :: Tree
r = Tree { root = 0, children = [], parent = Empty, left = Empty, right = Empty}

eval :: Command -> Tree -> Either Int Tree
eval Print tree = Left (root tree)
eval (ChangeValue v) (Tree _ children parent left right) =
    Right $ Tree v children parent left right
eval (Insert direction v) t@(Tree root children parent left right)
    | direction == "left"   =
        let tree = Tree root children parent' left' right
            left' = Tree v [] parent' left tree
            parent' = Tree (root parent) (children parent)
        in Righ $ tree
    | direction == "right"  = undefined
    | otherwise             = undefined
eval (Visit direction v) tree
    | direction == "left"   = undefined
    | direction == "right"  = undefined
    | direction == "parent" = undefined
    | otherwise             = undefined
eval Delete t@(Tree _ _ _ parent) = undefined
eval _ _ = undefined

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
