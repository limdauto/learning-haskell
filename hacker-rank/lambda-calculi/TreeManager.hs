{-# Language NamedFieldPuns #-}
import Control.Monad
import Text.ParserCombinators.Parsec

data Command = ChangeValue Int
             | Visit String Int
             | Insert String Int
             | Delete
             | Print
    deriving Show

data Tree a = Node {
    value :: a,
    children :: [Tree a]
} deriving (Show)

data Crumb a = DownCrumb Int (Tree a) deriving (Show)
type Breadcrumbs a = [Crumb a]
type Zipper a = (Tree a, Breadcrumbs a)

root :: Zipper Int
root = (Node { value = 0, children = [] }, [])

node :: a -> Tree a
node x = Node x []

main :: IO ()
main = do
    n <- liftM read getLine
    commands <- mapM (\_ -> liftM readCommand getLine) [1..n]
    foldM_ go root commands

go :: Zipper Int -> Command -> IO (Zipper Int)
go z (ChangeValue x) = return $ changeValue z x
go z (Visit direction index)
    | direction == "left"   = return $ visitLeft z
    | direction == "right"  = return $ visitRight z
    | direction == "parent" = return $ visitParent z
    | direction == "child"  = return $ visitChild z (index - 1)
go z (Insert direction n)
    | direction == "left"   = return $ insertLeft z n
    | direction == "right"  = return $ insertRight z n
    | direction == "child"  = return $ insertChild z n

visitLeft :: Zipper a -> Zipper a
visitLeft z@(_, DownCrumb index _:_) = let parent = visitParent z in
    visitChild parent (index - 1)

visitRight :: Zipper a -> Zipper a
visitRight z@(_, DownCrumb index _:_) = let parent = visitParent z in
    visitChild parent (index + 1)

visitParent :: Zipper a -> Zipper a
visitParent (_, DownCrumb _ parent:bs) = (parent, bs)

visitChild :: Zipper a -> Int -> Zipper a
visitChild (t@(Node _ children), bs) index =
    (children !! index, DownCrumb index t:bs)

insertLeft :: Zipper Int -> Int -> Zipper Int
insertLeft (_, DownCrumb index parent:bs) x =
    let newParent = (insertChildAtIndex (index - 1) x parent, bs) in
    visitChild newParent index

insertRight :: Zipper Int -> Int -> Zipper Int
insertRight (_, DownCrumb index parent:bs) x =
    let newParent = (insertChildAtIndex (index + 1) x parent, bs) in
    visitChild newParent index

insertChild :: Zipper a -> Int -> Zipper a
insertChild (Node value children, bs) x = undefined

insertChildAtIndex :: Int -> Int -> Tree Int -> Tree Int
insertChildAtIndex index x (Node _ children) = let newNode = node x in
    Node x (insertAt index newNode children)

insertAt :: Int -> a -> [a] -> [a]
insertAt z y xs = as ++ (y:bs)
                  where (as, bs) = splitAt z xs

changeValue :: Zipper a -> a -> Zipper a
changeValue (Node _ children, bs) x = (Node x children, bs)

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
