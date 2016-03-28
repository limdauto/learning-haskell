-- TODO: rewrite with Zipper
import Control.Monad
import Data.List (intercalate)

data Pair = Pair { left :: Int, right :: Int }
data Tree = Leaf
          | Node Tree Int Tree
    deriving Show

root :: Tree
root = Node Leaf 1 Leaf

main :: IO ()
main = do
    n <- liftM read getLine

    -- parse the children pairs
    pairs <- forM [1..n] $ \_ -> do
        (x:y:_) <- liftM parseInts getLine
        return $ Pair x y

    let tree = buildTree pairs
    let h = height tree

    -- parse the swap operations
    t <- liftM read getLine
    ks <- forM [1..t] $ \_ -> liftM read getLine

    -- swap
    foldM_ (\tr k -> (swap tr k h)) tree ks

buildTree :: [Pair] -> Tree
buildTree pairs = let index = 1 in build root pairs index

build :: Tree -> [Pair] -> Int -> Tree
build tree [] _     = tree
build tree (p:xs) i = build (insert p i tree) xs (i + 1)

insert :: Pair -> Int -> Tree -> Tree
insert _ _ Leaf = Leaf
insert p i (Node l v r)
    | i == v    = Node (node $ left p) v (node $ right p)
    | otherwise = Node (insert p i l) v (insert p i r)

node :: Int -> Tree
node v  = if v == -1 then Leaf else Node Leaf v Leaf

height :: Tree -> Int
height t = go t 1
    where go Leaf h = h
          go (Node l _ r) h = maximum [go l (h + 1), go r (h + 1)]

swap :: Tree -> Int -> Int -> IO Tree
swap t k h = do
    swapped <- foldM swapAtLevel t (takeWhile (<= h) [k, 2*k..])
    putStrLn $ printInOrder swapped
    return $ swapped

swapAtLevel :: Tree -> Int -> IO Tree
swapAtLevel Leaf _ = return Leaf
swapAtLevel (Node l v r) 1 = return $ Node r v l
swapAtLevel (Node l v r) k = do
    l' <- swapAtLevel l (k - 1)
    r' <- swapAtLevel r (k - 1)
    return $ Node l' v r'

printInOrder :: Tree -> String
printInOrder Leaf = ""
printInOrder (Node l v r) =
    (intercalate " ") . (filter (/= "")) $ [printInOrder l, show v, printInOrder r]

parseInts :: String -> [Int]
parseInts = (map read) . words
