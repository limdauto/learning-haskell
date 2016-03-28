import Control.Monad

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

    -- parse the swap operations
    t <- liftM read getLine
    ops <- forM [1..t] $ \_ -> liftM read getLine

    -- swap
    foldM_ swap tree ops

buildTree :: [Pair] -> Tree
buildTree pairs = let index = 1 in build root pairs index

build :: Tree -> [Pair] -> Int -> Tree
build tree [] _     = tree
build tree (p:xs) i = build (insert p i tree) xs (i + 1)

insert :: Pair -> Int -> Tree -> Tree
insert = undefined

node :: Int -> Tree
node -1 = Leaf
node v  = Node Leaf v Leaf

swap :: Tree -> Int -> IO Tree
swap = undefined

parseInts :: String -> [Int]
parseInts = (map read) . words
