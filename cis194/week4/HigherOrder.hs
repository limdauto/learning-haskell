module HigherOrder where

import Data.List ((\\))

-- fun stuff
fun1' :: [Integer] -> Integer
fun1' = product . (map (\x -> if even x then x - 2 else 1))

fun2' :: Integer -> Integer
fun2' = sum . (takeWhile (> 0)) . (iterate generate)
    where generate 1 = 0
          generate x = if even x then x `div` 2 else 3 * x + 1

-- fold tree
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
    where insert x Leaf = Node 0 Leaf x Leaf
          insert x (Node 0 Leaf node Leaf) = Node 1 (Node 0 Leaf x Leaf) node Leaf
          insert x (Node 1 left node Leaf) = Node 1 left node (Node 0 Leaf x Leaf)
          insert x (Node height left@(Node leftHeight _ _ _) node right@(Node rightHeight _ _ _))
              = case rightHeight - leftHeight of
                  1  -> Node rightHeight (insert x left) node right
                  0  -> Node (height + 1) left node (insert x right)
                  -1 -> Node leftHeight left node (insert x right)

-- fold xor
xor :: [Bool] -> Bool
xor = odd . foldr (\x numTrue -> if x then numTrue + 1 else numTrue ) 0

-- reinventing map
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> [f x] ++ y) []

-- foldl using foldr
-- http://stackoverflow.com/questions/6172004/writing-foldl-using-foldr
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\b g x -> g (f x b)) id xs base

-- sieve of Sundaram
-- https://en.wikipedia.org/wiki/Sieve_of_Sundaram
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x+1) $ [1..n] \\ [m | j <- [1.. n `div` 2], i <- [1..j], let m = i+j+2*i*j, m <= n]
