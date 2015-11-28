module JoinList where

import Sized
import Scrabble

import Data.Monoid

{-
let ls = Append (Size 4)
            (Append (Size 3)
                (Single (Size 1) 'y')
                (Append (Size 2)
                    (Single (Size 1) 'e')
                    (Single (Size 1) 'a')))
            (Single (Size 1) 'h')
-}
data JoinList m a = Empty
                    | Single m a
                    | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

-- gets the annotation at the root of a JoinList.
tag :: Monoid m => JoinList m a -> m
tag (Single m _) = m
tag (Append m _ _) = m
tag Empty = mempty 

-- append 2 join lists
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append (tag x <> tag y) x y

-- get size of a branch in the join list
sizeOfBranch :: (Sized b, Monoid b) => JoinList b a -> Int
sizeOfBranch = getSize . size . tag

-- index a joined list
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i _ | i < 0 = Nothing
indexJ i (Single _ a) = if i > 0 then Nothing else Just a
indexJ i (Append _ left right) = if i < sizeLeft then indexJ i left else indexJ (i - sizeLeft) right
    where sizeLeft = sizeOfBranch left

-- drop first n elements in the list
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ i ls
    | i == 0 || i < 0  = ls
dropJ _ Empty = Empty
dropJ _ (Single _ _) = Empty
dropJ i (Append m left right)
    | i < sizeLeft = Append m (dropJ i left) right
    | otherwise    = Append m Empty (dropJ (i - sizeLeft) right)
    where sizeLeft = sizeOfBranch left

-- take first n elements in the list
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ i _
    | i == 0 || i < 0 = Empty
takeJ _ Empty = Empty
takeJ _ ls@(Single _ _) = ls
takeJ i (Append m left right)
    | i < sizeLeft = Append m (takeJ i left) Empty
    | otherwise    = Append m left (takeJ (i - sizeLeft) right)
    where sizeLeft = sizeOfBranch left

-- scrablle scoring
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-- safe indexing simply list
(!!?) :: [a] -> Int -> Maybe a
[] !!? _        = Nothing
_ !!? i | i < 0 = Nothing
(x:_) !!? 0    = Just x
(_:xs) !!? i    = xs !!? (i-1)

-- join list to list
jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2