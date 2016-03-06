import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans.Either
import Data.List (intercalate)

-- step simulation
--main :: IO ()
--main = do
--    temp <- getLine
--    let size = read temp :: Int
--    arrTemp <- getLine
--    let arr = map read $ words arrTemp :: [Int]
--    let (e:reversed) = reverse arr
--    _ <- runEitherT $ forM_ [0..size - 1] $ \i -> do
--        let (first, second) = splitAt i reversed
--        let output = case second of
--                        []  -> first ++ [e] ++ second
--                        (x:_) -> if e < x then first ++ [x] ++ second else first ++ [e] ++ second
--        lift $ putStrLn $ intercalate " " $ map show (reverse output)
--        when (length second == 0 || e > (head second)) $ left ()
--    return ()

-- better solution: https://codepair.hackerrank.com/paper/TkSezB72?b=eyJyb2xlIjoiY2FuZGlkYXRlIiwibmFtZSI6ImxpbWRhdXRvIiwiZW1haWwiOiJsaW1kYXV0b0BnbWFpbC5jb20ifQ%3D%3D
--main :: IO ()
--main = do
--    size <- liftM read getLine
--    (numbers, (v : _)) <- liftM (splitAt (size - 1) . map read . words) getLine
--    mapM_ printList $ ins v (reverse numbers :: [Int])
--
--
--ins :: Ord a => a -> [a] -> [[a]]
--ins v [] = [[v]]
--ins v (x:xs)
--    | x > v = (x:x:xs) : (map ((:) x) $ ins v xs)
--    | otherwise = [(v : x : xs)]
--

main :: IO ()
main = do
    _ <- liftM read getLine :: IO Int
    (x:xs) <- liftM (map read . words) getLine :: IO [Int]
    _ <- runEitherT $ foldM sort ([x], xs) [1..]
    return ()
    where sort (sorted, unsorted) i = do
            -- fucking stupid requirement
            if (i > 1)
                then lift $ printList (sorted ++ unsorted)
                else return ()
            case unsorted of
                [] -> left ()
                (x:xs) -> return (insert x sorted, xs)


ins :: Ord a => [a] -> [a]
ins []          = []
ins (x:[])      = [x]
ins (x:v:vs)    = if x > v then v:(ins (x:vs)) else (x:v:vs)

printList :: Show a => [a] -> IO ()
printList = putStrLn . unwords . map show

-- insertion sort itself
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (v:xs) = if x > v then v : (insert x xs) else (x:v:xs)

insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)

{-
Best solution by far: https://codepair.hackerrank.com/paper/xopASikR?b=eyJyb2xlIjoiY2FuZGlkYXRlIiwibmFtZSI6ImxpbWRhdXRvIiwiZW1haWwiOiJsaW1kYXV0b0BnbWFpbC5jb20ifQ%3D%3D

insertionSortSteps :: Ord a => [a] -> [[a]]
insertionSortSteps [] = [[]]
insertionSortSteps (h:t) = go [h] t []
  where go _ [] trail = trail
        go sorted rest trail = go sorted' rest' (trail ++ [sorted' ++ rest'])
          where sorted' = insertInto (head rest) sorted
                rest' = tail rest
        insertInto v xs = before ++ [v] ++ after
          where (before, after) = span (<v) xs

main :: IO ()
main = do
  inp <- getContents
  let xs = (map read $ words ((lines inp) !! 1) ) :: [Int]
  putStr . unlines . map (unwords . map show) $ insertionSortSteps xs

-}
