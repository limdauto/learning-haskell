import Control.Monad

main :: IO ()
main = do
    numTests <- liftM read getLine
    forM_ [1..numTests] $ \_ -> do
        (_:k:_) <- liftM parseInts getLine
        numbers <- liftM parseInts getLine
        putStrLn $ show . sum $ knapsack numbers k

parseInts :: String -> [Int]
parseInts = (map read) . words

knapsack :: [Int] -> Int -> [Int]
knapsack []             _ = []
knapsack numbers@(x:xs) k
    | x > k             = knapsack xs k
    | k `mod` x == 0    = replicate (k `div` x) x
    | otherwise = merge withX withoutX
    where withX = x : (knapsack numbers (k - x))
          withoutX = knapsack xs k
          merge a b = if sum a > sum b then a else b
