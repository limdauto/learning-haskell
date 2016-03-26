import Control.Monad
import Data.List

data Pair = Pair Int Int

nullPair :: Pair
nullPair = Pair (-1) (-1)

instance Eq Pair where
    (Pair x1 y1) == (Pair x2 y2) = x1 == x2 && y1 == y2

instance Ord Pair where
    (Pair x1 y1) <= (Pair x2 y2)
        | x1 < x2   = True
        | x1 == x2  = y1 <= y2
        | otherwise = False

main :: IO ()
main = do
    numTests <- liftM read getLine
    forM_ [1..numTests] $ \_ -> do
        numPairs <- liftM read getLine
        pairs <- foldM parsePairs [] [1..numPairs]
        let sortedPairs = sort pairs
        case foldr checkFunc (True, nullPair) sortedPairs of
            (True, _)   -> putStrLn "YES"
            (False, _)  -> putStrLn "NO"

checkFunc :: Pair -> (Bool, Pair) -> (Bool, Pair)
checkFunc next@(Pair x y) (result, Pair x' y')
    | result == False    = (False, nullPair)
    | x == x' && y /= y' = (False, nullPair)
    | otherwise          = (True, next)

parsePairs :: [Pair] -> Int -> IO [Pair]
parsePairs xs _ = do
    (x:y:_) <- liftM parseInts getLine
    return $ xs ++ [Pair x y]

parseInts :: String -> [Int]
parseInts = (map read) . words
