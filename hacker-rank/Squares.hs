import Control.Monad

main :: IO ()
main = do
    nTemp <- getLine
    let n = read nTemp :: Int
    forM_ [1..n] $ \_ -> do
        arrTemp <- getLine
        let arr = map read $ words arrTemp :: [Int]
        putStrLn . show . countSquares $ arr

countSquares :: [Int] -> Int
countSquares (x:y:[]) = maximum [upperbound - lowerbound + 1, 0]
    where lowerbound = ceiling . sqrt . fromIntegral $ x
          upperbound = floor . sqrt . fromIntegral $ y
