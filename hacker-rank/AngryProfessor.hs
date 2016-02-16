import Control.Monad

main :: IO ()
main = do
    numTests <- getLine
    let t = read numTests :: Int
    forM_ [1..t] $ \_  -> do
        nk <- getLine
        let [n, k] = map read $ words nk :: [Int]
        arrivals <- getLine
        let a = map read $ words arrivals :: [Int]
        let numLate = length $ filter (> 0) a
        putStrLn $ if numLate > (n - k) then "YES" else "NO"
