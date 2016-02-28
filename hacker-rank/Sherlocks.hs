import Control.Monad

main :: IO ()
main = do
    numTests <- getLine
    let t = read numTests :: Int
    forM_ [1..t] $ \_ -> do
        numDigits <- getLine
        let n = read numDigits :: Int
        output n $ dropWhile notDivBy3 [n,n-5..(-1)]
    where notDivBy3 = (/= 0) . (`mod` 3)
          output n (n5:_) = putStrLn $ replicate n5 '5' ++ replicate (n-n5) '3'
          output _ [] = putStrLn "-1"

