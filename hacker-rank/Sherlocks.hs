import Control.Monad

main = readLn >>= \t ->
    forM_ [1..t] $ \_ ->
    readLn >>= putStrLn . solveFor

notDivBy3 = (/= 0) . (`mod` 3)
solveFor n = case dropWhile notDivBy3 [n,n-5,n-10] of
                (n5:_) -> replicate n5 '5' ++ replicate (n-n5) '3'
                []     -> "-1"
