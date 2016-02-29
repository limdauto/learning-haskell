import Control.Monad
import Data.List

main = readLn >>= \t ->
    forM_ [1..t] $ \_ ->
    readLn >>= putStrLn . show . solveFor

-- closed form solution
solveFor n = if n `mod` 2 == 0 then 2^(numYears + 1) - 1 else 2^(numYears + 2) - 2
    where numYears = n `div` 2

-- an alternative golf version
solveFor' n = foldl' (flip ($)) 1 $ take n funcs
    where funcs = concat $ repeat [(* 2), (+ 1)]
