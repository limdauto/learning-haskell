import Control.Monad
import Data.List

main = readLn >>= \t ->
    forM_ [1..t] $ \_ ->
    readLn >>= putStrLn . solveFor

digits = map (`mod` 10) . takeWhile (> 0) . iterate (`div` 10)
solveFor n = show $ foldl' checkMod 0 (digits n)
    where checkMod count 0 = count
          checkMod count x = if n `mod` x == 0 then count + 1 else count
