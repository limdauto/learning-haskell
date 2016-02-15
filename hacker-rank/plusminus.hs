-- every HackerRank warmup

import System.IO

data Count = Count Int Int Int
data Distribution = Distribution Double Double Double
type Population = Int

instance Show Distribution where
    show (Distribution neg zero pos) = show pos ++ "\n" ++ show neg ++ "\n" ++ show zero

main :: IO ()
main = do
    nTemp <- getLine
    let n = read nTemp :: Int
    arrTemp <- getLine
    let arr = map read $ words arrTemp :: [Int]
    let dist = distribution (process arr (Count 0 0 0)) n
    putStrLn $ show dist

process :: [Int] -> Count -> Count
process [] count = count
process (x:xs) (Count neg zero pos)
    | x == 0    = process xs (Count neg (zero + 1) pos)
    | x < 0     = process xs (Count (neg + 1) zero pos)
    | otherwise = process xs (Count neg zero (pos + 1))

distribution :: Count -> Population -> Distribution
distribution (Count neg zero pos) n = Distribution x y z
    where x = (fromIntegral neg / fromIntegral n) :: Double
          y = (fromIntegral zero / fromIntegral n) :: Double
          z = (fromIntegral pos / fromIntegral n) :: Double

