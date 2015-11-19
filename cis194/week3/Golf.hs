module Golf where

import Data.List (intercalate, tails)
import Data.Maybe (isJust)

-- skip a list of lists
-- my idea is to generate a bunch of mask boolean lists and use it to filter the original

skips :: [a] -> [[a]]
skips xs = let idx = [1..length xs] in [map fst $ filter snd $ zip xs m | m <- [[n `mod` i == 0 | n <- idx] | i <- idx]]

-- local maxima
-- use tails from Data.List to generate a sliding window

localMaxima :: [Integer] -> [Integer]
localMaxima xs = map (\(Just i) -> i) $ filter isJust $ map (maxima . take 3) $ tails xs
    where maxima :: [Integer] -> Maybe Integer
          maxima [x, y, z] = if x < y && y > z then Just y else Nothing
          maxima _ = Nothing

-- histogram
-- keep an occurence list and compare a number's occurence with line number to see if we should print star
-- for that number and that line

histogram :: [Int] -> String
histogram xs = intercalate "\n" (stars ++ ["==========", "0123456789", ""])
    where stars = reverse [intercalate "" (map (char i) counts) | i <- [1..maximum counts]]
          counts = map (\x -> length $ filter (== x) xs) [0..9]
          char i n = if n >= i then "*" else " "
