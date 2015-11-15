{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit x = x `mod` 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit x
    | x < 10 	= 0
    | otherwise = x `div` 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits x
    | x <= 0 	= []
    | otherwise = lastDigit x : toRevDigits (dropLastDigit x)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = zipWith (\x y -> if x `mod` 2 == 0 then y * 2 else y) [1 .. length xs] xs

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (sum (toRevDigits x)) + (sumDigits xs)

-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn x = ((sumDigits . doubleEveryOther . toRevDigits $ x) `mod` 10) == 0