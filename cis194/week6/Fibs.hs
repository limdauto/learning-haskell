module Fibs where

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs (tail fibs)