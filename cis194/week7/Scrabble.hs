{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Scrabble where

import Data.Monoid
import Data.Char

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)

score :: Char -> Score
score c
    | elem uc ['D', 'G'] = Score 2
    | elem uc ['B', 'C', 'M', 'P'] = Score 3
    | elem uc ['F', 'H', 'Y'] = Score 4
    | elem uc ['K'] = Score 5
    | elem uc ['J', 'X'] = Score 8
    | elem uc ['Q', 'J'] = Score 10
    | elem uc ['A', 'E', 'I', 'L', 'N', 'O', 'R', 'S', 'T', 'U'] = Score 1
    | otherwise = 0
    where uc = toUpper c

scoreString :: String -> Score
scoreString s = foldr (<>) (Score 0) (map score s)