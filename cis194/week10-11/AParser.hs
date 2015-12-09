{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------
first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Functor Parser where
  fmap f p = Parser h
    where h s = fmap (first f) $ (runParser p) s

-- http://stackoverflow.com/questions/25587787/better-applicative-instance-for-parser-haskell
instance Applicative Parser where
  pure a      = Parser (\s -> Just(a, s))
  p1 <*> p2   = Parser (combine p1 p2)
    where combine p1 p2 s = case runParser p1 s of
                              Nothing -> Nothing
                              Just (f, s1) -> case runParser p2 s1 of
                                                Nothing -> Nothing
                                                Just (x, s2) -> Just (f x, s2) 

compose f = fmap (first f)

abParser :: Parser (Char, Char)
abParser = charA <*> charB
  where step1 = runParser (char 'a') 
        charA = Parser (compose (\x y -> (x, y)) . step1)
        charB = char 'b'

abParser_ :: Parser ()
abParser_ = charA <*> charB
  where step1 = runParser (char 'a') 
        charA = Parser (compose (\x y -> ()) . step1)
        charB = char 'b'

spacedInt :: String -> String
spacedInt s = case runParser (char ' ') s of
                Nothing -> ""
                Just (_, s1) -> s1

intPair :: Parser [Integer]
intPair = int1 <*> int2
  where step1 = runParser posInt
        int1 = Parser (compose (\x y -> [x, y]) . step1)
        int2 = Parser (step1 . spacedInt)