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

charA = char 'a'
charB = char 'b'

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
  pure a = Parser (\s -> Just(a, s))
  p1 <*> p2 = Parser (combine p1 p2)
    where combine p1 p2 s = case runParser p1 s of
                              Nothing -> Nothing
                              Just (f, s1) -> case runParser p2 s1 of
                                                Nothing -> Nothing
                                                Just (x, s2) -> Just (f x, s2)

instance Alternative Parser where
  empty = Parser (\_ -> Nothing)
  p1 <|> p2 = Parser (pick p1 p2)
    where pick p1 p2 s = case runParser p1 s of
                            Nothing -> runParser p2 s
                            x -> x

abParser :: Parser (Char, Char)
abParser = liftA2 (,) charA charB

abParser_ :: Parser ()
abParser_ = liftA2 (\_ _ -> ()) charA charB

intPair :: Parser [Integer]
intPair = liftA2 (\x y -> [x, y]) posInt ((satisfy isSpace) *> posInt)

intOrUppercase :: Parser ()
intOrUppercase = let f = pure (const ()) in f <*> posInt <|> f <*> (satisfy isUpper)