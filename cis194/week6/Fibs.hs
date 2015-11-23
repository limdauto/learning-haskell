{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE FlexibleInstances #-}

module Fibs where

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

data Stream a = Element a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Element x s) = x : streamToList s

instance Show a => Show (Stream a) where
    show = show . ((take 20) . streamToList)

streamRepeat :: a -> Stream a
streamRepeat x = Element x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Element x s) = Element (f x) (streamMap f s)

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Element ax as) b = Element ax (interleaveStreams b as)

nats :: Stream Integer
nats = streamify [0..]
    where streamify (x:xs) = Element x (streamify xs)
          streamify [] = undefined

ruler :: Stream Integer
ruler = ruling nats
    where ruling (Element x s) = interleaveStreams (streamRepeat x) (ruling s)

-- in the voice of Chandler Bing, OH. MY. GOD.

x :: Stream Integer
x = Element 0 (Element 1 (streamRepeat 0))

instance Num (Stream Integer) where
    fromInteger n = Element n (streamRepeat 0)
    negate (Element n s) = Element (-n) (negate s)
    (+) (Element n ns) (Element m ms) = Element (n+m) ((+) ns ms)
    (*) (Element n ns) b@(Element m ms) = Element (n*m) ((streamMap (*n) ms) + ns*b)

instance Fractional (Stream Integer) where
    (/) a@(Element n ns) b@(Element m ms) = let q = a / b in Element (n `div` m) (streamMap (`div` m) (ns - q*ms))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^(2 :: Integer))