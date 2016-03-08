module TrivialMonad where

import Control.Monad
import Control.Applicative

data TrivialMonad x = TrivialMonad x deriving Show

instance Functor TrivialMonad where
  fmap f (TrivialMonad x) = TrivialMonad (f x)

instance Applicative TrivialMonad where
  pure x = TrivialMonad x
  TrivialMonad f <*> TrivialMonad x = TrivialMonad (f x)

instance Monad TrivialMonad where
  return x = TrivialMonad x
  TrivialMonad x >>= f = f x

runTrivial :: TrivialMonad a -> a
runTrivial (TrivialMonad x) = x
