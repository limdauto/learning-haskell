module FreeMonad where

-- http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html
-- http://dlaing.org/cofun/posts/free_and_cofree.html
-- http://arxiv.org/pdf/1403.0749v3.pdf
data Free f a = Pure a
              | Free (f (Free f a))

instance Functor f => Functor (Free f) where
    fmap f (Pure x) = Pure (f x)
    fmap f (Free x) = Free (fmap (fmap f) x)

instance Functor f => Applicative (Free f) where
    pure x = Pure x
    g <*> x = fmap₂ ($) g x
        where fmap₂ h z y = (fmap h z) <*> y

instance Functor f => Monad (Free f) where
    return x = Pure x
    (Pure r) >>= f = f r
    (Free x) >>= f = Free (fmap (>>= f) x)

-- Application 1: option parsers
data User = User {
    username :: String,
    fullname :: String,
    id :: Int
} deriving Show

data Option a = Option {
    optName :: String,
    optDefault :: Maybe a,
    optReader :: String -> Maybe a
} deriving Functor

readInt :: String -> Int
readInt x = read x :: Int

userP :: Free Option User
userP = User <$> one (Option "username" Nothing Just)
             <*> one (Option "fullname" (Just "") Just)
             <*> one (Option "id" Nothing readInt)

one :: Option a -> Free Option a
one opt = Pure opt
