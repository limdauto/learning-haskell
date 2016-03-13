module ReaderMonad where

newtype Reader e a = Reader { runReader :: e -> a }

instance Functor (Reader e) where
    fmap f (Reader r) = Reader $ \e -> f (r e)

instance Applicative (Reader e) where
    pure a                    = Reader $ const a
    (Reader f) <*> (Reader r) = Reader $ \e -> f e $ r e

instance Monad (Reader e) where
    return a         = Reader $ const a
    (Reader r) >>= f = Reader $ \e -> runReader (f (r e)) e
