module WriterMonad where

newtype Writer w a = Writer { runWriter :: (a, w) }

instance (Monoid w) => Functor (Writer w) where
    fmap f (Writer (a, w)) = Writer (f a, w)

instance (Monoid w) => Applicative (Writer w) where
    pure a = Writer (a, mempty)
    (Writer (f, w1)) <*> (Writer (a, w2)) = Writer (f a, w1 `mappend` w2)

instance (Monoid w) => Monad (Writer w) where
    return a = Writer (a, mempty)
    (Writer (a, w)) >>= f = Writer $ let (a', w') = runWriter $ f a in (a', w' `mappend` w)
