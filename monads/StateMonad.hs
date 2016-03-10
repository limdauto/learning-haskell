module StateMonad where

newtype State s a = State { runState :: s -> (a,s) }

instance Functor (State s) where
    fmap f (State x) = State $ \s -> let (v, s') = x s in (f v, s')

instance Applicative (State s) where
    pure a = State $ \s -> (a, s)
    (State f) <*> (State a) = State $ \s -> let (f', s1) = f s
                                                (a', s') = a s1
                                            in (f' a', s')

instance Monad (State s) where
    return a        = State $ \s -> (a,s)
    (State x) >>= f = State $ \s -> let (v, s') = x s in runState (f v) s'

