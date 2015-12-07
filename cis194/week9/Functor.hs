instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap h (Just a) = Just (h a)

instance Functor [] where
    fmap _ [] = []
    fmap f (x:xs) = f x : fmap f xs

instance Functor IO where
    fmap f ioa = ioa >>= (\a -> return (f a))