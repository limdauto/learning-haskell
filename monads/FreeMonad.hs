module FreeMonad where

-- http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html
-- http://dlaing.org/cofun/posts/free_and_cofree.html
-- http://arxiv.org/pdf/1403.0749v3.pdf
import Control.Monad.Fix ()

data Toy b next = Output b next
                | Bell next
                | Done
data FixE f e = Fix (f (FixE f e)) | Throw e

catch :: (Functor f) => FixE f e1 -> (e1 -> FixE f e2) -> FixE f e2
catch (Fix x) f = Fix (fmap (`catch` f) x)
catch (Throw e) f = f e

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
