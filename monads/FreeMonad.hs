{-# LANGUAGE DeriveFunctor #-}

module FreeMonad where

{-
    http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html
    http://dlaing.org/cofun/posts/free_and_cofree.html
    http://arxiv.org/pdf/1403.0749v3.pdf
    http://okmij.org/ftp/Haskell/extensible/more.pdf
    http://apfelmus.nfshost.com/articles/operational-monad.html
-}
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

readInt :: String -> Maybe Int
readInt x = case reads x of
            [(x', "")] -> Just x'
            _         -> Nothing

userP :: Free Option User
userP = User <$> one (Option "username" Nothing Just)
             <*> one (Option "fullname" (Just "") Just)
             <*> one (Option "id" Nothing readInt)

one :: Option a -> Free Option a
one = undefined

-- Application 2: Toy language
data Toy b next = Output b next
                | Bell next
                | Done
    deriving Functor

{-
 There is only one way to derive a Functor instance for Toy, see
 http://programmers.stackexchange.com/questions/242795/what-is-the-free-monad-interpreter-pattern
 but here is the definition

 instance Functor (Toy b) where
    fmap f (Output x next) = Output x (f next)
    fmap f (Bell     next) = Bell     (f next)
    fmap f  Done           = Done
 -}
liftF :: (Functor f) => f r -> Free f r
liftF command = Free (fmap Pure command)

output x = liftF (Output x ())
bell     = liftF (Bell     ())
done     = liftF  Done

subroutine :: Free (Toy Char) ()
subroutine = output 'A'

program :: Free (Toy Char) r
program = do
    subroutine
    bell
    done

showProgram :: (Show a, Show r) => Free (Toy a) r -> String
showProgram (Free (Output a x)) =
    "output " ++ show a ++ "\n" ++ showProgram x
showProgram (Free (Bell x)) =
    "bell\n" ++ showProgram x
showProgram (Free Done) =
    "done\n"
showProgram (Pure r) =
    "return " ++ show r ++ "\n"
