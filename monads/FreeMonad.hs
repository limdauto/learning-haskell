module FreeMonad where

-- http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html
import Control.Monad.Fix ()

data Toy b next = Output b next
                | Bell next
                | Done
data FixE f e = Fix (f (FixE f e)) | Throw e

catch :: (Functor f) => FixE f e1 -> (e1 -> FixE f e2) -> FixE f e2
catch (Fix x) f = Fix (fmap (`catch` f) x)
catch (Throw e) f = f e

