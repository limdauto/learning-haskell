{-# LANGUAGE GADTs #-}
module OperationalMonad where

-- http://apfelmus.nfshost.com/articles/operational-monad.html

type Stack a = [a]
interpret :: StackProgram -> (Stack Int -> Stack Int)
interpret (Push a : is) stack = interpret is (a : stack)
interpret (Pop    : is) stack = interpret is (tail stack)
interpret []            stack = stack

data StackInstruction a where
    Pop  :: StackInstruction Int
    Push :: Int -> StackInstruction ()

type StackProgram a     = Program StackInstruction a
