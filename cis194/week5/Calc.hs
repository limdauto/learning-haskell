{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import Control.Monad
import ExprT
import Parser
import StackVM

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = ExprT.Lit
    add = ExprT.Add
    mul = ExprT.Mul

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit x = if x <= 0 then False else True
    add = (||)
    mul = (&&)

instance Expr MinMax where
    lit = MinMax
    add (MinMax x) (MinMax y) = lit (max x y)
    mul (MinMax x) (MinMax y) = lit (min x y)

instance Expr Mod7 where
    lit x = Mod7 (x `mod` 7)
    add (Mod7 x) (Mod7 y) = lit (x + y)
    mul (Mod7 x) (Mod7 y) = lit (x * y)

instance Expr Program where
    lit x = [PushI x]
    add x y = x ++ y ++ [StackVM.Add]
    mul x y = x ++ y ++ [StackVM.Mul]

eval :: ExprT -> Integer
eval (ExprT.Lit x) = x
eval (ExprT.Add x y) = eval x + eval y
eval (ExprT.Mul x y) = eval x * eval y

evalStr :: String -> Maybe Integer
evalStr str = case parseExp ExprT.Lit ExprT.Add ExprT.Mul str of
    Just expr -> Just (eval expr)
    Nothing -> Nothing

compile :: String -> Maybe Program
compile str = parseExp lit add mul str

-- test arithmatic experssions
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

-- test StackVM expressions
-- the following expression should evaluate to Just (Right (IVal (-7)))
testCompile = liftM stackVM (compile "(3 * -4) + 5")
