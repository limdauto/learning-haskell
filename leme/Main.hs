module Main where

import System.Environment

import REPL

main :: IO ()
main = do args <- getArgs
          case length args of
               0 -> runRepl
               1 -> runOne (head args)
               otherwise -> putStrLn "Program takes only 0 or 1 argument"
