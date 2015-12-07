module Name where

import Data.Char

main = do
    putStrLn "What is your first name?"
    firstname <- getLine
    putStrLn "What is your last name?"
    lastname <- getLine
    let bigFirstName = map toUpper firstname
        bigLastName = map toUpper lastname
    putStrLn $ "Hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", you rock!"