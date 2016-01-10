{-# LANGUAGE TemplateHaskell #-}

import TemplateHaskell

data MyData = MyData
    { foo :: String
    , bar :: Int
    }

listFields ''MyData

main = print $ MyData { foo = "what", bar = 1 }
