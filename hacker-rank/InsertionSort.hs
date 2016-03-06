import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans.Either
import Data.List

main :: IO ()
main = do
    temp <- getLine
    let size = read temp :: Int
    arrTemp <- getLine
    let arr = map read $ words arrTemp :: [Int]
    let (e:reversed) = reverse arr
    _ <- runEitherT $ forM_ [0..size - 1] $ \i -> do
        let (first, second) = splitAt i reversed
        let output = case second of
                        []  -> first ++ [e] ++ second
                        (x:_) -> if e < x then first ++ [x] ++ second else first ++ [e] ++ second
        lift $ putStrLn $ intercalate " " $ map show (reverse output)
        when (length second == 0 || e > (head second)) $ left ()
    return ()
