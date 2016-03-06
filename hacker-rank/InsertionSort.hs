import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans.Either
import Data.List

main' :: IO ()
main' = do
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

-- better solution: https://codepair.hackerrank.com/paper/TkSezB72?b=eyJyb2xlIjoiY2FuZGlkYXRlIiwibmFtZSI6ImxpbWRhdXRvIiwiZW1haWwiOiJsaW1kYXV0b0BnbWFpbC5jb20ifQ%3D%3D
main :: IO ()
main = do
    size <- liftM read getLine
    (numbers, (v : _)) <- liftM (splitAt (size - 1) . map read . words) getLine
    mapM_ printList $ ins v (reverse numbers :: [Int])

ins :: Ord a => a -> [a] -> [[a]]
ins v [] = [[v]]
ins v (x:xs)
    | x > v = (x:x:xs) : (map ((:) x) $ ins v xs)
    | otherwise = [(v : x : xs)]

printList :: Show a => [a] -> IO ()
printList = putStrLn . unwords . map show . reverse
