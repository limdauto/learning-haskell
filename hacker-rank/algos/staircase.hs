-- every HackerRank warmup
import System.IO

main :: IO ()
main = do
    nTemp <- getLine
    let n = read nTemp :: Int
    putStrLn $ staircase 1 n

staircase :: Int -> Int -> String
staircase currentLine maxLine
    | currentLine == maxLine  = drawLine currentLine maxLine
    | otherwise               = drawLine currentLine maxLine ++ "\n" ++ staircase (currentLine + 1) maxLine

drawLine :: Int -> Int -> String
drawLine currentLine maxLine = concat (replicate (maxLine - currentLine) " " ++
                                       replicate currentLine "#")
