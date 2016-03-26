import Text.ParserCombinators.Parsec
import Control.Monad

data Time = Time Int Int Int String

show2d :: Int -> String
show2d n = if n < 10 then "0" ++ show n else show n

instance Show Time where
    show (Time 12 mm ss "PM") = "12:" ++ show2d mm ++ ":" ++ show2d ss
    show (Time 12 mm ss _)    = "00:" ++ show2d mm ++ ":" ++ show2d ss
    show (Time hh mm ss "PM") = show2d (hh + 12) ++ ":" ++ show2d mm ++ ":" ++ show2d ss
    show (Time hh mm ss _)    = show2d hh ++ ":" ++ show2d mm ++ ":" ++ show2d ss

main :: IO ()
main = do
    timeString <- getLine
    print $ either (const (Time 0 0 0 "")) id
                   (parse timeParser "time" timeString)

digits :: Parser Int
digits = liftM read $ many1 digit

separator :: Parser Char
separator = oneOf ":/-"

period :: Parser String
period = choice [string "AM", string "PM"]

timeParser :: Parser Time
timeParser = Time <$> (digits <* separator) <*> (digits <* separator) <*> digits <*> period
