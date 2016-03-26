import Control.Monad
import Text.Printf


data Point = Point Int Int

distance :: Point -> Point -> Float
distance (Point x1 y1) (Point x2 y2) =
    sqrt . fromIntegral $ (x1 - x2)^2 + (y1 - y2)^2

shoelace :: Point -> Point -> Float
shoelace (Point x1 y1) (Point x2 y2) = fromIntegral $ x1*y2 - x2*y1

main :: IO ()
main = do
    numPoints <- liftM read getLine
    (x1:y1:_) <- liftM parseInts getLine
    let firstPoint = Point x1 y1
    (result', lastPoint) <- foldM area (0.0, firstPoint) [2..numPoints]
    let result = abs ((result' + shoelace lastPoint firstPoint) / 2)
    putStrLn $ printf "%.1f" result

area :: (Float, Point) -> Int -> IO (Float, Point)
area (result, prevPoint) _ = do
    (x:y:_) <- liftM parseInts getLine
    let nextPoint = Point x y
    return $ (result + shoelace prevPoint nextPoint, nextPoint)

perimeter :: (Float, Point) -> Int -> IO (Float, Point)
perimeter (result, prevPoint) _ = do
    (x:y:_) <- liftM parseInts getLine
    let nextPoint = Point x y
    return $ (result + distance prevPoint nextPoint, nextPoint)

parseInts :: String -> [Int]
parseInts = (map read) . words
