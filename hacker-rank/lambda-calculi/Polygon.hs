import Control.Monad
import Data.List (sortBy, foldl')
import Text.Printf

data Point = Point Int Int
    deriving Show
data Vector = Vector Int Int
    deriving Show

instance Eq Point where
    (Point x1 y1) == (Point x2 y2) = x1 == x2 && y1 == y2

instance Ord Point where
    (Point x1 y1) <= (Point x2 y2)
        | y1 == y2 && x1 == x2 = True
        | y1 == y2 = x1 <= x2
        | otherwise = y1 <= y2

-- area
-- main :: IO ()
-- main = do
--     numPoints <- liftM read getLine
--     firstPoint <- liftM (parsePoint . parseInts) getLine
--     (result', lastPoint) <- foldM area (0.0, firstPoint) [2..numPoints]
--     let result = abs ((result' + shoelace lastPoint firstPoint) / 2)
--     putStrLn $ printf "%.1f" result

-- check if polygon is concave
main :: IO ()
main = do
    n  <- liftM read getLine
    tmp <- forM [1..n] getPoint
    let p1 = findMinPoint tmp
    let rest = filter (/= p1) tmp
    let xs@(p2:p3:tails) = sortBy (sortPointByAngle p1) rest
    let d = direction p1 p2 p3
    let points = tails ++ [p1, p2]
    let (tmpResult, d', _, _) = foldl' isConcave (False, d, p2, p3) points
    let result = if d' /= 0 then tmpResult else True
    printResult result

isConcave :: (Bool, Int, Point, Point) -> Point -> (Bool, Int, Point, Point)
isConcave (result, d, p1, p2) p3 =
    if result
        then (True, d, p2, p3)
        else (concaveCheck d p1 p2 p3, d, p2, p3)

concaveCheck :: Int -> Point -> Point -> Point -> Bool
concaveCheck d p1 p2 p3 = let d' = direction p1 p2 p3 in d' * d < 0 || d' /= d

findMinPoint :: [Point] -> Point
findMinPoint (x:xs) = foldl' cmp x xs
    where cmp min p = if p < min then p else min

sortPointByAngle :: Point -> (Point -> Point -> Ordering)
sortPointByAngle p = sort
    where sort p1 p2
            | cosAngleOnX p p1 < cosAngleOnX p p2 = LT
            | cosAngleOnX p p1 > cosAngleOnX p p2 = GT
            | otherwise = EQ

printResult :: Bool -> IO ()
printResult True = putStrLn "YES"
printResult False = putStrLn "NO"

vector :: Point -> Point -> Vector
vector (Point x1 y1) (Point x2 y2) = Vector (x2 - x1) (y2 - y1)

vlen :: Vector -> Float
vlen (Vector u v) = sqrt . fromIntegral $ u^2 + v^2

sinAngleOnX :: Point -> Point -> Float
sinAngleOnX p1@(Point x1 y1) p2@(Point x2 y2) = let l = vlen vx in
    if l == 0 then 1 else fromIntegral (zcross' vx u) / (vlen u * vlen vx)
    where u = vector p1 p2
          x = if x2 < x1 then 2 * x1 - x2 else x2
          vx = vector p1 (Point x y1)

cosAngleOnX :: Point -> Point -> Float
cosAngleOnX p1@(Point x1 y1) p2@(Point x2 y2) = let l = vlen vx in
    if l == 0 then 1 else fromIntegral (dot vx u) / (vlen u * l)
    where u = vector p1 p2
          x = if x2 < x1 then 2 * x1 - x2 else x2
          vx = vector p1 (Point x y1)

dot :: Vector -> Vector -> Int
dot (Vector x1 y1) (Vector x2 y2) = x1 * x2 + y1 * y2

zcross' :: Vector -> Vector -> Int
zcross' (Vector x1 y1) (Vector x2 y2) = x1 * y2 - y1 * x2

zcross :: Point -> Point -> Point -> Int
zcross p1 p2 p3 = zcross' (vector p1 p2) (vector p2 p3)

direction :: Point -> Point -> Point -> Int
direction p1 p2 p3 = sign (zcross p1 p2 p3)

sign :: Int -> Int
sign x | x == 0     = 0
       | x < 0      = -1
       | otherwise  = 1

distance :: Point -> Point -> Float
distance (Point x1 y1) (Point x2 y2) =
    sqrt . fromIntegral $ (x1 - x2)^2 + (y1 - y2)^2

shoelace :: Point -> Point -> Float
shoelace (Point x1 y1) (Point x2 y2) = fromIntegral $ x1*y2 - x2*y1

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

getPoint :: Int -> IO Point
getPoint _ = liftM (parsePoint . parseInts) getLine

parsePoint :: [Int] -> Point
parsePoint (x:y:_) = Point x y

parseInts :: String -> [Int]
parseInts = (map read) . words
