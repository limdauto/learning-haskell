{-# LANGUAGE NamedFieldPuns #-}
import Control.Monad
import Data.List


data Array2D = Array2D [[Int]]

data Size = Size {
    rows :: Int,
    cols :: Int
}

data Position = Position {
    x :: Int,
    y :: Int
}

data BoundingRect = BoundingRect {
    topLeft :: Position,
    topRight :: Position,
    bottomLeft :: Position,
    bottomRight :: Position
}

data Matrix = Matrix {
    size :: Size,
    nums :: Array2D
}

instance Show Matrix where
    show m = "Rows: " ++ (show . rows . size $ m) ++ " | Cols: " ++ (show . cols . size $ m) ++ "\n" ++ (show . nums $ m)

instance Show Array2D where
    show (Array2D arr) = intercalate "\n" . map (intercalate " " . map show) $ arr

instance Show Position where
    show pos = "(" ++ (intercalate ", " . map show $ [x pos, y pos]) ++ ")"

instance Show BoundingRect where
    show box = "Top Left: " ++ (show . topLeft $ box) ++ " | Top Right: " ++ (show . topRight $ box) ++
               " | Bottom Left: " ++ (show . bottomLeft $ box) ++ " | Bottom Right: " ++ (show . bottomRight $ box) ++ "\n"

instance Eq Position where
    (Position x1 y1) == (Position x2 y2) = x1 == x2 && y1 == y2

main :: IO ()
main = do
    nTemp <- getLine
    let (m:n:r:_) = map read $ words nTemp :: [Int]
    arr <- foldM readLines (Array2D []) [1..m]
    let matrix = Matrix (Size m n) arr
    let results = rotate matrix r
    putStrLn $ intercalate "\n" [intercalate " " [showJust $ lookup (Position x y) results |
                                 x <- [1..n]] | y <- [1..m]]

showJust :: Maybe Int -> String
showJust (Just x) = show x

readLines :: Array2D -> t -> IO Array2D
readLines (Array2D arr) _ = do
    arrTemp <- getLine
    return $ Array2D (arr ++ [(map read $ words arrTemp :: [Int])])


elemAt :: Matrix -> Position -> Int
elemAt (Matrix _ (Array2D nums)) (Position x y) = nums !! (y - 1) !! (x - 1)


onLeftEdge box pos = (x . topLeft $ box) == (x pos)
onRightEdge box pos = (x . topRight $ box) == (x pos)
onTopEdge box pos = (y . topLeft $ box) == (y pos)


backtrackOne :: BoundingRect -> Position -> Position
backtrackOne box@(BoundingRect {topLeft, topRight, bottomLeft, bottomRight}) pos@(Position {x, y})
    | pos == topLeft        = Position (x + 1) y
    | pos == topRight       = Position x (y + 1)
    | pos == bottomLeft     = Position x (y - 1)
    | pos == bottomRight    = Position (x - 1) y
    | onLeftEdge box pos    = Position x (y - 1)
    | onRightEdge box pos   = Position x (y + 1)
    | onTopEdge box pos     = Position (x + 1) y
    | otherwise             = Position (x - 1) y


backtrack :: Matrix -> BoundingRect -> Int -> Position -> (Position, Int)
backtrack matrix box numRotations pos = let origin = foldl' (\p _ -> backtrackOne box p) pos [1..numRotations] in (pos, elemAt matrix origin)


boundingBoxes :: Size -> [BoundingRect]
boundingBoxes (Size rows cols) = [BoundingRect (topLeft d) (topRight d) (bottomLeft d) (bottomRight d) | d <- [0..numBoxes - 1]]
    where numBoxes          = minimum [rows, cols] `div` 2
          topLeft depth     = Position (1 + depth) (1 + depth)
          topRight depth    = Position (cols - depth) (1 + depth)
          bottomLeft depth  = Position (1 + depth) (rows - depth)
          bottomRight depth = Position (cols - depth) (rows - depth)


walkOnBoundingBox :: BoundingRect -> [Position]
walkOnBoundingBox box = nub $ walkOnLeftEdge ++ walkOnBottomEdge ++ walkOnRightEdge ++ walkOnTopEdge
    where walkOnLeftEdge = zipWith Position (repeat . x . topLeft $ box) [(y . topLeft $ box)..(y . bottomLeft $ box)]
          walkOnRightEdge = zipWith Position (repeat . x . topRight $ box) [(y . topRight $ box)..(y . bottomRight $ box)]
          walkOnTopEdge = zipWith Position [(x . topLeft $ box)..(x . topRight $ box)] (repeat . y . topLeft $ box)
          walkOnBottomEdge = zipWith Position [(x . bottomLeft $ box)..(x . bottomRight $ box)] (repeat . y . bottomLeft $ box)


rotate :: Matrix -> Int -> [(Position, Int)]
rotate matrix numRotations =  concat $ map backtrackOneBox boxes
    where boxes = boundingBoxes . size $ matrix
          backtrackOneBox box = map (backtrack matrix box numRotations) (walkOnBoundingBox box)

