import Control.Monad
import Data.List


type Depth = Int
data Array2D = Array2D [[Int]]
data Size = Size Int Int
data Position = Position Int Int
data Matrix = Matrix Size Array2D


instance Show Matrix where
    show (Matrix (Size rows cols) nums) = "Rows: " ++ show rows ++ " | Cols: " ++ show cols ++ "\n" ++ show nums

instance Show Array2D where
    show (Array2D nums) = intercalate "\n" . map (intercalate " " . map show) $ nums


main :: IO ()
main = do
    nTemp <- getLine
    let (m:n:r:_) = map read $ words nTemp :: [Int]
    nums <- foldM readLines (Array2D []) [1..m]
    let matrix = Matrix (Size m n) nums
    print $ rotate matrix r


readLines :: Array2D -> t -> IO Array2D
readLines (Array2D nums) _ = do
    arrTemp <- getLine
    return $ Array2D (nums ++ [(map read $ words arrTemp :: [Int])])


-- backtrack a final position in a block to the current position
backtrack :: Size -> Position -> Int -> Position
backtrack (Size rows cols) (Position finalX finalY) numRotations_ = undefined
    where numRotations = numRotations_ `mod` ((rows + cols) * 2)


rotate :: Matrix -> Int -> Matrix
rotate m@(Matrix size@(Size cols rows) (Array2D nums)) numRotations = Matrix size rotated
    where rotated = Array2D [[elemAt m $ (backtrack size (Position col row) numRotations) | col <- [1..cols]] | row <- [1..rows]]


elemAt :: Matrix -> Position -> Int
elemAt (Matrix _ (Array2D nums)) (Position x y) = nums !! (x - 1) !! (y - 1)
