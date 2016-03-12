import Control.Monad
import Data.Graph as G
import Data.IORef as Ref


main = do
    results <- newIORef [] :: IO (IORef [(Int, Int)])
    numTests <- liftM read getLine
    forM_ [1..numTests] $ \_ -> do
        (n:m:_) <- liftM parseInts getLine
        let bounds = (1, n) :: G.Bounds

        -- edges
        e <- foldM readEdge [] [1..m]

        -- start node
        s <- liftM read getLine :: IO Int

        -- graph
        let g = G.buildG bounds e

        -- find shortest path from s to each node
        print g

    where readEdge = \e _ -> do
            (n1:n2:_) <- liftM parseInts getLine
            return $ e ++ [(n1, n2)]

parseInts :: String -> [Int]
parseInts = (map read) . words
