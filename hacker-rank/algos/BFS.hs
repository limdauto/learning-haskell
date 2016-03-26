import Control.Monad
import Data.Graph as G
import Data.Maybe as M
import Data.IORef as Ref
import Data.List (intercalate)

type Env = IORef [(Int, Int)]

nullEnv :: Int -> IO Env
nullEnv _ = newIORef []

isBound :: Env -> Int -> IO Bool
isBound envRef var = liftM (M.isJust . lookup var) $ readIORef envRef

getVar :: Env -> Int -> IO Int
getVar envRef var = do
    env <- Ref.readIORef envRef
    return $ maybe (-1) id (lookup var env)

setVar :: Env -> Int -> Int -> IO ()
setVar envRef var val = do
    modifyIORef' envRef (\xs -> xs ++ [(var, val)])

existOrSetVar :: Env -> Int -> Int -> IO Bool
existOrSetVar envRef val var = do
    isBounded <- isBound envRef var
    if isBounded
        then return True
        else setVar envRef var val >> return False

main = do
    numTests <- liftM read getLine

    res <- forM [1..numTests] $ \t -> do
        env <- nullEnv t
        (n:m:_) <- liftM parseInts getLine
        let bounds = (1, n) :: G.Bounds

        -- edges
        e <- foldM readEdge [] [1..m]

        -- start node
        s <- liftM read getLine :: IO Int

        -- graph
        let g = G.buildG bounds e

        -- find shortest path from s to each node
        _ <- traverse' env g 6 s

        res <- forM (filter (/= s) [1..n]) $ \i -> do
            getVar env i >>= \w -> return $ show w

        return $ intercalate " " res

    putStrLn $ intercalate "\n" res

    where readEdge = \e _ -> do
            (n1:n2:_) <- liftM parseInts getLine
            return $ e ++ [(n1, n2)]

parseInts :: String -> [Int]
parseInts = (map read) . words

traverse' :: Env -> G.Graph -> Int -> G.Vertex -> IO ()
traverse' env g w v = do
    let neighbors = reachable g v
    yetToReach <- filterM (existOrSetVar env w) neighbors
    if length (yetToReach) == 0
        then return ()
        else mapM_ (traverse' env g (w+6)) (tail yetToReach)
