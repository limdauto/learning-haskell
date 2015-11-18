{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- parse messages from a log file

parseMessage :: String -> LogMessage
parseMessage = process . words
    where process :: [String] -> LogMessage
          process ("E":level:ts:string) = LogMessage (Error (read level :: Int)) (read ts :: TimeStamp) (unwords string)
          process ("I":ts:string)       = LogMessage Info (read ts :: TimeStamp) (unwords string)
          process ("W":ts:string)       = LogMessage Warning (read ts :: TimeStamp) (unwords string)
          process string                = Unknown (unwords string)

parse :: String -> [LogMessage]
parse content = [parseMessage msg | msg <- lines content]

-- compare 2 log messages
-- TODO: replace with ordering

gt :: LogMessage -> LogMessage -> Bool
(Unknown _) `gt` _ = False
_ `gt` (Unknown _) = False
(LogMessage _ ts1 _) `gt` (LogMessage _ ts2 _) = ts1 >= ts2

-- store parsed log messages into a tree

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message Leaf = Node Leaf message Leaf
insert message (Node left node right) =
    case message `gt` node of
        False   -> Node (insert message left) node right
        True    -> Node left node (insert message right)

build :: [LogMessage] -> MessageTree
build messages = process messages Leaf
    where process :: [LogMessage] -> MessageTree -> MessageTree
          process []     tree = tree
          process (x:xs) tree = process xs (insert x tree)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left message right) = (inOrder left) ++ [message] ++ (inOrder right)

-- analysis

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = filter (\s -> length s > 0) (map toString messages)
    where toString :: LogMessage -> String
          toString (LogMessage (Error severity) _ string)
            | severity >= 50 = string
            | otherwise      = ""
          toString _ = ""
