{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- parse messages from a log file

parseMessage :: String -> LogMessage
parseMessage msg = process (words msg)
    where process :: [String] -> LogMessage
          process ("E":level:ts:string) = LogMessage (Error (read level :: Int)) (read ts :: TimeStamp) (unwords string)
          process ("I":ts:string)       = LogMessage Info (read ts :: TimeStamp) (unwords string)
          process ("W":ts:string)       = LogMessage Warning (read ts :: TimeStamp) (unwords string)
          process string                = Unknown (unwords string)

parse :: String -> [LogMessage]
parse content = [parseMessage msg | msg <- lines content]


-- store parsed log messages into a tree

insert :: LogMessage -> MessageTree -> MessageTree
insert = undefined