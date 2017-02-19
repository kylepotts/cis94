module Hw2 where

import Log

parseMessage :: String -> LogMessage
parseMessage s = case words s of
  ("E":severity:timestamp:message) ->
    LogMessage (Error (read severity)) (read timestamp) (unwords message)
  ("I":timestamp:message) ->
    LogMessage Info (read timestamp) (unwords message)
  ("W":timestamp:message) ->
    LogMessage Warning (read timestamp) (unwords message)
  (_) ->
    Unknown s

parse :: String -> [LogMessage]
parse [] = []
parse s  = map parseMessage (lines s)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message@(LogMessage _ timestampA _) tree = case tree of
  (Leaf) -> Node Leaf message Leaf
  (Node _ (Unknown _) _) -> tree
  -- If timestamp of message to insert is less then roots timestamp, insert left else insert right
  (Node left root@(LogMessage _ timestampB _) right) -> case timestampA < timestampB of
    False -> Node left root (insert message right)
    True  -> Node (insert message left) root right

build :: [LogMessage] -> MessageTree
build messages = foldl (\tree message -> insert message tree) Leaf messages

inOrder :: MessageTree -> [LogMessage]
inOrder tree = case tree of
  (Leaf) -> []
  (Node left (Unknown _) right) -> inOrder(left) ++ inOrder(right)
  (Node left message right) -> inOrder(left) ++ [message] ++ inOrder(right)

isBadError :: LogMessage -> Bool
isBadError (LogMessage (Error sev) _ _ ) = sev > 49
isBadError _  = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = map (\(LogMessage _ _ message) -> message) 
  $ inOrder 
  $ build 
  $ filter (\ message -> isBadError message) messages