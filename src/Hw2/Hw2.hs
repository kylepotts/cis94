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

newNode:: LogMessage -> MessageTree
newNode a = Node Leaf a Leaf

