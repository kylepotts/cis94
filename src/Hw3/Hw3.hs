module Hw3 where 
every :: [a] -> Int -> [a]
every xs n = case drop n xs of
  []     -> []
  (y:ys) -> y : every ys n

skips :: [a] -> [[a]]
skips [] = []
skips x@(_:_) = map (every x) [0..length x - 1]