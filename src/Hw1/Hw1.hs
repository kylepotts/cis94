module Hw1
(
  toDigits,
  toDigitsRev
) where

toDigits :: Integer -> [Integer]
toDigits n
  | n < 1     = []
  | n < 10    = [n]
  | otherwise = toDigits(n `div` 10) ++ [(n `mod` 10)]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

-- essentially does [1,2,1,2 ...] * list argument
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = zipWith (*) (cycle [1,2]) xs

sumDigits :: [Integer] -> Integer
sumDigits = sum . concat . (map  toDigits)

validate :: Integer -> Bool
validate x = ((sumDigits . doubleEveryOther .  toDigitsRev $ x) `mod` 10) == 0

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ =  []
hanoi x a b c = hanoi(x-1) a c b ++ [(a,b)] ++ hanoi (x-1) c b a 