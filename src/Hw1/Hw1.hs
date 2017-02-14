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


