module Chapter5 where

import Data.Char (ord, chr)
import Data.Ix (inRange)

factors n = [x | x <- [1 .. n], n `mod` x == 0]
isPerfect n = n == sum (init (factors n))

-- Caesar code

caesar :: Int -> String -> String
caesar k = map f
  where
    f c
      | inRange ('a','z') c = tr 'a' k c
      | inRange ('A','Z') c = tr 'A' k c
      | otherwise = c

unCaesar :: Int -> String -> String
unCaesar k = caesar (-k)

-- char addition
tr :: Char -> Int -> Char -> Char
tr base offset char = chr $ ord base + (ord char - ord base + offset) `mod` 26
