import Prelude hiding ((^), and, concat, replicate, (!!), elem)

prod :: [a] -> Int
prod []     = 1
prod (x:xs) = x * prod xs

leng :: [a] -> Int
leng []     = 0
leng (_:xs) = 1 + leng xs

rev :: [a] -> [a]
rev []     = []
rev (x:xs) = rev xs ++ [x]

init_ :: [a] -> [a]
init_ [_]    = []
init_ (x:xs) = x : init xs

-- 1. Define the exponentiation operator ↑ for non-negative integers using the
-- same pattern of recursion as the multiplication operator ∗, and show how
-- 2 ↑ 3 is evaluated using your definition.

(^) :: Int -> Int -> Int
n ^ 0 = 1
m ^ n = m * m ^ (n - 1)

-- 2 ^ 3
-- 2 * 2 ^ 2
-- 2 * 2 * 2 ^ 1
-- 2 * 2 * 2 * 2 ^ 0
-- 2 * 2 * 2 * 1
-- 8

-- 2. Using the definitions given in this chapter, show how length [1, 2, 3],
-- drop 3 [1, 2, 3, 4, 5], and init [1, 2, 3] are evaluated.

-- length [1, 2, 3]
-- 1 + length [2, 3]
-- 1 + 1 + length [3]
-- 1 + 1 + 1 + length []
-- 1 + 1 + 1 + 0
-- 3

-- drop 3 [1, 2, 3, 4, 5]
-- drop 2 [2, 3, 4, 5]
-- drop 1 [3, 4, 5]
-- drop 0 [4, 5]
-- [4, 5]

-- init [1, 2, 3]
-- 1 : init [2, 3]
-- 1 : 2 : init [3]
-- 1: 2: []
-- [1, 2]

-- 3. Without looking at the definitions from the standard prelude, define the
-- following library functions using recursion.

-- – Decide if all logical values in a list are True:
and :: [Bool] -> Bool
and [] = True
and (x:xs)
   | x == False = False
   | otherwise  = and xs

-- – Concatenate a list of lists:
concat :: [[a]] -> [a]
concat []     = []
concat (x:xs) = x ++ concat xs

-- – Produce a list with n identical elements:
replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = [x] ++ replicate (n - 1) x

-- – Select the nth element of a list:
(!!) :: [a] -> Int -> a
(x:xs) !! 0 = x
(x:xs) !! n = xs !! (n - 1)

-- [1, 2, 3] !! 1
-- [2, 3] !! 0
-- 2

-- – Decide if a value is an element of a list:
elem :: Eq a => a -> [a] -> Bool
elem _ []       = False
elem n (x:xs)
    | n == x    = True
    | otherwise = elem n xs

