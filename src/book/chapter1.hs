-- 1. Give another possible calculation for the result of double (double 2)

quadruple n = n * 4

-- 2. Show that sum [x] = x for any number x

testSum (x : xs) =
    if length xs == 0
        then x
        else x + testSum xs

-- 3. Define a function product that produces the product of a list of numbers,
-- and show using your definition that product [2, 3, 4] = 24.

testProduct (x : xs) =
    if length xs == 0
        then x
        else x * product xs

-- testProduct [2, 3, 4]

-- 4. How should the definition of the function qsort be modified so that it
-- produces a reverse sorted version of a list?

qsort [] = []
qsort (x : xs) = qsort larger ++ [x] ++ qsort smaller
    where
        smaller = [p | p <- xs, p <= x ]
        larger = [p | p <- xs, p > x ]

-- 5. What would be the effect of replacing ≤ by < in the definition of qsort?
-- Hint: consider the example qsort [2, 2, 3, 1, 1].

-- qsort [2, 2, 3, 1, 1] using <

-- qsort [1, 1] ++ [2] ++ qsort [3]
-- qsort [] ++ [1] ++ qsort [] ++ [2] ++ qsort [] ++ [3] ++ qsort []
-- [] ++ [1] ++ [] ++ [2] ++ [] ++ [3] ++ []

-- Result: [1, 2, 3]
-- Removes duplicated values

--------------------------------------------------------------------------------

-- qsort [2, 2, 3, 1, 1] using <=

-- qsort [2, 1, 1] ++ [2] + qsort [3]
-- qsort [1, 1] + [2] + qsort [] ++ [2] ++ qsort [] ++ [3] ++ qsort []
-- qsort [1] ++ [1] ++ qsort [] ++ [2] ++ [] ++ [2] ++ [] ++ [3] ++ []
-- qsort [] ++ [1] ++ qsort [] ++ [1] ++ [] ++ [2] ++ [] ++ [2] ++ [] ++ [3] ++ []
-- [] ++ [1] ++ [] ++ [1] ++ [] ++ [2] ++ [] ++ [2] ++ [] ++ [3] ++ []

-- Result: [1, 1, 2, 2, 3]



qsort_1 [] = []
qsort_1 xs = x : qsort larger ++ qsort smaller
    where x = maximum xs
        smaller = [p | p <- xs, p < x ]
        larger = [p | p <- xs, p >= x ]
