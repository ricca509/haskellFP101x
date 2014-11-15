import Prelude hiding ((&&))

halve_a xs = (take n xs, drop n xs)
    where n = length xs `div` 2

remove n xs = take n xs ++ drop (n + 1) xs

funct x xs = take (x + 1) xs ++ drop x xs

a && b =
    if b
        then
            a
        else
            False
