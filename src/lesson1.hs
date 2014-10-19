double x = x + x

factorial n = product [1..n]

n = a `div` length xs
    where
        a = 10
        xs = [1, 2, 3, 4, 5]

last_1 ns = head (reverse ns)

last_2 ns = take 1 (reverse ns)
