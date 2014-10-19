mysum n =
    if n == 1
        then n
        else n + mysum (n - 1)
