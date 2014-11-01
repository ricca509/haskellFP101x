-- 1. Using library functions, define a function halve :: [a] → ([a], [a]) that
-- splits an even-lengthed list into two halves. For example:
-- > halve [1, 2, 3, 4, 5, 6]
-- ([1, 2, 3], [4, 5, 6])

halve xs =
	if ( length (xs) `mod` 2 == 0 )
		then
			splitAt ((length xs) `div` 2) xs
		else
			(xs, xs)

-- 2. Consider a function safetail :: [a] → [a] that behaves as the library function
-- tail, except that safetail maps the empty list to itself, whereas tail
-- produces an error in this case. Define safetail using:
-- (a) a conditional expression;
-- (b) guarded equations;
-- (c) pattern matching.

safetail_a xs =
	if null xs
		then
			xs
		else
			tail xs

safetail_b xs | null xs = xs
			  | otherwise = tail xs

safetail_c [] = []
safetail_c xs = tail xs
