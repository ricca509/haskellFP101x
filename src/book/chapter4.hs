module Chapter4 where

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

safetail_b xs
    | null xs   = xs
    | otherwise = tail xs

safetail_c [] = []
safetail_c xs = tail xs

-- 3. In a similar way to ∧, show how the logical disjunction operator ∨ can be
-- defined in four different ways using pattern matching.
-- No need for four ways

True ∧ _  = True
False ∧ b = b

-- 4. Redefine the following version of the conjunction operator using conditional
-- expressions rather than pattern matching:
-- True ∧ True = True
-- _ ∧ _ = False

myAnd a =
	\ b ->
        if a == True && b == True
        then
            True
        else
            False

-- 5. Do the same for the following version, and note the difference in the number
-- of conditional expressions required:
-- True ∧ b = b
-- False ∧ _ = False

myAnd2 a =
	\ b ->
        if a == True
        then
            b
        else
            False

-- 6. Show how the curried function definition mult x y z = x ∗ y ∗ z can be
-- understood in terms of lambda expressions.

mult =
    \ x ->
        \ y ->
            \ z ->
                x * y * z
