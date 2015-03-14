module DZ_2 where --DZ_2.hs
data Color = Red|Green|Black|Purple|Blue
fact::Int -> Int
fact n = if n == 0 
           then 1
           else n * fact(n-1)
fibb::Int -> Int
fibb n = case n of
			1 -> 1
			2 -> 1
			_ -> fibb(n-1)+fibb(n-2)
acc::(Integer, Integer) -> Integer
acc(m, n) = if (m < 0) || (n < 0)
				then -1
				else if m == 0
						then n+1
						else if n == 0
							then acc(m-1, 1)
							else acc(m-1, acc(m, n-1))