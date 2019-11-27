-- Task from CodeWars. Name: Tribonacci, recursion
tribonacci :: Num a => (a,a,a) -> Int -> [a]
tribonacci _ 0 = []
tribonacci (a,b,c) n = a : tribonacci (b,c,(a+b+c)) (n-1)

-- Exercises 7.9, page 89

{-- Exercise 1. Rewrite [f x | x <- xs, p x] with map and filter
    map f (filter p xs)
--}

f1 :: (a -> Bool) -> (a -> b) -> [a] -> [b]
f1 p f xs = map f $ filter p xs

-- Exercise 2
-- Option A:

customAll :: (a -> Bool) -> [a] -> Bool
customAll f = and . map f -- ... = and (map f xs)

customAny :: (a -> Bool) -> [a] -> Bool
customAny f = or . map f -- ... = or (map f xs)


