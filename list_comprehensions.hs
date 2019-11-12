-- Guards
-- Find example

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (z,v) <- t, k == z]

-- find 'b' [('a',10),('b',13),('b',2)] ---> [13,2]

-- Count example

count :: Char -> String -> Int
count x xs = length [z | z <- xs, z == x]

-- count 's' "Mississipi" ---> 4

{--
    Exercises 5.7, page 57
--}

-- #1

hundredOfPows :: [Int]
hundredOfPows = [x*x | x <- [1..100]]

-- #2

grid :: Int -> Int -> [(Int, Int)]
grid x y = [(a,b) | a <- [0..x], b <- [0..y]]

-- #3

square :: Int -> [(Int,Int)]
-- invoke grid
-- filter results
square x = [zs | zs <- (grid x x), fst zs /= snd zs]

-- #4

replicateMy :: Int -> a -> [a]
replicateMy x y = [y | _ <- [1..x]]

-- #5

pyths :: Int -> [(Int,Int,Int)]
pyths x = [(a,b,c) | a <- [1..x], b <- [1..x], c <- [1..x], (a*a + b*b) == c*c]
