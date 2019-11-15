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

-- #6
factors :: Int -> [Int]
factors x = [a | a <- [1..x], b <- [1..x], a * b == x]

perfects :: Int -> [Int]
perfects x = [y | y <- [1..x], y == sum (tail (reverse (factors y)))]

-- #7
-- 1 comprehension 2 generators:
-- [(x,y) | x <- [1,2], y <- [3,4]]
-- generator #1:
-- x <- [1,2]
-- generator #2:
-- y <- [3,4]
-- result [(1,3),(1,4),(2,3),(2,4)]
-- replace this expression with 2 comprehensions nested into each other with one generator each, use function concat :: [[]] -> [a]

twoAsOne = concat [[(x,y) | y <- [3,4]] | x <- [1,2]]

{--
    Explanation to the exercise #7: Two elements we need to specify to fulfill this expression
    [(x,y) | y <- [3,4]]
    To provide this elements we're using the outer generator x <- [1,2]
    After fullfillment of x's in the left expression we got the list of tuples (2 tuples) as a result: [(1,3),(1,4)]
    The next iteration supplies x = 2 to the expression, and we got the next array as a result [(2,3),(2,4)]
    After concat function is called (flattering) we've got the result: [(1,3),(1,4),(2,3),(2,4)]
--}

{--
	#8 redefine function positions using function find.
	Positions:
	positions :: Eq a => a -> [a] -> [Int]
	positions x xs = [i | (a,i) <- zip xs [0..], x = a]
	Find:
	find :: Eq a => a -> [(a,b)] -> [b]
	find k t = [v | (a,v) <- t, k == a]
--}

pos2 :: Eq a => a -> [a] -> [Int]
pos2 x xs = find x [(a,b) | (a,b) <- zip xs [0..]]

-- #9 Scalar product

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [a*b | (a,b) <- zip xs ys]

