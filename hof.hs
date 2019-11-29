-- Exercises 7.9, page 89
import Prelude hiding (all, any, takeWhile, dropWhile, map, filter, curry, uncurry)

-- Exercise 1. Rewrite [f x | x <- xs, p x] with map and filter

-- map f (filter p xs)

-- Exercise 2
-- a. Decide if all elements of a list satisfy a predicate:

all :: (a -> Bool) -> [a] -> Bool
all f = and . map f -- ... = and (map f xs)

-- b. Decide if any element of a list satisfy a predicate:

any :: (a -> Bool) -> [a] -> Bool
any f = or . map f -- ... = or (map f xs)

-- c. Select elements from a list while they satisfy a predicate:

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ []                 = []
takeWhile p (x:xs) | p x       = x : takeWhile p xs
                   | otherwise = []
                   
-- d. Remove elements from a list while they satisfy a predicate:

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ []                      = []
dropWhile p (x:xs) | p x            = dropWhile p xs
                   | p x == False   = x : xs

-- Exercise 3.
-- Redefine the functions map f and filter p using foldr.
-- map f
map :: (a -> b) -> [a] -> [b]
-- map _ [] = []
-- map f (x:xs) = f x : map f xs
{--
    Typically fold deals with two things: a combination function, and a data structure, typically a list of elements.
    The fold then proceeds to combine elements of the data structure using the function in some systematic way.
    On lists, there are two obvious ways to carry this out:
    either by recursively combining the first element with the results of combining the rest (called a right fold)
    or by recursively combining the results of combining all but the last element with the last one, (called a left fold).
    (ref: https://wiki.haskell.org/Fold)
--}
map f = foldr (\a xs -> f a : xs) []

-- filter p
filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x xs -> if p x then x:xs else xs) []

{--
    Exercise 4.
    Using foldl, define a function dec2int :: [Int] -> Int that converts a decimal number into an integer.
    For example:
    > dec2int [2,3,4,5]
    2345
--}
dec2int :: [Int] -> Int
dec2int = foldl (\x xs -> x * 10 + xs) 0
-- (((2 * 10 + 3) * 10 + 4) * 10 + 5)

-- Exercise 5. Define curry and uncurry functions.
curry :: ((a,a) -> b) -> (a -> a -> b)

curry f = \x -> \y -> f (x,y)

uncurry :: (a -> a -> b) -> ((a,a) -> b)

uncurry f = \(x,y) -> f x y

