{--
    Exercises 6.8, page 71, Recursive function calls
--}
import Prelude hiding ((^),(!!))
-- Exercise #1: Edit factorial function by adding the guard prohibiting
-- to use a negative numbers as an arguments

fac :: Int -> Int
fac 0 = 1
-- it could be done this way (out of the scope of the exercise definition)
-- fac x | x < 0 = (-1)^(-x) * fac (-x)
fac x | x < 0 = 1
      | otherwise = x * fac(x-1)

-- Exercise #2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown x = x + sumdown(x-1)

-- Exercise #3
(^) :: Integral a => a -> a -> a
(^) 0 _ = 0
(^) x 1 = x
(^) x y = x * (x ^ (y - 1))
{--
    2 ^ 3 ->^ 2 * ((^) 2 (3 - 1))
          ->^ 2 * (2 * ((^) 2 * (2 - 1)))
          ->* 2 * (2 * 2)
          ->* 2 * 4
          ->* 8
--}

-- Exercise #4: Euclid's algorithm for finding greatest common devisor
euclid :: Int -> Int -> Int
-- example: euclid 6 27 -> 3
euclid _ 0 = 1
euclid 0 _ = 1
euclid a b | a > b  = euclid (a - b) b
           | a < b  = euclid a (b - a)
           | a == b = a
           
-- Exercise #5: c prefix means custom
clength :: [a] -> Int
clength []     = 0
clength (x:xs) = 1 + length xs
{--
    [1,2,3] -> 1 + length [2,3,[]]
            -> 1 + 1 + length [3,[]]
            -> 1 + 1 + 1 + 0
--}

cdrop :: Int -> [a] -> [a]
cdrop 0 xs     = xs
cdrop _ []     = []
cdrop a (x:xs) = cdrop (a-1) xs
{--
    2 [1,2,3] -> cdrop (2-1) [2,3,[]]
              -> cdrop (1-1) [3,[]]
              -> [3]
--}

cinit :: [a] -> [a]
cinit []     = []
cinit [x]    = []
cinit (x:xs) = x : cinit xs
{--
    [1,2,3] -> 1 : cinit [2,3,[]]
            -> 1 : 2 : cinit [3,[]]
            -> 1 : 2 : []
            -> [1,2]
--}

-- Exercise #6
-- And for list of boolean values
uand :: [Bool] -> Bool
uand [] = True
uand (False:_) = False
uand (x:xs) = x && uand xs
{--
    [True, False,True] -> False
    [True,True] -> True
--}

-- Concats a list of lists in a single list of the same type
uconcat :: [[a]] -> [a] -- [[1,2,3],[4,5,6]] -> [1,2,3,4,5,6]
uconcat (x:[]) = x
uconcat (x:xs) = x ++ uconcat xs -- [1,2,3] ++ [4,5,6] ++ []
-- Lists pattern matching explanation
-- [1,2,3] -> 1 : 2 : 3 : []
-- (x:xs) -> x  -> 1
--        -> xs -> [2,3]

-- Replicate the element nth times
ureplicate :: Int -> a -> [a]
ureplicate 0 _ = []
ureplicate a b = b : ureplicate (a-1) b

-- Select the nth element of the list
(!!) :: [a] -> Int -> a
(!!) (x:xs) b = if b == 1 then x else (!!) xs (b-1)

-- Decide if an element is an element of the list
celem :: Eq a => a -> [a] -> Bool
celem _ []     = False
celem a (x:xs) = if a == x then True else celem a xs

-- Exercise #7
-- Define a function for merging two sorted lists into one
cmerge :: Ord a => [a] -> [a] -> [a] -- Attention!!! Fighted with wrong type Eq instead of Ord
-- [2,5,6] [1,3,4] = [1,2,3,4,5,6]
cmerge xs []         = xs
cmerge [] ys         = ys
cmerge (x:xs) (y:ys) = if x <= y then x : cmerge xs (y:ys) else y : cmerge (x:xs) ys

-- Exercise #8
-- Halve copied from the functions.hs
halve :: [a] -> ([a],[a])
halve xs = splitAt ((length xs) `div` 2) xs

-- Comparison with Java's merge sort implementation (check out the length): https://www.geeksforgeeks.org/merge-sort/
msort :: Ord a => [a] -> [a]
msort []  = []
msort [a] = [a]
msort xs  = cmerge (msort (fst h)) (msort (snd h))
            where h = halve xs
            
{--
    Exercise #9:
        - sum of the list of integers
        - take n elements from the list
        - select the last element from the non-empty list
--}
csum :: Integral a => [a] -> a
csum []     = 0
csum (x:xs) = x + csum xs

take' :: Int -> [a] -> [a]
take' n _ | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

clast :: [a] -> a
clast (x:[]) = x
clast [] = error "Empty list"
clast (_:xs) = clast xs

