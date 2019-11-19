{--
    Exercises 6.8, page 71, Recursive function calls
--}
import Prelude hiding ((^))
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
euclid a b | a > b = euclid (a - b) b
           | a < b = euclid a (b - a)
           | a == b = a
           
-- Exercise #5: c prefix means custom
clength :: [a] -> Int
clength [] = 0
clength (x:xs) = 1 + length xs
{--
    [1,2,3] -> 1 + length [2,3,[]]
            -> 1 + 1 + length [3,[]]
            -> 1 + 1 + 1 + 0
--}

cdrop :: Int -> [a] -> [a]
cdrop 0 xs = xs
cdrop _ [] = []
cdrop a (x:xs) = cdrop (a-1) xs
{--
    2 [1,2,3] -> cdrop (2-1) [2,3,[]]
              -> cdrop (1-1) [3,[]]
              -> [3]
--}

cinit :: [a] -> [a]
cinit [] = []
cinit [x] = []
cinit (x:xs) = x : cinit xs
{--
    [1,2,3] -> 1 : cinit [2,3,[]]
            -> 1 : 2 : cinit [3,[]]
            -> 1 : 2 : []
            -> [1,2]
--}
