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
