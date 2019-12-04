{--Conditional expressions--}

condFoo :: Bool
condFoo = if x > 5 then True else False
            where x = 10

{--Guarded equations--}

condBaz :: Integral a => (a,a) -> a
condBaz (x,y) | x == y = 0
              | x >  y = 1
              | x <  y = -1

{--Pattern matching--}

zooFoo :: String -> Bool
zooFoo "Haskell" = True
zooFoo _ = False

firstEl :: [a] -> a
firstEl (x:_) = x 

createEls :: Integral a => a -> [a]
createEls x = [x..100]

thirdEl :: [a] -> a
thirdEl (_:_:x:_) = x

{--Exercises 4.8, page 45--}

-- Exericse 1
halve :: [a] -> ([a],[a]) -- splits even length list into two halves
halve xs = splitAt ((length xs) `div` 2) xs -- div a b == a `div` b

-- Exericse 2
third :: [a] -> a
-- pattern matching
-- third (_:_:x:xs) = x
-- head and tail
-- third xs = head (tail (tail xs))
-- !! indexing
third xs = ((!!) xs) 2

{--
    Exericse 3
    Create safetail function that behaves exactly the same as tail, but returns an empty list in case of empty list.
    You should use:
    - conditions
    - guarded equations
    - pattern matching
--}
safetail :: [a] -> [a]
-- safetail xs = if (null xs) then [] else (tail xs)
-- safetail xs | (null xs) = []
--             | otherwise = tail xs
safetail [] = []
safetail (_:xs) = xs

{-- Exercise 4
(||) :: Bool -> Bool -> Bool
False || False = False
True  || False = True
False || True  = True
True  || True  = True

False || False = False
_     || _     = True

False || b = b
True  || _ = True

b || c | b == c    = b
       | Otherwise = True
--}

{--
    Exercise 5:
    (&&) :: Bool -> Bool -> Bool
    True && True = True
    _    && _    = False
--}

(&&) :: Bool -> Bool -> Bool

-- O.Chmut
-- a && b = if a == True then
--            if b == True then True
--                else False
--            else False

-- Me
-- a && b = if a == b then
--            if a == False then False
--                else True
--            else False

{--
    Exercise 6:
    (&&) :: Bool -> Bool -> Bool
    True && b = b
    False && _ = False
--}

a && b = if a == True then b else False

{--
    Exercise 7:
    Formalize in terms of lambda expressions.
    mult :: Int -> Int -> Int -> Int
    mult x y z = x*y*z
-}

mult :: Int -> Int -> Int -> Int
-- mult x y z = x*y*z
mult = \x -> (\y -> (\z -> x*y*z))

{--
    Exercise 8:
--}

luhnDouble :: Int -> Int
luhnDouble x = if (x*2)>9 then (x*2)-9 else x*2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (((luhnDouble a) + b + (luhnDouble c) + d) `mod` 10) == 0
