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

halve :: [a] -> ([a],[a]) -- splits even length list into two halves
halve xs = splitAt ((length xs) `div` 2) xs -- div a b == a `div` b

third :: [a] -> a
-- pattern matching
-- third (_:_:x:xs) = x
-- head and tail
-- third xs = head (tail (tail xs))
-- !! indexing
third xs = ((!!) xs) 2

{--
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
