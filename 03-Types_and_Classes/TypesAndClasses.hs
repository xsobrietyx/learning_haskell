-- Init
fact x = product [1..x]

double1 x = x * x

quadro x = double (double x)

a = c + b
    where
        c = 1
        b = 2

-- Interesting one. Three mistakes corrected: (length xs), n, xs tab
n = a `div` (length xs)
        where
            a = 10
            xs = [1,2,3,4,5]

-- Exercises

-- Custom last by my own hand
newLast xs = head (drop ((length xs) - 1) xs)

-- Custom init impl, exercises 2.7
customInit xs = reverse (drop 1 (reverse xs))

customInit2 xs = reverse (tail (reverse xs))

-- Exercises 3.11
-- ['a','b','c'] -- [Char]
-- ('a','b','c') -- (Char, Char, Char)
-- [(False, '0'),(True, '1')] -- [(Bool, Char)]
-- ([False, True],['0','1']) -- ([Bool],[Char])
-- [tail, init, reverse] -- [[a]->[a]]

bools :: [Bool]
bools = [True,False,True]

nums :: [[Int]]
nums = [[1,2,3],[6,7,8]]

add :: Int -> Int -> Int -> Int
add a b c = a + b + c

copy :: a -> (a,a)
copy a = (a,a)

apply :: (a -> b) -> a -> b
apply x y = x y

-- next exercise

second :: [a] -> a
second xs = head (tail xs)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

pair :: a -> b -> (a,b)
pair x y = (x,y)

double :: Integral a => a -> a
double x = x*2

palindrome :: String -> Bool
palindrome xs = reverse xs == xs

twice :: (a->a) -> a -> a
twice f x = f (f x)
