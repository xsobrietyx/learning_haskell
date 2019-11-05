-- Init
fact x = product [1..x]

double x = x * x

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

-- Custom init impl, exercise on page 21
customInit xs = reverse (drop 1 (reverse xs))

customInit2 xs = reverse (tail (reverse xs))

-- Functions

-- same as newAdd a b = a + b , which type is newAdd :: Int -> Int -> Int
newAdd :: Int -> (Int -> Int)
newAdd a b = a + b
