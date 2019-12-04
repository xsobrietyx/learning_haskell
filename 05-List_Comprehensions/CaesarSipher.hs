{--
    Exercises 5.7, page 57
    #10 Modify Caesar Sipher program to handle upper-case letters
--}

import Data.Char

let2int :: Char -> Int
let2int c = ord c - ord 'a'

let2int2 :: Char -> Int
let2int2 c = ord c - ord 'A'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

int2let2 :: Int -> Char
int2let2 n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | isUpper c = int2let2 ((let2int2 c + n) `mod` 26)
          | otherwise = c
          
encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]
