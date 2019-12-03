import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
bin2int bits = sum [w*b | (w,b) <- zip weights bits]
               where weights = iterate (*2) 1

-- bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

-- Binary String transmitter part

encode :: String -> [Bit]
-- make8extended call added, exercise 7
encode = concat . map (make8extended . make8 . int2bin . ord)


chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
-- if statement added to check the last bit and throw an error
chop8 bits = if customCondition then error "Custom error fired" else take 8 allBits : chop8 (drop 9 bits)
             where customCondition =  (head (reverse allBits)) == 1
                   allBits = take 9 bits


decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
-- Exercise 8. Commented out a line and added a tail call that simulates that the first bit has been lost
-- channel = id
channel = tail

-- Exercise 7, page 90
-- function that adds additional ninth bit depending on the condition
make8extended :: [Bit] -> [Bit]
make8extended xs = if rule then xs ++ [0] else xs ++ [1]
                   where rule = (length xs `mod` 2) == 0

-- Exercise 9. Write function altMap
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f1 f2 (x:xs) = f1 x : altMap f2 f1 xs