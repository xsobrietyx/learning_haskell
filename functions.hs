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
thirdEl (_:_:x:xs) = x
