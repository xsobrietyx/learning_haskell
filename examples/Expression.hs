module ADT.Expr where

data Expr = Val Int | Div Expr Expr

{--
    Maths                           Haskell
    1                               Val 1
    6 / 2                           Div (Val 6) (Val 2)
    6 / (3 / 1)                     Div (Val 6) (Div (Val 3) (Val 1))
--}

-- Could fail with div by zero
eval :: Expr -> Int
eval (Val n)   = n
eval (Div x y) = (eval x) `div` (eval y)

-- Failsafe version
safeDiv :: Int -> Int -> Maybe Int
safeDiv n m = if m == 0 then Nothing else Just (n `div` m)

eval1 :: Expr -> Maybe Int
eval1 (Val n) = return n
eval1 (Div x y) = case eval1 x of
                    Nothing -> Nothing
                    Just n -> case eval1 y of
                                Nothing -> Nothing
                                Just m -> safeDiv n m

{--
    Monad's pattern:
    case m of
        Nothing -> Nothing
        Just x -> f x

    m >>= f = case m of
                Nothing -> Nothing
                Just x  -> f x
--}

{--
    Better and concise version. Monad.
    eval2 :: Expr -> Maybe Int
    eval2 (Val x)   = return x
    eval2 (Div x y) = eval2 x >>= (\n ->
                            eval2 y >>= (\m -> safeDiv n m))
--}

-- Do notation
eval3 :: Expr -> Maybe Int
eval3 (Val n) = return n
eval3 (Div x y) = do n <- eval3 x
                     m <- eval3 y
                     safeDiv n m

{--
    Maybe monad
    return :: a -> Maybe a
    >>= :: Maybe a -> (a -> Maybe b) -> Maybe b
--}

{--
    - Same idea is used in other cases, effects
    - Supports pure programming with effects
    - Use of effects is explicit in types
    - Function that works for any effect
--}