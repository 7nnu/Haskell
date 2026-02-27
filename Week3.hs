import Prelude hiding ((||), (&&), gcd)
infixr 3 &&
nor :: Bool -> Bool -> Bool
nor False False = True
nor False True  = False
nor True  False = False
nor True  True  = False

fibonacci :: Int -> Int

fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)
{-

(&&) ::  Bool -> Bool -> Bool
False && False = False
False && True = False
True && False = False
True && True = True

(&&) :: Bool -> Bool -> Bool
True && True = True
_ && _ = False 
-}

(&&) :: Bool -> Bool -> Bool
False && _ = False
True && p = p

ex0r :: Bool -> Bool -> Bool
ex0r False True = True
ex0r True False = True
ex0r _ _ = False

ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse True x _ = x
ifThenElse False _ y = y

daysInMonth :: Int -> Int
daysInMonth 2 = 28
daysInMonth 4 = 30
daysInMonth 6 = 30
daysInMonth 9 = 30
daysInMonth 11 = 30
daysInMonth _ = 31

sumNumbers :: Int -> Int
sumNumbers 0 = 0
sumNumbers n = n + sumNumbers (n-1)

sumSquares :: Int -> Int
sumSquares 0 = 0
sumSquares n = n^2 + sumSquares (n-1)

power :: Int -> Int -> Int
power _ 0 = 1
power x n = x * power x (n-1)

sumFromTo :: Int -> Int -> Int
sumFromTo a b
    | b < a = 0
    | otherwise = a + sumFromTo(a+1) b
   
gcd :: Int -> Int -> Int
gcd a b
    | a == b = a
    | a > b = gcd (a-b) b
    | otherwise = gcd a (b-a)
    
intSquareRoot :: Int -> Int
intSquareRoot n = numRoot n n

numRoot :: Int -> Int -> Int
numRoot n s
    | s*s <= n && (s+1)*(s+1) > n = s
    | otherwise = numRoot n (s-1)