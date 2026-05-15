import Prelude hiding ((||), (&&), gcd)
infixr 3 &&


nor :: Bool -> Bool -> Bool
nor False x = not x
nor True _ = False

fibonacci :: Int -> Int
fibonacci n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = fibonacci (n - 1) + fibonacci (n - 2)

fibonacciPattern :: Int -> Int
fibonacciPattern 0 = 0
fibonacciPattern 1 = 1
fibonacciPattern n = fibonacciPattern (n-1) + fibonacciPattern (n-2)

(||) :: Bool -> Bool -> Bool
True || _ = True
False || p = p

(&&) :: Bool -> Bool -> Bool
True && x = x
False && _ = False

ex0r :: Bool -> Bool -> Bool
ex0r True False = True
ex0r False True = True 
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

validDate :: Int -> Int -> Bool
validDate x y = daysInMonth x >= y

sumNumbers :: Int -> Int
sumNumbers 0 = 0
sumNumbers n = n + sumNumbers (n - 1)

sumSquares :: Int -> Int
sumSquares 0 = 0
sumSquares n = n^2 + sumSquares (n-1)

power :: Int -> Int -> Int
power _ 0 = 1
power n m = n * power n ( m-1 )

sumFromTo :: Int -> Int -> Int
sumFromTo x y
    | x > y = 0
    | x == y = y
    | otherwise = x + sumFromTo (x+1) y 
    
gcd :: Int -> Int -> Int
gcd x y
    | x == y = x
    | x > y = gcd (x - y ) y
    | otherwise =  gcd x (y - x)
    
intSquareRoot :: Int -> Int
intSquareRoot n = findRoot n 0

findRoot :: Int -> Int -> Int
findRoot n x
    | x * x > n = x - 1
    | otherwise = findRoot n ( x+1)