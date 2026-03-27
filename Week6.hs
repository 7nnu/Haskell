import Data.Char (isLower)

alwaysEven :: (Int -> Int) -> [Int] -> Bool
alwaysEven f = andAll . map (even . f)

andAll :: [Bool] -> Bool
andAll xs = foldr (&&) True xs

mult10 :: [Int] -> [Int]
mult10 xs = map (*10) xs 

onlyLowerCase :: String -> String
onlyLowerCase xs = filter isLower (xs)

orAll :: [Bool] -> Bool
orAll xs = foldr (||) False xs

sumSquares :: [Int] -> Int
sumSquares xs = sum (map (^2) xs)

zeroToTen :: [Int] -> [Int]
zeroToTen = filter zeroToTenRange

zeroToTenRange :: Int -> Bool
zeroToTenRange x = x >= 0 && x <= 10

squareRoots :: [Float] -> [Float]
squareRoots = map sqrt . filter (>=0)

countBetween :: Float -> Float -> [Float] -> Int
countBetween a b = length . filter (countBetweenLimit a b)

countBetweenLimit :: Float -> Float -> Float -> Bool
countBetweenLimit a b x = x >= a && x <= b


alwaysPositive :: (Float -> Float) -> [Float] -> Bool
alwaysPositive f = andAll . map ((>=0). f)

productSquareRoots :: [Float] -> Float
productSquareRoots = product . map sqrt . filter (>= 0)

removeFirst :: (a -> Bool) -> [a] -> [a]
removeFirst _ [] = []
removeFirst f (x:xs)
    | f x = xs
    | otherwise = x : removeFirst f xs
    
removeLast :: (a -> Bool) -> [a] -> [a]
removeLast f = reverse . removeFirst f . reverse

zeroToTenLambda :: [Int] -> [Int]
zeroToTenLambda xs = filter (\x -> x <= 10 && x >=0) xs

alwaysPositiveLambda :: (Float -> Float -> [Float] -> Bool
alwaysPositiveLambda f xs = andAll [ (\x -> f x > = 0) x | x <- xs ]

productSquareRootsLambda :: [Float] -> Float
productSquareRootsLambda xs = product [ (\x -> sqrt x) x | x <- xs, (\x -> x >= 0 x ]

reverseListLambda :: [a] -> [a]
reverseList xs = foldr (\x xs -> xs ++ [x]) []