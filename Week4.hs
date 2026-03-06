import Data.Char

sumNumbersBetween :: Int -> Int -> Int
sumNumbersBetween x y = sum [i | i <- [x..y]]

type StudentMark = (String, Int)

averageMark :: [StudentMark] -> Float
averageMark [] = 0
averageMark stmks = fromIntegral sumMarks / fromIntegral numberOfStudents
  where
    sumMarks = sum [mk | (_, mk) <- stmks]
    numberOfStudents = length stmks
    
sumDifference :: Int -> Int -> (Int,Int)
sumDifference x y = (x+y, x-y)

grade :: StudentMark -> Char
grade (_, mk)
    | mk > 100 || mk < 0 = error "Mark is not between 0 and 100"
    | mk >= 70 = 'A'
    | mk >= 60 = 'B'
    | mk >= 50 = 'C'
    | mk >= 40 = 'D'
    | otherwise = 'F'
    
capMark :: StudentMark -> StudentMark
capMark (name,mk)
    | mk < 0 || mk > 100 = error "Mark is not between 0 and 100"
    | mk >= 40 = (name,40)
    | otherwise = (name,mk)
    
firstNumbers :: Int -> [Int]
firstNumbers x = [i | i <- [1..x]]

firstSquares :: Int -> [Int]
firstSquares x = [i^2 | i <- firstNumbers x]

capitalise :: String -> String
capitalise x = [toUpper i | i <- x]

onlyDigits :: String -> String
onlyDigits x = [i | i <- x, isDigit i]

capMarks :: [StudentMark] -> [StudentMark]
capMarks stmks = [capMark i | i <- stmks]

gradeStudents :: [StudentMark] -> [(String, Char)]
gradeStudents stmks = [(name, grade i) | i@(name, _) <- stmks]

duplicateRecursive :: String -> Int -> String
duplicateRecursive x y 
    | y <= 0 = ""
    | otherwise = x ++ duplicateRecursive x (y-1)
    
duplicateList :: String -> Int -> String
duplicateList x y = "" ++ [i | _ <- [1..y], i <- x]

divisors :: Int -> [Int]
divisors x
    | x <= 0 = []
    | otherwise = [i | i <- [1..x], x `mod` i == 0]
    
isPrime :: Int -> Bool
isPrime n = divisors n == [1,n]

split :: [(a,b)] -> ([a],[b])
split xy = ([fst i | i <- xy], [snd i | i <- xy])