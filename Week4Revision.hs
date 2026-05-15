import Data.Char

sumNumbersBetween :: Int -> Int -> Int
sumNumbersBetween x y = sum [i | i <- [x ..y], mod i 2 == 0]


type StudentMark = (String, Int)

marks :: [StudentMark] -> [Int]
marks stmks = [mk | (st,mk) <- stmks]

sumMarks :: [StudentMark] -> Int
sumMarks stmks = sum [mk | (_, mk) <- stmks]

numberOfStudents :: [StudentMark] -> Int
numberOfStudents stmks = length stmks

averageMark :: [StudentMark] -> Float
averageMark [] = 0
averageMark stmks = fromIntegral sumMarks / fromIntegral numberOfStudents
    where
    sumMarks = sum [mk | (_ , mk) <- stmks]
    numberOfStudents = length stmks

sumDifference :: Int -> Int -> (Int, Int)
sumDifference x y = (x + y, x - y)

grade :: StudentMark -> Char
grade (_,mark)
    | mark < 0 || mark > 100 = error "Mark is not a valid number"
    | mark >= 70 = 'A'
    | mark >= 60 = 'B'
    | mark >= 50 = 'C'
    | mark >= 40 = 'D'
    | otherwise = 'F'
    

capMark :: StudentMark -> StudentMark
capMark (st,mk)
    | mk < 0 || mk > 100 = error "Mark is not a valid number"
    | mk > 40 = (st,40)
    | otherwise = (st,mk)
    

firstNumbers :: Int -> [Int]
firstNumbers 0 = []
firstNumbers n = firstNumbers (n - 1) ++ [n]

firstSquares :: Int -> [Int]
firstSquares 0 = []
firstSquares n = firstSquares (n - 1) ++ [n^2]

capitalise :: String -> String
capitalise xs = [toUpper x | x <- xs]

onlyDigits :: String -> String
onlyDigits [] = []
onlyDigits (x:xs)
    | isDigit x = x : onlyDigits xs
    | otherwise = onlyDigits xs
    
capMarks :: [StudentMark] -> [StudentMark]
capMarks xs = [capMark x | x <- xs]

gradeStudents :: [StudentMark] -> [(String,Char)]
gradeStudents marks = [(name, grade (name, m)) | (name, m) <- marks]