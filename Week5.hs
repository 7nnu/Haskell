countSpaces :: String -> Int
countSpaces "" = 0
countSpaces (x : xs)
    | x == ' ' = 1 + countSpaces xs
    | otherwise = countSpaces xs
    
mergeLists :: [Int] -> [Int] -> [Int]
mergeLists [] ys = ys
mergeLists xs [] = xs
mergeLists (x:xs) (y:ys)
    | x <= y = x : mergeLists xs (y:ys)
    | otherwise = y :mergeLists (x:xs) ys
    
headPlusOne :: [Int] -> Int
headPlusOne [] = -1
headPlusOne (x:_) = x + 1

duplicateHead :: [a] -> [a]
duplicateHead [] = []
duplicateHead (x:xs) = x : x : xs

rotate :: [a] -> [a]
rotate (x:y:xs) = y : x : xs
rotate xs = xs

listLength :: [a] -> Int
listLength [] = 0
listLength (_:xs) = 1 + listLength xs

multAll :: [Int] -> Int
multAll [] = 1
multAll (x:xs) = x * multAll xs

andAll :: [Bool] -> Bool
andAll [] = True
andAll (x:xs) = x && andAll xs

orAll :: [Bool] -> Bool
orAll [] = False
orAll (x:xs) = x || orAll xs

countIntegers :: Int -> [Int] -> Int
countIntegers _ [] = 0
countIntegers n (x:xs)
    | n == x = 1 + countIntegers n xs
    | otherwise = countIntegers n xs
    
removeAll :: Int -> [Int] -> [Int]
removeAll _ [] = []
removeAll n (x:xs)
    | n == x = removeAll n xs
    | otherwise = x : removeAll n xs

removeAllButFirst :: Int -> [Int] -> [Int]
removeAllButFirst _ [] = []
removeAllButFirst n (x:xs)
    | n == x = x : removeAll n xs
    | otherwise = x : removeAllButFirst n xs
    
type StudentMark = (String, Int)

testData :: [StudentMark]
testData =
  [ ("John", 53),
    ("Sam", 16),
    ("Kate", 85),
    ("Jill", 65),
    ("Bill", 37),
    ("Amy", 22),
    ("Jack", 41),
    ("Sue", 71)
  ]

listMarks :: String -> [StudentMark] -> [Int]
listMarks _ [] = []
listMarks name ((student,mark):xs)
    | name == student = mark : listMarks name xs
    | otherwise = listMarks name xs 
    
sorted :: [Int] -> Bool
sorted [] = True
sorted [_] = True
sorted (x:y:xs)
    | x <= y = sorted (y:xs)
    | otherwise = False
    
prefix :: [Int] -> [Int] -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (x:xs) (y:ys)
    | x == y  = prefix xs ys
    | otherwise = False
    
subSequence :: [Int] -> [Int] -> Bool
subSequence [] _ = True
subSequence _ [] = False
subSequence xs (y:ys)
  | prefix xs (y:ys) = True
  | otherwise        = subSequence xs ys