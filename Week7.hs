
import Text.Printf (printf)

-- Day algebraic type
data Day = Mon | Tue | Wed | Thur | Fri | Sat | Sun
  deriving (Eq, Ord, Show, Read)

-- Alternative definitions of isWeekend function
isWeekend :: Day -> Bool
isWeekend Sat = True
isWeekend Sun = True
isWeekend _ = False

isWeekend2 day = day == Sat || day == Sun

isWeekend3 day = day >= Sat

-- Copy of StudentMark type synonym from worksheet 4
data StudentMark = Student String Int
  deriving (Eq, Show)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (Student s1 m1) (Student s2 m2)
  | m1 >= m2 = s1
  | otherwise = s2

-- Shapes algebraic type
data Shape = Circle Float | Rectangle Float Float
    deriving (Show)

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle h w) = h * w

-- Address algebraic type (note that a constructor can have
-- the same name as the type).
data Address = Address Building String
  deriving (Show)

data Building = Name String | Number Int
  deriving (Show)

-- Binary tree algebraic type
data Tree = Null | Node Int Tree Tree
  deriving (Show)

-- Binary tree test data
testTree = Node 20 (Node 3 (Node 12 Null Null) (Node 7 Null Null)) (Node 8 (Node 4 (Node 6 Null Null) Null) Null)

-- Binary search tree test data
testSearchTree = Node 5 (Node 1 Null Null) (Node 8 (Node 7 Null Null) Null)

height :: Tree -> Int
height Null = 0
height (Node _ st1 st2) = 1 + max (height st1) (height st2)

sumValues :: Tree -> Int
sumValues Null = 0
sumValues (Node n st1 st2) = n + sumValues st1 + sumValues st2




type Make = String
type Model = String
type HorsePower = Int
type Price = Float

data EngineType = Petrol | Diesel | Electric
  deriving (Show, Eq)

data Engine = Engine EngineType HorsePower
  deriving (Show)

data CarName = CarName Make Model
  deriving (Show)

data Car = Car CarName Engine Price
  deriving (Show)

testCars :: [Car]
testCars =
  [ Car (CarName "Ford" "Fiesta") (Engine Petrol 55) 10000.0
  , Car (CarName "Ford" "Focus") (Engine Diesel 85) 15000.0
  , Car (CarName "Vauxhall" "Corsa") (Engine Petrol 55) 8000.0
  , Car (CarName "Vauxhall" "Astra") (Engine Diesel 81) 12000.0
  , Car (CarName "Vauxhall" "Astra") (Engine Diesel 96) 14000.0
  , Car (CarName "VolksWagen" "Golf") (Engine Electric 81) 20000.0
  ]

getMake :: Car -> Make
getMake (Car (CarName make _) _ _) = make

getModel :: Car -> Model
getModel (Car (CarName _ model) _ _) = model

getPrice :: Car -> Price
getPrice (Car _ _ price) = price

totalPrice :: [Car] -> Float
totalPrice [] = 0
totalPrice (Car _ _ price : cs) = price + totalPrice cs

filterByMake :: String -> [Car] -> [Car]
filterByMake manufacturer = filter (\c -> getMake c == manufacturer)

updatePriceAt :: Int -> Float -> [Car] -> [Car]
updatePriceAt _ _ [] = []
updatePriceAt 0 amount (c:cs) = updatePrice amount c : cs
updatePriceAt index amount (c:cs) =
  c : updatePriceAt (index - 1) amount cs

updatePrice :: Float -> Car -> Car
updatePrice newPrice (Car name engine _) =
  Car name engine newPrice

updatePriceAtAlt :: Int -> Float -> [Car] -> [Car]
updatePriceAtAlt index price cars =
  take index cars ++ [newCar] ++ drop (index + 1) cars
  where
    newCar = updatePrice price (cars !! index)

formatCar :: [Car] -> Int -> String
formatCar [] _ = ""
formatCar cars i =
  printf "%d- %s %s costs %.2f pounds"
    (i + 1)
    (getMake c)
    (getModel c)
    (getPrice c)
  where
    c = cars !! i
    
{- --------------------------- -}

data Month = January | February | March | April | May | June | July | August | September | October | November | December
    deriving (Show, Eq)
  
data Season = Spring | Summer | Autumn | Winter
    deriving (Show, Eq)
    
season :: Month -> Season
season m
  | m == December || m == January || m == February = Winter
  | m == March || m == April || m == May = Spring
  | m == June || m == July || m == August = Summer
  | otherwise = Autumn

numberOfDays :: Month -> Int -> Int
numberOfDays m y
  | m == April || m == June || m == September || m == November = 30
  | m == February && y `mod` 4 == 0 = 29
  | m == February = 28
  | otherwise = 31

data Point = Point Float Float
  deriving (Show)
 
data PositionedShape = PositionedShape Shape Point
  deriving (Show)
  
move :: PositionedShape -> Float -> Float -> PositionedShape
move (PositionedShape shape (Point x y)) ax ay =
  PositionedShape shape (Point (x + ax) (y + ay))
  
numberOfNodes :: Tree -> Int
numberOfNodes Null = 0
numberOfNodes (Node _ left right) =
  1 + numberOfNodes left + numberOfNodes right
  
isMember :: Int -> Tree -> Bool
isMember _ Null = False
isMember x (Node n left right)
  | x == n    = True
  | otherwise = isMember x left || isMember x right
  
leaves :: Tree -> [Int]
leaves Null = []
leaves (Node n Null Null) = [n]
leaves (Node _ left right) = leaves left ++ leaves right