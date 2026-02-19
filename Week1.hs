circumferenceOfCircle :: Float -> Float
circumferenceOfCircle d = pi * d

sideOfCylinder :: Float -> Float -> Float
sideOfCylinder d h = circumferenceOfCircle d * h 

canDrink :: Int -> Bool
canDrink age = age >= 18

all3CanDrink :: Int -> Int -> Int -> Bool
all3CanDrink a b c = canDrink a && canDrink b && canDrink c

timesTen :: Int -> Int
timesTen a = a * 10

sumThree :: Int -> Int -> Int -> Int
sumThree a b c = a + b + c 

areaOfCircle :: Float -> Float
areaOfCircle r = pi * r^2

volumeOfCylinder :: Float -> Float -> Float
volumeOfCylinder h r = areaOfCircle r * h

distance :: Float -> Float -> Float -> Float -> Float
distance x1 y1 x2 y2= sqrt ((y1-y2)^2 + (x1-x2)^2)

threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent x y z  = x /= y && x /= z && y /= z

divisibleBy :: Int -> Int -> Bool
divisibleBy x y  = x `mod` y == 0

isEven :: Int -> Bool
isEven x = divisibleBy x 2

averageThree :: Int -> Int -> Int -> Float
averageThree x y z = fromIntegral (x + y + z) / 3

absolute :: Int -> Int
absolute x = 
    if x > 0 
        then x
        else -x
