heartMonitor :: Int -> Int -> String
heartMonitor age bpm 
    | age > 80 && bpm > 100 = "High heart rate for 81+!"
    | age > 60 && bpm > 130 = "High heart rate for 61-80!"
    | age > 40 && bpm > 140 = "High heart rate for 41-60!"
    | age > 20 && bpm > 155 = "High heart rate for 21-40!"
    | age >= 0 && bpm > 170 = "High heart rate for 0-20!"
    | otherwise = "Normal heart rate"
    
pizzaCalories :: Int -> String -> Float
pizzaCalories diameter toppings = (11.5 + toppingCalories) * area
    where
        area = pi * (fromIntegral diameter / 2) ^ 2
        toppingCalories
            | toppings == "pepperoni" = 6
            | toppings == "tuna" = 4
            | toppings == "veggie" = 2.5
            | otherwise = 0

absolute :: Int -> Int
absolute number
    | number > 0 = number
    | otherwise = -number
    
sign :: Int -> Int
sign numb
    | numb > 0 = 1
    | numb == 0 = 0
    | otherwise = -1
    
howManyEqual :: Int -> Int -> Int -> Int
howManyEqual x y z
    | x == y && y == z = 3
    | x == z = 2
    | x == y = 2
    | y == z = 2
    | otherwise = 0

sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths x y z = xLength + yLength + zLength
    where
        xLength = sqrt (2 * x^2)
        yLength = sqrt (2 * y^2)
        zLength = sqrt (2 * z^2)
        
taxiFare :: Int -> Float
taxiFare km = fee + 2.2
    where
        fee
            | km <= 10  = fromIntegral km * 0.50
            | otherwise = 10 * 0.50 + fromIntegral (km - 10) * 0.30
            
howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage x y z = greaterX + greaterY + greaterZ
    where
        average = fromIntegral (x + y + z) /3
        
        greaterX =
            if fromIntegral x > average
                then 1
                else 0
        
        greaterY =
            if fromIntegral y > average
                then 1
                else 0
        
        greaterZ =
            if fromIntegral z > average
                then 1
                else 0
                
validDate :: Int -> Int -> Bool
validDate day month
    | month < 1 || month > 12 || day < 1 = False
    | month == 2 = day <= 28
    | month == 4 || month == 6 || month == 9 || month == 11 = day <= 30 
    | otherwise = day <= 31
        
 
daysInMonth :: Int -> Int -> Int
daysInMonth month year
    | month == 2 && mod year 4 == 0 = 29
    | month == 2 = 28
    | month == 4 || month == 6 || month == 9 || month == 11 = 30
    | otherwise = 31

   
sumThree :: Int -> Int -> Int -> Int
sumThree a b c = a + b + c 

threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent x y z  = x /= y && x /= z && y /= z

{-
sumThree 3 5 7 - def of SumThree
3 + 5 + 7 - arithmetic
8 + 7 - arithmetic
15

sumThree 8 (1+3) 2 - def of SumThree
8 + (1+3) + 2 - arithmetic
8 + 4 + 2 - arithmetic
12 + 2 - arithmetic
14

threeDifferent 1 4 2  - def of threeDifferent
1 /= 4 && 1 /= 2 && 4 /= 2 - def of /=
True && 1 /= 2 && 4 /= 2 - def of /=
True && True && 4 /= 2 - def of /=
True && True && True  - def of &&
True

threeDifferent 1 7 7 - def of threeDifferent
1 /= 7 && 1 /= 7 && 7 /= 7 - def of /=
True && 1 /= 7 && 7 /= 7 - def of /= 
True && True && 7 /= 7 - def of /=
True && True && True - def of &&
True

