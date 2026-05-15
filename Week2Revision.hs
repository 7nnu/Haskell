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
absolute num
    | num < 0 = -num
    | otherwise = num
    
sign :: Int -> Int
sign num
    | num <0 = -1
    | num >0 = 1
    | otherwise = 0
    
howManyEqual :: Int -> Int -> Int -> Int
howManyEqual x y z
    | x == y && x == z = 3
    | x == y || x == z || y == z = 2
    | otherwise = 0
    
sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths a b c = diag a + diag b + diag c
    where
        diag x = sqrt (2 * x^2)
    
taxiFare :: Int -> Float
taxiFare km
    | km <= 10 = 2.2 + fromIntegral(km) * 0.5
    | otherwise = 2.2 + 10 * 0.5 + fromIntegral(km - 10) * 0.3
    
howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage x y z = count x + count y + count z
    where
        average = fromIntegral(x+y+z) /3
        count num
            | fromIntegral num > average =1
            | otherwise = 0

validDate :: Int -> Int -> Bool
validDate day month
    | month == 1 || month == 3 || month == 5 || month == 7 || month == 8 || month == 10 || month == 12 && day <= 31 = True
    | month == 4 ||  month == 6 || month == 9 || month == 11 && day <= 30 = True
    | month == 2 && day <= 28 = True
    | otherwise = False
    
daysInMonth :: Int -> Int -> Int
daysInMonth month year
    | month == 1 || month == 3 || month == 5 || month == 7 || month == 8 || month == 10 || month == 12 = 31
    | month == 4 ||  month == 6 || month == 9 || month == 11 = 30
    | (year `mod` 4) == 0 && month == 2 = 29
    | otherwise = 28