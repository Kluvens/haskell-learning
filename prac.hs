module Practice where

isEven :: Int -> Bool
isEven x = ((x `mod` 2) == 0)

isPositive :: Int -> Bool
isPositive x = x > 0

toNthPower :: Double -> Double -> Double
toNthPower n x = x ** n

toCube :: Double -> Double
toCube x = toNthPower 3 x

phi :: Double
phi = (1 + sqrt 5) / 2

div2 :: Int -> (Int, Int)
div2 x = (divResult, reminder) where
    divResult :: Int
    divResult = x `div` 2
    reminder :: Int
    reminder = x `mod` 2
    
polarity :: Int -> String
polarity x = if (x > 0) then "Positive" else if (x < 0) then "Negative" else "zero"

polarity2 :: Int -> String
polarity2 x
    | x > 0 = "Positive"
    | x < 0 = "Negative"
    | otherwise = "zero"
    
polarity3 x = case x > 0 of 
    False -> case x < 0 of
        False -> "zero"
        True -> "Negative"
    True -> "Positive"
    
-- recursion to get factorial
fact :: Int -> Int -> Int -> Int
fact n result i
    | i <= n = fact n (result * i) (i + 1)
    | otherwise = result
    
factorial :: Int -> Int
factorial n = product [1 .. n]

isEmpty :: [a] -> Bool
isEmpty xs = case xs of
    [] -> True
    (y:ys) -> False
    
isEmpty' :: [a] -> Bool
isEmpty' [] = True
isEmpty' (y:ys) = False

product' :: (Int) -> Int
product' [] = 1
product' (y:ys) = y * product' ys

-- The following is to illustrate enumerated data types

-- with data, we are creating a new type
data Document = Passport | BirthCert | License | StudentID | CreditCard

data Category = Primary | Secondary | Tertiary

category :: Document -> Category
category Passport -> Primary
category BirthCert -> Primary
category License -> Secondary
category StudentID -> Secondary
category _ -> Tertiary
    


