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
data Document = Passport | BirthCert | License | StudentID | CreditCard deriving (Show, Eq)

data Category = Primary | Secondary | Tertiary deriving (Show, Eq)

category :: Document -> Category
category Passport = Primary
category BirthCert = Primary
category License = Secondary
category StudentID = Secondary
category _ = Tertiary
    
points :: [Document] -> Int
points docs = 70*primaryCounts + 25*tertiaryCounts + secondaryPoints secondaryDocuments where
    primaryCounts = length $ filter (\x -> category x == Primary) docs
    tertiaryCounts = length $ filter (\x -> category x == Tertiary) docs
    secondaryDocuments = filter (\x -> category x == Secondary) docs
    secondaryPoints :: [Document] -> Int
    secondaryPoints [] = 0
    secondaryPoints (x:xs) = 40 + (25 * length xs)

-- This example is about compound data types
data MonthDayType = MonthDayConstructor Int Int deriving (Show, Eq)

showMonthDay :: MonthDayType -> String
showMonthDay (MonthDayConstructor m d) =
    "the day is " ++ show d ++ " and the month is " ++ show m
    
-- We can have multiple constructors.
data WeekDay = Mon | Tue | Wed | ...

-- recursive and parametric types
divide :: Int -> Int -> Maybe Int
divide x 0 = Nothing
divide x y = Just (x `div` y)

data Maybe a = Just a | Nothing
data List a = Nil | Cons a (List a)
data Natural = Zero | Suc Natural deriving (Show, Eq)

length' :: List a -> Int
length' Nil = 0
length' (Cons x xs) = 1 + length' xs
