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
