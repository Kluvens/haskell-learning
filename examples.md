```f ::  Int -> Bool``` (f is the function name, :: is of type, Int is the domain, Bool is the codomain)

```f x = (x > 0)``` (x is the input, x > 0 is the output)

Basic types: ```Int, Double, Bool, Char, Tuple```

Define constant:
``` haskell
always5 :: Int
always5 = 5
```

Sum numbers:
``` haskell
sumOfNums = sum [1..1000]
```

Basic expressions:
``` haskell
addEx = 5 + 4
subEx = 5 - 4
mulEx = 5 * 4
divEx = 5 / 4
modEx = mod 5 4 (or modEx = 5 `mod` 4)
```

Square root:
``` haskell
num9 = 9 :: Int
sqrt9 = sqrt (fromIntegral num9)
```

Creat a list:
``` haskell
favNums = 2 : 7 : 21 : 66 :[]
```

Concatenate lists:
``` haskell
morePrime = [3, 5, 7] ++ [11, 13, 17]
```

MultList:
``` haskell
multList = [[3, 5, 7], [11, 13, 17]]
```

Reverse a list:
``` haskell
revPrime = reverse morePrime
```

Check if list is empty:
``` haskell
isListEmpty = null morePrime
```

Get second element of the list (indexing):
``` haskell
secondPrime = morePrime !! 1
```

Get the first value of a list:
``` haskell
firstPrime = head morePrime
```

Get the last value of a list:
``` haskell
lastPrime = last morePrime
```

Take first three elements:
``` haskell
first3Primes = take 3 morePrime
```

Remove first three elements:
``` haskell
removedPrimes = drop 3 morePrime
```

Check if an element is in the list:
``` haskell
is7InList = 7 `elem` morePrime
```

Get maximum value:
``` haskell
maxPrime = maximum morePrime
```

Get minimum value:
``` haskell
minPrime = minimum morePrime
```

Get the product of a list:
``` haskell
prodPrime = product morePrime
```

Generate a list from 0 to 10:
``` haskell
zeroToTen = [0..10]
```

Generate list of even numbers:
``` haskell
evenList = [2,4..20]
```

Generate a list of letters:
``` haskell
letterList = ['A', 'C'..'Z']
```

Generate a list of ten 2s:
``` haskell
many2s = take 10 (repeat 2)
or
many2s = replicate 10 2
```

Generate a cycle list:
``` haskell
cycleList = take 10 (cycle [1,2,32,4,5])
-- should produce [1,2,3,4,5,1,2,3,4,5]
```

List times by 2:
``` haskell
listTimes2 = [x * 2 | x <- [1..10]]
```

List times by 2 and filtering:
``` haskell
listTimes2 = [x * 2 | x <- [1..10], x * 2 <= 15]
```

Check if divisible by 9 and 13:
``` haskell
divisBy9N13 = [x | x <- [1..500], x `mod` 13 == 0, x `mod` 9 == 0]
```

Sort a list:
``` haskell
sortedList = sort [9,1,8,3,4,7,6]
```

Zip two lists:
``` haskell
sumOfLists = zipWith (+) [1,2,3,4,5] [6,7,8,9,10]
-- should produce [7,9,11,13,15]
```

Filter:
``` haskell
listBiggerThen5 = filter (>5) morePrimes
```

Take while:
``` haskell
evensUpTo20 = takeWhile (<= 20) [2,4..]
```

Multiply of list from left:
``` haskell
multOfList = foldl (*) 1 [2,3,4,5]
-- l means from left
```

Generate powers list:
``` haskell
pow3List = [3^n | n <- [1..10]]
```

MultTable:
``` haskell
multTable = [[x * y | y <- [1..10]] | x <- [1..10]]
```

Create a tuple pair:
``` haskell
bobSmith = ("Bob Smith", 52)
```

Get the name and age from the tuple above:
``` haskell
bobName = fst bobSmith
bobAge = snd bobSmith
```

Zip to tuples:
``` haskell
names = ["Bob", "Mary", "Tom"]
addresses = ["123 Main", "234 North", "567 South"]

namesAddresses = zip names addresses
```

Functions:
``` haskell
main = do 
  putStrln "What's your name"
  name <- getline
  putStrln("Hello" ++ name)
```

Type declaration:
``` haskell
addMe :: Int -> Int -> Int
-- funcName param1 param2 = operations (returned value)
addMe x y = x + y

addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (x, y) (x2, y2) = (x + x2, y + y2)
```

Recursion:
``` haskell
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)
```

Check if is odd:
``` haskell
isOdd :: Int -> Bool
isOdd n
  | n `mod` 2 == 0 = False
  | otherwise = True
```

What grade example:
``` haskell
whatGrade :: Int -> String
whatGrade age
  | (age >= 5) && (age <= 6) = "Kindergarten"
  | (age > 6) && (age <= 10) = "Elementary School"
  | (age > 10) && (age <= 14) = "Middle School"
  | (age > 14) && (age <= 18) = "High School"
  | otherwise = "Go to college"
```

Where statement:
``` haskell
batAvgRating :: Double -> Double -> String
batAvgRating hits atBats
  | avg <= 0.200 = "Terrible Batting Average"
  | avg <= 0.500 = "Average Player"
  | avg <= 0.280 = "You are doing pretty good"
  | otherwise = "You are a Superstar"
  where avg = hits / atBats
```

Get list items:
``` haskell
getListItems :: [Int] -> String
getListItems [] = "Your list is empty"
getListItems (x:[]) = "Your list starts with" ++ show x
getListItems (x:y:[]) = "Your list contains" ++ show x ++ "and" ++ show y
getListItems (x:xs) = "The 1st item is " ++ show x ++ "and the rest are" show xs
```

As pattern:
``` haskell
getFirstItem :: String -> String
getFirstItem [] = "Empty String"
getFirstItem all@(x:xs) = "The first letter in " ++ all ++ "is " ++ [x]
```

Higher order functions:
``` haskell
times4 :: Int -> Int
times4 x - x * 4
listTimes4 = map times4 [1,2,3,4,5]

multBy4 :: [Int] -> [Int]
multBy4 [] = []
multBy4 (x:xs) = times4 : multBy4 xs
```

More on x:xs:
``` haskell
areStringEq :: [Char] -> [Char] -> Bool
areStringEq [] [] = True
areStringEq (x:xs) (y:ys) = x == y && areStringEq xs ys
areStringEq _ _ = False
``` 

Passing a function into a function:
``` haskell
times4 :: Int -> Int
times4 x = x * 4
doMult :: (Int -> Int) -> Int
doMult func = func 3
num3Times4 = doMult times4
```

Lambda:
``` haskell
dbl1To10 = map (\x -> x * 2) [1..10]
```

If:
``` haskell
doubleEvenNumber y = 
  if (y `mod` 2 /= 0) 
    then y
    else y * 2
    
getClass :: Int -> String
getClass n = case n of 
  5 -> "Go to Kindergarten"
  6 -> "Go to elementary school"
  _ -> "Go away"
```

Modules:
``` haskell
module SampFunctions (getClass, doubleEvenNumbers) where
-- all my functions listed
import SampFunctions
```

Enumerations:
``` haskell
data BaseballPlayer = Pitcher | Catcher | Infielder | Outfield deriving Show
barryBonds :: BaseballPlayer -> Bool
barryBonds Outfield = True
barryInOf = print(barryBonds Outfield)
```

Customer data types:
``` haskell
data Customer = Customer String String Double devring Show
toSmith :: Customer
toSmith = Customer "Tom Smith" "123 Main" 20.50
getBalance :: Customer -> Double
getBalance (Customer _ _ b) = b
```

Polymorphic Type:
``` haskell
data Shape = Circle Float Float Float | Rectangle Float Float Float Float
  deriving Show
area :: Shape -> Float 
area (Circle _ _ r) = pi * r ^ 2
area (Rectangle x y x2 y2) = (abs (x2 - x)) * (abs (y2 - y))
-- equivalent to (abs $ x2 - x) * (abs $ y2 - y)
sumValue = putStrln (Show (1+2))
sumValue = putStrln . show $ 1 + 2

areaOfCircle = area (Circle 50 60 20)
areaOfRect = area $ Rectangle 100 10 100 100
```

Type classes:
``` haskell
data Employee = Employee {
                          name :: String,
                          position :: String,
                          idNum :: Int
                          } deriving (Eq, Show)
samSmith = Employee {name = "Sam Smith", position = "Manager", idNum = 1000}
pamMax = Employee {name = "Pam Max", position = "Employee", idNum = 1001}

isSamPam = samSmith == pamMax

samSmithData = show samSmith
-- print the information out
```

Type instance:
``` haskell
data ShirtSize S | M | L
instance Eq ShirtSize where
  S == S = True
  M == M = True
  L == L = True
  _ == _ = False
  
instance Show ShirtSize where
  show S = "Small"
  show M = "Medium"
  show L = "Large"
  
smallAvail = S `elem` [S, M, L]
theSize = show S
```

Custom Typeclass:
``` haskell
class MyEq a where
  areEqual :: a -> a -> Bool
  
instance MyEq ShirtSize where
  areEqual S S = True
  areEqual M M = True
  areEqual L L = True
  areEqual _ _ = False

newSize = areEqual M M
```

File I/O:
``` haskell
sayHello = do
  putStrln "What's your name"
  name <- getLine
  putStrLn $ "Hello " ++ name
  
writeToFile = do
  theFile <- openFile "test.txt" WriteMode
  hPutStrLn theFile ("Random line of text")
  hClose theFile
  
readFromFile = do
  theFile2 <- openFile "text.txt" ReadMode
  contents <- hGetContents theFile2
  hClose theFile2
```

Fibonacci sequence
``` haskell
fib = 1 : 1 : [a + b | (a, b) <- zip fib (tail fib)]
```

