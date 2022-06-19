# haskell-learning

## Basic Haskell

**How haskell works**
```
average :: Float -> Float -> Float
average a b  = (a + b) / 2.0
```
```average``` is the name of the function, and takes two parameters ```a``` and ```b```. Doing the process on the right hand side, the return value is the thing on the right hand side. ```=`` in haskell means that you are defining the thing on the RHS to be the value of the LHS.

### Values
Values are terms, such as ```5``` (an integer number), ```"Hello World!"``` (a character string), and ```3.141``` (a floating point number). Values are processed by functions. For example, addition ```+``` takes two numbers and produces a new number, namely the sum of the two input values; ```++``` takes two strings and produces a new string by concatenating the two input strings; ```length``` takes a string and produces a number, namely the length of the input string. In other words, functions, such as +, ++, and length, are mappings from input values to output values.

### Types
Values can be grouped into sets with similar properties. For example, values which represent integers, strings, booleans, or floating point numbers. We call such sets of values types. Some examples of types which are present in most programming languages are the following:
- ```Int``` = {…, -3, -2, -1, 0, 1, 2, 3, …}
- ```Float``` = {…, -1232.42342, …, 1.0, 3.141, …}
- ```Double``` =  {… , -1232.42342, …, 1.0, 3.141, …}
- ```Char``` = {…, 'a', 'A', 'b', 'B', …'1', …, '@', '#', …}
- ```String(but acturally String is [Char])``` = {"", "a", "b", …, "Hi" ,"3423#", …}
- ```Bool``` = {False, True}
- ```Tuple``` = {(3, 5)...} where ```("hello", "world")``` is of type ```([Char], [Char])```
- ```List``` -> ```"hello world"``` is ```[Char]```; ```[1, 2, 3]``` is a list of integers(```[Int]```). ```[(3, 'A'), (4, 'H')]``` means ```[(Int, Char)]```.

We write ```1 :: Int``` or ```"Hello" :: String``` to indicate that the values ```1``` and ```Hello``` have the the type ```Int``` and ```String```, respectively. Hence, ```1 :: Int``` can be read as “```1``` has type ```Int```”.

Types essentially describe sets of values with similar properties and help us to distinguish correct from erroneous programs. For example, the expression ```1 + "abc"``` contains a type error, because the string value ```"abc"``` does not match the type of argument expected by ```+```. We call an expression without type errors well typed. Programming languages that enforce a rigorous type discipline are often called strongly typed languages. Type errors should generally be regarded as a hint by the programming system, telling us that part of our program do not make sense —the program is inconsistent— and they are one of the means by which the programming system helps us write better programs.

```Lists``` are singly-linked lists in Haskell. The empty list is written as ```[]``` and a list node is written as x : xs. The value x is called the head and rest of the list xs is called the tail. Thus, ```"hi!" == ['h', 'i', '!'] == 'h':('i':('!':[])) == 'h':'i':'!':[]```

### Data constructor
data constructors are essentially functions that produce values of particular variety of some type.
``` haskell
data MonthDayType = MonthDayConstructor Int Int deriving(Show, Eq)

showMonthDay :: MonthDayType -> String
showMonthDay (MonthDayConstructor m d) = "The day is " + show d + " and the month is " + show m 
```

### Types in design
Maybe type is very useful when you don't want to return something
``` haskell
divide :: Int -> Int -> Maybe Int
divide x 0 = Nothing
divide x y = Just (x `div` y)
```
``` haskell
data Contact = C Name (Maybe Address) (Maybe Email)
```
is changed to:
``` haskell
data ContactDetails = EmailOnly Email
                    | PostOnly Address
                    | Both Address Email
data Contact = C Name ContactDetails
```
More about Maybe, Just and Nothing:
``` haskell
incFirst :: [Int] -> Maybe Int
incFirst [] = Nothing
incFirst (x:xs) = Just (x+1)

inconFirst :: [Int] -> Maybe [Int]
inconFirst [] = Nothing
inconFirst (x:xs) = Just ((x+1):xs)
```

### Functions
We have seen that, by applying functions to values, we can compute new values; but, how can we define new functions? Let us start with a simple example and write a function that increments a number by the value ```1```; let us call this function ```inc```. So, the application ```inc 10``` should produce ```11```, ```inc 11``` should produce ```12```, and so forth — in other words, for any number ```x```, the expression inc ```x```, should yield ```x + 1```.

In ```inc x = x + 1```, ```inc x``` is called the head and ```x + 1``` is called the body.

A bit nesting: ```inc (inc 5)``` => ```inc (5 + 1)``` => ```inc 6``` => ```6 + 1``` => ```7```

In haskell, there are technically no "multi-argument" functions. Every function that appears to be multi-argument on the surface is actually just **curried** so that they return a sequence of single-argument functions, that in turn may return similar such functions.

### Partial functions
A partial function is a function not defined for all possible inputs. e.g. head, tail, (!!), division.

Partial functions are to be avoided, because they cause your program to crash if undefined cases are encountered.

To eliminate partiality, we must

1. enlarge the codomain, usually with a Maybe type:
``` haskell
safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead [] = Nothing

addFirstElements :: [Int] -> [Int] -> Maybe Int
addFirstElements xs ys = case safeHead xs of
  Nothing -> Nothing
  Just x -> case safeHead ys of 
    Nothing -> Nothing
    Just y -> Just (x + y)
```

2. or we must constrain the domain to be more specific
```
data NonEmpty a
  = One a 
  | Cons a (NonEmpty)
  deriving (Show, Eq)

toList :: NonEmpty a -> [a]
toList (One x) = [x]
toList (ConsNE x xs) = x: toList xs

safeHead' :: NonEmpty a -> a
safeHead' (One x) = [x]
safeHead' (ConsNE x _) = x
```

### Useful functions
A useful function is ```map```, which given a function, applies it to each element of a list: 
- The type of ```map``` is ```map :: (a -> b) -> [a] -> [b]``` which takes a function and a list then return a new list.
- ```map not [True, False, True]``` implies ```[False, True, False]```
- ```map square [3, -2, 4]``` implies ```[9, 4, 6]```
- ```map (\x -> x + 1) [1, 5]``` implies ```[2, 6]```

### Type signatures
Functions map input values to output values, for example, ```inc``` maps integers to integers. Thus, we denote the type of ```inc``` as ```Int -> Int```.
```
inc :: Int -> Int     -- type signature
inc x = x + 1         -- function equation
```

### Multiple arguments
we can turn into a function with two arguments as follows:
```
average :: Float -> Float -> Float
average a b  = (a + b) / 2.0
```
So, we have
```average 3.0 4.0``` => ```(3.0 + 4.0)/2.0``` => ```3.5```

In haskell, ```average :: Float -> Float -> Float``` means the same thing as ```average :: Float -> (Float -> Float)```

### Frequently used type classes and overloaded functions
Typeclass ```Show```:
- functions: ```show :: Show a => a -> String```: convert the given value into a string.
- member types: almost all predefined types, excluding function types.

Typeclass ```Eq```:
- funcions: ```(==), (/=) :: Eq a => a -> a -> Bool```: equality and inequality.
- member types: almost all predefined types, excluding function types.

Typeclass ```Ord```:
- functions: ```(<), (>), (<=), (>=) :: Ord a => a -> a-> Bool```: less than, greater than, less or equal, greater or equal
- member types: almost all predefined types, excluding function types.
- all types in ```Ord``` are already in ```Eq```, so if you are using both ```==``` and ```<``` on a value, it is sufficient to require it to be in ```Ord```.

Typeclasss ```Num```:
- functions: ```(+), (-), (*) :: Num a => a -> a -> a```: arithmetic operations.
- member types: ```Float, Double, Int, Integer```

Typeclass ```Integral```:
- functions: ```div, mod :: Integral a => a -> a -> a```: division.
- member types: ```Int```(fixed precision), ```Integer```(arbitrary precision)

Typeclass ```Fractional```:
- functions: ```(/) :: Fractional a => a -> a -> a```: division.
- member types: ```Float, Double```

Typeclass Floating:
- functions: ```sin, cos, tan, exp, sqrt,… :: Floating a => a -> a```: trigonometric and other functions.
- member types: ```Float, Double```

User-defined typeclass:
``` haskell
calss ShortShow a where
  shortShow :: a -> Char
  
instance ShortShow Bool where
  shortShow True = 'T'
  shortShow False = 'F'
  
instance Show a => ShortShow (Maybe a) where
  shortShow x = head (show x)
```

## Semigroup
A semigroup is a pair of a set S and an operation • : S → S → S where the operation • is associative.

Associativity is defined as, for all a, b, c: (a • (b • c)) = ((a • b) • c)

## Monoid
A monoid is a semigroup (S, •) equipped with a special identity element z : S such that x • z = x and z • y = y for all x, y.
``` haskell
class (Semigroup a) => Monoid a where
  mempty :: a
```

## Newtypes
A newtype declaration is much like a data declaration except that there can be only one constructor and it must take exactly one argument:
``` haskell
newtype Score = S Integer

instance Semigroup Score where
  S x <> S y = S (x + y)
  
instance Monoid Score where
  mempty = S 0
```

## Recursive types
``` haskell
data FamTree i
  = NoKnownParents i (FamTree i)
  | KnownMother i (FamTree i)
  | KnownFather i (FamTree i)
  | KnownMotherFather i (FamTree i) (FamTree i)
  deriving (Show, Eq)

carlo :: FamTree String
carlo = NoKnownParents "Carlo"
Louis :: FamTree String
Louis = KnownFather "Louis" carlo
hortense :: FamTree String
hortense = NoKnownParents "Hortense"
napoleon3 :: FamTree String
napoleon3 = KnownMotherFather "Napoleon3" hortense louis

numAncestors :: FamTree i -> Int
numAncestors (NoKnownParents a) = 0
numAncestors (KnownMother a motherTree) = 1 + numAncestors motherTree
numAncestors (KnownFather a fatherTree) = 1 + numAncestors fatherTree
numAncestors (KnownMotherFather a m f) = 2 + numAncestors m + numAncestors f
```

## Coding examples
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
