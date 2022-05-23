# haskell-learning

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
