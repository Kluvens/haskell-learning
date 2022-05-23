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
modEx = mod 5 4 (or modEx = 5 'mod' 4)
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
