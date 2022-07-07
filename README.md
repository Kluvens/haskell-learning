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

- Monoids: a simple type class
- Type classes are somewhat like interfaces
- Most important: the laws they satisfy
``` haskell
class (Semigroup a) => Monoid a where
  mempty :: a
```

Declaring Monoids:
``` haskell
data Sum = Sum Int deriving (Eq, Show)
instance Semigroup Sum where
  Sum x <> Sum y = Sum (x + y)        -- bullet (semigroup operation)
instance Monoid Sum where
  mappend = (<>)                      -- bullet (same as Semigroup operation)
  mempty = Sum 0                      -- identity
```
``` haskell
expo :: (Monoid g) => g -> Int -> g
expo x 0 = mempty
expo x n = x <> expo x (n-1) where
  (<>) = mappend
```
``` haskell
fexpo :: (Monoid g) => g -> Int -> g
fexpo x 0 = mempty
fexpo x n
  | even n = y <> y
  | otherwise = x <> fexpo x (n - 1) where
  y = fexpo x (n `div` 2)
```

Monoid algorithms:
- write once: need to write only one implementation
- test/prove once: establish correctness using the laws
- reuse: hundreds of monoids occur in real-world code

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
louis :: FamTree String
louis = KnownFather "Louis" carlo
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

## MapReduce with somewhat parallel version
``` haskell
import Control.Parallel (par)
import Control.Parallel.Strategies (using, parMap, rpar)

mapReduce' :: (Monoid g) =>
              (i -> [p]) -> (p -> g) -> i -> g
mapReduce' split f input = mapResult `par` reduceResult
  where
  mapResult = parMap rpar f (split input)
  reduceResult = mconcat mapResult `using` rpar
```
