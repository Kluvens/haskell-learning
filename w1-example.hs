```
-- comments

{-
multiple line comments

to load file, we use :l example.hs
-}

{-
Given a number n and a string s containing English words, generate
a report that lists the n most common words in the given string s.

Break into steps:
1 Break the input string into words.
2 Convert the words to lowercase.
3 Sort the words.
4 Group adjacent occurrences (runs) of the same word.
5 Sort runs words by length.
6 Take the longest n runs of the sorted list.
7 Generate a report.
-}

module Example where

-- Like C has with its string library, Haskell has some functions which are in packages/libraries outside of what is provided in the base language. 
-- toLower and sort exist outside of the base language and therefore need to be imported.
import Data.Char (toLower)
import Data.List (sort)

-- Task 1:
-- Given a number n and a string s containing English words
-- (and nothing else), generate a report that lists the n
-- most common words in the given string s.

-- 1. break the input string into words
-- The following line is declaring a function
-- NameOfFunction :: Domain(Input type) -> Codomain(Output type)
breakIntoWords :: String -> [String]
-- The following line is the body of a function
-- NameofFunction = AnotherFunction 
-- or 
-- NameOfFunction Argument = Result
breakIntoWords = words

-- 2. convert the words to lowercase
convertToLowercase :: [String] -> [String]
convertToLowercase = map (map toLower)

-- 3. sort the words
sortWords :: [String] -> [String]
sortWords = sort

-- 4. group adjacent occurrences (runs)
type Run = [String]
groupAdjacentRuns :: [String] -> [Run]
groupAdjacentRuns = undefined

-- 5. sort by length of the run
sortByLength :: [Run] -> [Run]
sortByLength = undefined

-- 6. take the n longest runs
takeLongestRuns :: Int -> [Run] -> [Run]
takeLongestRuns = undefined

-- 7. generate report
generateReport :: [Run] -> String
generateReport = undefined

-- then put it all together
program :: Int -> String -> String
program n s =
  generateReport (takeLongestRuns n (sortByLength (
        groupAdjacentRuns (sortWords (convertToLowercase (breakIntoWords s)))
  )))
```
