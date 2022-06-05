```
-- comments

{-
multiple line comments

to load file, we use :l example.hs
-}

module Example where

import Data.Char (toLower)
import Data.List (sort)

-- Task 1:
-- Given a number n and a string s containing English words
-- (and nothing else), generate a report that lists the n
-- most common words in the given string s.

-- 1. break the input string into words
breakIntoWords :: String -> [String]
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
