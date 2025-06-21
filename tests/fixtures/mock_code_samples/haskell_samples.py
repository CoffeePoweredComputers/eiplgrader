"""Mock Haskell code samples for testing executors."""

# Simple arithmetic function
ADD_NUMBERS = """
addNumbers :: Int -> Int -> Int
addNumbers a b = a + b
"""

# List processing
SUM_EVEN_NUMBERS = """
sumEvenNumbers :: [Int] -> Int
sumEvenNumbers numbers = sum [n | n <- numbers, even n]
"""

# String manipulation
COUNT_VOWELS = """
countVowels :: String -> Int
countVowels str = length [c | c <- str, c `elem` "aeiouAEIOU"]
"""

# Factorial function
FACTORIAL = """
factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n - 1)
"""

# List sort
SORT_LIST = """
import Data.List

sortList :: [Int] -> [Int]
sortList = sort
"""

# Find maximum
FIND_MAX = """
findMax :: [Int] -> Int
findMax [] = 0
findMax xs = maximum xs
"""

# Boolean check
IS_PALINDROME = """
isPalindrome :: String -> Bool
isPalindrome s = s == reverse s
"""

# String reversal
REVERSE_STRING = """
reverseString :: String -> String
reverseString = reverse
"""

# List manipulation
DOUBLE_ALL = """
doubleAll :: [Int] -> [Int]
doubleAll = map (*2)
"""

# Filter and map
SQUARE_EVENS = """
squareEvens :: [Int] -> [Int]
squareEvens xs = map (^2) (filter even xs)
"""

# Fibonacci
FIBONACCI = """
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)
"""

# List concatenation
CONCAT_LISTS = """
concatLists :: [Int] -> [Int] -> [Int]
concatLists xs ys = xs ++ ys
"""

# Mathematical operations
CALCULATE_AVERAGE = """
calculateAverage :: Double -> Double -> Double
calculateAverage a b = (a + b) / 2.0
"""

# String operations
JOIN_WORDS = """
import Data.List

joinWords :: [String] -> String
joinWords = intercalate " "
"""

# Search function
LINEAR_SEARCH = """
linearSearch :: [Int] -> Int -> Int
linearSearch [] _ = -1
linearSearch (x:xs) target 
    | x == target = 0
    | otherwise = 
        let result = linearSearch xs target
        in if result == -1 then -1 else result + 1
"""

# Complex types function
FORMAT_INFO = """
formatInfo :: String -> Int -> Bool -> Double -> String
formatInfo name age isActive salary = 
    name ++ "," ++ show age ++ "," ++ show isActive ++ "," ++ show salary
"""

# Nested list operations
FLATTEN_NESTED = """
flattenNested :: [[Int]] -> [Int]
flattenNested = concat
"""

# Count characters
COUNT_CHARACTERS = """
countCharacters :: String -> Int
countCharacters = length
"""

# Average of list
AVERAGE_LIST = """
averageList :: [Double] -> Double
averageList [] = 0.0
averageList xs = sum xs / fromIntegral (length xs)
"""

# Filter positive numbers
FILTER_POSITIVE = """
filterPositive :: [Int] -> [Int]
filterPositive = filter (> 0)
"""

# Multiple return values (using tuple)
DIVMOD_OPERATION = """
divmodOperation :: Int -> Int -> (Int, Int)
divmodOperation a b = divMod a b
"""
