"""Mock Python code samples for testing executors."""

# Simple arithmetic function
ADD_NUMBERS = """
def add_numbers(a, b):
    return a + b
"""

# List processing
SUM_EVEN_NUMBERS = """
def sum_even_numbers(numbers):
    return sum(n for n in numbers if n % 2 == 0)
"""

# String manipulation
COUNT_VOWELS = """
def count_vowels(s):
    vowels = 'aeiouAEIOU'
    return sum(1 for char in s if char in vowels)
"""

# Factorial function
FACTORIAL = """
def factorial(n):
    if n == 0 or n == 1:
        return 1
    return n * factorial(n - 1)
"""

# List sort (in-place)
SORT_LIST = """
def sort_list(arr):
    arr.sort()
"""

# Find maximum
FIND_MAX = """
def find_max(numbers):
    if not numbers:
        return None
    return max(numbers)
"""

# Boolean check
IS_PALINDROME = """
def is_palindrome(s):
    return s == s[::-1]
"""

# Multiple return values
DIVMOD_OPERATION = """
def divmod_operation(a, b):
    return divmod(a, b)
"""

# String reversal
REVERSE_STRING = """
def reverse_string(s):
    return s[::-1]
"""

# Complex types
MERGE_DICTS = """
def merge_dicts(dict1, dict2):
    result = dict1.copy()
    result.update(dict2)
    return result
"""
