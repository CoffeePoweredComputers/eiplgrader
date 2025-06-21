"""Mock JavaScript code samples for testing executors."""

# Simple arithmetic function
ADD_NUMBERS = """
function addNumbers(a, b) {
    return a + b;
}
"""

# Array processing
SUM_EVEN_NUMBERS = """
function sumEvenNumbers(numbers) {
    return numbers.filter(n => n % 2 === 0).reduce((sum, n) => sum + n, 0);
}
"""

# String manipulation
COUNT_VOWELS = """
function countVowels(str) {
    const vowels = 'aeiouAEIOU';
    let count = 0;
    for (let char of str) {
        if (vowels.includes(char)) {
            count++;
        }
    }
    return count;
}
"""

# Factorial function
FACTORIAL = """
function factorial(n) {
    if (n === 0 || n === 1) {
        return 1;
    }
    return n * factorial(n - 1);
}
"""

# Array sort (in-place)
SORT_ARRAY = """
function sortArray(arr) {
    arr.sort((a, b) => a - b);
}
"""

# Find maximum
FIND_MAX = """
function findMax(numbers) {
    if (numbers.length === 0) {
        return null;
    }
    return Math.max(...numbers);
}
"""

# Boolean check
IS_PALINDROME = """
function isPalindrome(s) {
    return s === s.split('').reverse().join('');
}
"""

# String reversal
REVERSE_STRING = """
function reverseString(s) {
    return s.split('').reverse().join('');
}
"""

# Array manipulation
DOUBLE_ARRAY = """
function doubleArray(arr) {
    return arr.map(x => x * 2);
}
"""

# Object merging
MERGE_OBJECTS = """
function mergeObjects(obj1, obj2) {
    return {...obj1, ...obj2};
}
"""
