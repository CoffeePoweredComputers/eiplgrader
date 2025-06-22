"""Mock C code samples for testing executors."""

# Simple arithmetic function
ADD_NUMBERS = """
int addNumbers(int a, int b) {
    return a + b;
}
"""

# Array processing
SUM_EVEN_NUMBERS = """
int sumEvenNumbers(int* numbers, int size) {
    int sum = 0;
    for (int i = 0; i < size; i++) {
        if (numbers[i] % 2 == 0) {
            sum += numbers[i];
        }
    }
    return sum;
}
"""

# String manipulation
COUNT_VOWELS = """
#include <string.h>

int countVowels(char* str) {
    char* vowels = "aeiouAEIOU";
    int count = 0;
    for (int i = 0; str[i] != '\\0'; i++) {
        if (strchr(vowels, str[i]) != NULL) {
            count++;
        }
    }
    return count;
}
"""

# Factorial function
FACTORIAL = """
int factorial(int n) {
    if (n == 0 || n == 1) {
        return 1;
    }
    return n * factorial(n - 1);
}
"""

# Array sort (bubble sort)
BUBBLE_SORT = """
void bubbleSort(int* arr, int n) {
    for (int i = 0; i < n - 1; i++) {
        for (int j = 0; j < n - i - 1; j++) {
            if (arr[j] > arr[j + 1]) {
                int temp = arr[j];
                arr[j] = arr[j + 1];
                arr[j + 1] = temp;
            }
        }
    }
}
"""

# Find maximum
FIND_MAX = """
int findMax(int* numbers, int size) {
    if (size == 0) {
        return 0;
    }
    int max = numbers[0];
    for (int i = 1; i < size; i++) {
        if (numbers[i] > max) {
            max = numbers[i];
        }
    }
    return max;
}
"""

# Boolean check (returns int in C)
IS_PALINDROME = """
#include <string.h>

int isPalindrome(char* s) {
    int len = strlen(s);
    for (int i = 0; i < len / 2; i++) {
        if (s[i] != s[len - 1 - i]) {
            return 0;
        }
    }
    return 1;
}
"""

# String reversal (in-place)
REVERSE_STRING = """
#include <string.h>

void reverseString(char* s) {
    int len = strlen(s);
    for (int i = 0; i < len / 2; i++) {
        char temp = s[i];
        s[i] = s[len - 1 - i];
        s[len - 1 - i] = temp;
    }
}
"""

# Array manipulation
DOUBLE_ARRAY = """
void doubleArray(int* arr, int size) {
    for (int i = 0; i < size; i++) {
        arr[i] *= 2;
    }
}
"""

# Linear search
LINEAR_SEARCH = """
int linearSearch(int* arr, int size, int target) {
    for (int i = 0; i < size; i++) {
        if (arr[i] == target) {
            return i;
        }
    }
    return -1;
}
"""
