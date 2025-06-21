"""Mock Java code samples for testing executors."""

# Simple arithmetic function
ADD_NUMBERS = """
public static int addNumbers(int a, int b) {
    return a + b;
}
"""

# Array processing
SUM_EVEN_NUMBERS = """
public static int sumEvenNumbers(int[] numbers) {
    int sum = 0;
    for (int n : numbers) {
        if (n % 2 == 0) {
            sum += n;
        }
    }
    return sum;
}
"""

# String manipulation
COUNT_VOWELS = """
public static int countVowels(String str) {
    String vowels = "aeiouAEIOU";
    int count = 0;
    for (char c : str.toCharArray()) {
        if (vowels.indexOf(c) != -1) {
            count++;
        }
    }
    return count;
}
"""

# Factorial function
FACTORIAL = """
public static int factorial(int n) {
    if (n == 0 || n == 1) {
        return 1;
    }
    return n * factorial(n - 1);
}
"""

# Array sort (in-place)
SORT_ARRAY = """
import java.util.Arrays;

public static void sortArray(int[] arr) {
    Arrays.sort(arr);
}
"""

# Find maximum
FIND_MAX = """
public static int findMax(int[] numbers) {
    if (numbers.length == 0) {
        return 0;
    }
    int max = numbers[0];
    for (int n : numbers) {
        if (n > max) {
            max = n;
        }
    }
    return max;
}
"""

# Boolean check
IS_PALINDROME = """
public static boolean isPalindrome(String s) {
    return s.equals(new StringBuilder(s).reverse().toString());
}
"""

# String reversal
REVERSE_STRING = """
public static String reverseString(String s) {
    return new StringBuilder(s).reverse().toString();
}
"""

# Array manipulation
DOUBLE_ARRAY = """
public static int[] doubleArray(int[] arr) {
    int[] result = new int[arr.length];
    for (int i = 0; i < arr.length; i++) {
        result[i] = arr[i] * 2;
    }
    return result;
}
"""

# Find second largest
FIND_SECOND_LARGEST = """
public static int findSecondLargest(int[] arr) {
    if (arr.length < 2) {
        return -1;
    }
    int first = Integer.MIN_VALUE, second = Integer.MIN_VALUE;
    for (int n : arr) {
        if (n > first) {
            second = first;
            first = n;
        } else if (n > second && n != first) {
            second = n;
        }
    }
    return second;
}
"""
