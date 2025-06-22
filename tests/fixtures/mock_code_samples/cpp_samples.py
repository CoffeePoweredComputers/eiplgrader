"""Mock C++ code samples for testing executors."""

# Simple arithmetic function
ADD_NUMBERS = """
int addNumbers(int a, int b) {
    return a + b;
}
"""

# Vector processing
SUM_EVEN_NUMBERS = """
#include <vector>

int sumEvenNumbers(std::vector<int> numbers) {
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
#include <string>

int countVowels(std::string str) {
    std::string vowels = "aeiouAEIOU";
    int count = 0;
    for (char c : str) {
        if (vowels.find(c) != std::string::npos) {
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

# Vector sort (in-place)
SORT_VECTOR = """
#include <vector>
#include <algorithm>

void sortVector(std::vector<int>& arr) {
    std::sort(arr.begin(), arr.end());
}
"""

# Find maximum
FIND_MAX = """
#include <vector>
#include <algorithm>

int findMax(std::vector<int> numbers) {
    if (numbers.empty()) {
        return 0;
    }
    return *std::max_element(numbers.begin(), numbers.end());
}
"""

# Boolean check
IS_PALINDROME = """
#include <string>
#include <algorithm>

bool isPalindrome(std::string s) {
    std::string reversed = s;
    std::reverse(reversed.begin(), reversed.end());
    return s == reversed;
}
"""

# String reversal
REVERSE_STRING = """
#include <string>
#include <algorithm>

std::string reverseString(std::string s) {
    std::reverse(s.begin(), s.end());
    return s;
}
"""

# Vector manipulation
DOUBLE_VECTOR = """
#include <vector>

std::vector<int> doubleVector(std::vector<int> arr) {
    std::vector<int> result;
    for (int n : arr) {
        result.push_back(n * 2);
    }
    return result;
}
"""

# Unique elements (using STL)
GET_UNIQUE_ELEMENTS = """
#include <vector>
#include <set>
#include <algorithm>

std::vector<int> getUniqueElements(std::vector<int> input) {
    std::set<int> unique_set(input.begin(), input.end());
    std::vector<int> result(unique_set.begin(), unique_set.end());
    return result;
}
"""

# Mathematical operations
CALCULATE_AVERAGE = """
double calculateAverage(double a, double b) {
    return (a + b) / 2.0;
}
"""

# String operations
JOIN_STRINGS = """
#include <vector>
#include <string>

std::string joinStrings(std::vector<std::string> words) {
    std::string result;
    for (size_t i = 0; i < words.size(); i++) {
        if (i > 0) {
            result += " ";
        }
        result += words[i];
    }
    return result;
}
"""

# Search function
LINEAR_SEARCH = """
#include <vector>

int linearSearch(std::vector<int> arr, int target) {
    for (size_t i = 0; i < arr.size(); i++) {
        if (arr[i] == target) {
            return static_cast<int>(i);
        }
    }
    return -1;
}
"""

# Complex types function
FORMAT_INFO = """
#include <string>
#include <sstream>
#include <iomanip>

std::string formatInfo(std::string name, int age, bool isActive, double salary) {
    std::ostringstream oss;
    oss << name << "," << age << "," << (isActive ? "true" : "false") 
        << "," << std::fixed << std::setprecision(2) << salary;
    return oss.str();
}
"""

# Nested vector operations
FLATTEN_NESTED = """
#include <vector>

std::vector<int> flattenNested(std::vector<std::vector<int>> nested) {
    std::vector<int> result;
    for (const auto& subVec : nested) {
        result.insert(result.end(), subVec.begin(), subVec.end());
    }
    return result;
}
"""

# Count characters
COUNT_CHARACTERS = """
#include <string>

int countCharacters(std::string s) {
    return static_cast<int>(s.length());
}
"""

# Double array processing
MULTIPLY_DOUBLES = """
#include <vector>

std::vector<double> multiplyDoubles(std::vector<double> numbers, double multiplier) {
    std::vector<double> result;
    for (double n : numbers) {
        result.push_back(n * multiplier);
    }
    return result;
}
"""
