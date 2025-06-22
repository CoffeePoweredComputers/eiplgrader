"""Mock Go code samples for testing executors."""

# Simple arithmetic function
ADD_NUMBERS = """
func addNumbers(a, b int) int {
    return a + b
}
"""

# Slice processing
SUM_EVEN_NUMBERS = """
func sumEvenNumbers(numbers []int) int {
    sum := 0
    for _, n := range numbers {
        if n%2 == 0 {
            sum += n
        }
    }
    return sum
}
"""

# String manipulation
COUNT_VOWELS = """
func countVowels(s string) int {
    vowels := "aeiouAEIOU"
    count := 0
    for _, char := range s {
        for _, vowel := range vowels {
            if char == vowel {
                count++
                break
            }
        }
    }
    return count
}
"""

# Factorial function
FACTORIAL = """
func factorial(n int) int {
    if n == 0 || n == 1 {
        return 1
    }
    return n * factorial(n-1)
}
"""

# Slice sort (using sort package)
SORT_SLICE = """
import "sort"

func sortSlice(arr []int) {
    sort.Ints(arr)
}
"""

# Find maximum
FIND_MAX = """
func findMax(numbers []int) int {
    if len(numbers) == 0 {
        return 0
    }
    max := numbers[0]
    for _, n := range numbers {
        if n > max {
            max = n
        }
    }
    return max
}
"""

# Boolean check
IS_PALINDROME = """
func isPalindrome(s string) bool {
    runes := []rune(s)
    n := len(runes)
    for i := 0; i < n/2; i++ {
        if runes[i] != runes[n-1-i] {
            return false
        }
    }
    return true
}
"""

# String reversal
REVERSE_STRING = """
func reverseString(s string) string {
    runes := []rune(s)
    for i, j := 0, len(runes)-1; i < j; i, j = i+1, j-1 {
        runes[i], runes[j] = runes[j], runes[i]
    }
    return string(runes)
}
"""

# Slice manipulation
DOUBLE_SLICE = """
func doubleSlice(arr []int) []int {
    result := make([]int, len(arr))
    for i, v := range arr {
        result[i] = v * 2
    }
    return result
}
"""

# Float operations
AVERAGE = """
func average(numbers []float64) float64 {
    if len(numbers) == 0 {
        return 0
    }
    sum := 0.0
    for _, n := range numbers {
        sum += n
    }
    return sum / float64(len(numbers))
}
"""

# Mathematical operations
CALCULATE_AVERAGE = """
func calculateAverage(a, b float64) float64 {
    return (a + b) / 2.0
}
"""

# String operations
JOIN_WORDS = """
import "strings"

func joinWords(words []string) string {
    return strings.Join(words, " ")
}
"""

# Search function
LINEAR_SEARCH = """
func linearSearch(arr []int, target int) int {
    for i, v := range arr {
        if v == target {
            return i
        }
    }
    return -1
}
"""

# Complex types function
FORMAT_INFO = """
import "fmt"

func formatInfo(name string, age int, isActive bool, salary float64) string {
    return fmt.Sprintf("%s,%d,%t,%.2f", name, age, isActive, salary)
}
"""

# Nested slice operations
FLATTEN_NESTED = """
func flattenNested(nested [][]int) []int {
    var result []int
    for _, subSlice := range nested {
        result = append(result, subSlice...)
    }
    return result
}
"""

# Count characters
COUNT_CHARACTERS = """
func countCharacters(s string) int {
    return len([]rune(s))
}
"""
