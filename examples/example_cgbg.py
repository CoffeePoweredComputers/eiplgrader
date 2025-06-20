"""
EiplGrader CGBG (Code Generation Based Grading) Examples
========================================================

This file demonstrates Code Generation Based Grading (CGBG) - generating code 
from natural language descriptions across multiple programming languages.

Setup: Set your OPENAI_API_KEY environment variable or create a .env file with:
OPENAI_API_KEY=your_api_key_here
"""

# Set the package to be the package in the parent directory
import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from eiplgrader.codegen import CodeGenerator
from eiplgrader.tester import CodeTester
import dotenv

# Load environment variables
dotenv.load_dotenv()
api_key = os.getenv("OPENAI_API_KEY")

if not api_key:
    print("ERROR: Please set OPENAI_API_KEY environment variable")
    exit(1)

print("="*80)
print("CGBG (CODE GENERATION BASED GRADING) EXAMPLES")
print("="*80)

# ============================================================================
# Example 1: Basic Python CGBG
# ============================================================================
print("\n1. Basic Python CGBG - Simple Math Function")
print("-" * 50)

code_generator = CodeGenerator(api_key, language="python")

result = code_generator.generate_code(
    student_response="that adds two numbers and returns the result",
    function_name="add_numbers",
    gen_type="cgbg"  # Explicitly specify CGBG mode
)

generated_code = result["code"]
print(f"Generated code: {generated_code[0]}")

test_cases = [
    {"parameters": {"a": 1, "b": 2}, "expected": 3},
    {"parameters": {"a": 5, "b": 7}, "expected": 12},
    {"parameters": {"a": -1, "b": 1}, "expected": 0},
    {"parameters": {"a": 0, "b": 0}, "expected": 0},
]

code_tester = CodeTester(
    code=generated_code[0], 
    test_cases=test_cases,
    function_name="add_numbers",
    language="python"
)
test_result = code_tester.run_tests()
print(f"Tests passed: {test_result.successes}/{test_result.testsRun}")

# ============================================================================
# Example 2: Python CGBG - List Processing
# ============================================================================
print("\n\n2. Python CGBG - List Processing")
print("-" * 50)

result = code_generator.generate_code(
    student_response="that takes a list of numbers and returns the sum of all even numbers",
    function_name="sum_even_numbers"
)

generated_code = result["code"]
print(f"Generated code: {generated_code[0]}")

test_cases = [
    {"parameters": {"numbers": [1, 2, 3, 4, 5, 6]}, "expected": 12},  # 2+4+6
    {"parameters": {"numbers": [1, 3, 5]}, "expected": 0},           # No even numbers
    {"parameters": {"numbers": [2, 4, 6, 8]}, "expected": 20},       # All even
    {"parameters": {"numbers": []}, "expected": 0},                  # Empty list
]

code_tester = CodeTester(
    code=generated_code[0],
    test_cases=test_cases,
    function_name="sum_even_numbers",
    language="python"
)
test_result = code_tester.run_tests()
print(f"Tests passed: {test_result.successes}/{test_result.testsRun}")

# ============================================================================
# Example 3: JavaScript CGBG - String Manipulation
# ============================================================================
print("\n\n3. JavaScript CGBG - String Manipulation")
print("-" * 50)

js_generator = CodeGenerator(api_key, language="javascript")

result = js_generator.generate_code(
    student_response="that counts the number of vowels in a string (a, e, i, o, u)",
    function_name="countVowels"
)

generated_code = result["code"]
print(f"Generated JavaScript code: {generated_code[0]}")

test_cases = [
    {"parameters": {"str": "hello"}, "expected": 2},      # e, o
    {"parameters": {"str": "programming"}, "expected": 3}, # o, a, i
    {"parameters": {"str": "xyz"}, "expected": 0},        # No vowels
    {"parameters": {"str": "AEIOU"}, "expected": 5},      # All vowels uppercase
]

code_tester = CodeTester(
    code=generated_code[0],
    test_cases=test_cases,
    function_name="countVowels",
    language="javascript"
)
test_result = code_tester.run_tests()
print(f"Tests passed: {test_result.successes}/{test_result.testsRun}")

# ============================================================================
# Example 4: Java CGBG - Array Operations
# ============================================================================
print("\n\n4. Java CGBG - Array Operations")
print("-" * 50)

java_generator = CodeGenerator(api_key, language="java")

result = java_generator.generate_code(
    student_response="that finds the second largest number in an array of integers",
    function_name="findSecondLargest"
)

generated_code = result["code"]
print(f"Generated Java code: {generated_code[0]}")

test_cases = [
    {"parameters": {"arr": [1, 5, 3, 9, 2]}, "expected": 5},
    {"parameters": {"arr": [10, 10, 5]}, "expected": 5},      # Handle duplicates
    {"parameters": {"arr": [1, 2]}, "expected": 1},           # Minimum case
]

code_tester = CodeTester(
    code=generated_code[0],
    test_cases=test_cases,
    function_name="findSecondLargest",
    language="java"
)
test_result = code_tester.run_tests()
print(f"Tests passed: {test_result.successes}/{test_result.testsRun}")

# ============================================================================
# Example 5: C++ CGBG - Algorithm Implementation
# ============================================================================
print("\n\n5. C++ CGBG - Algorithm Implementation")
print("-" * 50)

cpp_generator = CodeGenerator(api_key, language="cpp")

result = cpp_generator.generate_code(
    student_response="that implements bubble sort to sort an array of integers in ascending order",
    function_name="bubbleSort"
)

generated_code = result["code"]
print(f"Generated C++ code: {generated_code[0]}")

# Note: C++ testing would require compilation
print("Note: C++ code requires compilation before testing")

# ============================================================================
# Example 6: Go CGBG - Concurrent Processing
# ============================================================================
print("\n\n6. Go CGBG - String Processing")
print("-" * 50)

go_generator = CodeGenerator(api_key, language="go")

result = go_generator.generate_code(
    student_response="that checks if a string is a palindrome (reads the same forwards and backwards)",
    function_name="isPalindrome"
)

generated_code = result["code"]
print(f"Generated Go code: {generated_code[0]}")

# ============================================================================
# Example 7: Multiple Code Generation for Robustness
# ============================================================================
print("\n\n7. Multiple Code Generation for Robustness Testing")
print("-" * 50)

result = code_generator.generate_code(
    student_response="that calculates the factorial of a non-negative integer",
    function_name="factorial",
    num_to_gen=3  # Generate 3 different implementations
)

generated_codes = result["code"]
print(f"Generated {len(generated_codes)} different implementations")

test_cases = [
    {"parameters": {"n": 0}, "expected": 1},   # Edge case: 0! = 1
    {"parameters": {"n": 1}, "expected": 1},   # Base case
    {"parameters": {"n": 5}, "expected": 120}, # 5! = 5*4*3*2*1
    {"parameters": {"n": 3}, "expected": 6},   # 3! = 3*2*1
]

# Test all generated variants
for i, code in enumerate(generated_codes):
    print(f"\n--- Testing Implementation {i+1} ---")
    print(f"Code: {code}")
    
    code_tester = CodeTester(
        code=code,
        test_cases=test_cases,
        function_name="factorial",
        language="python"
    )
    test_result = code_tester.run_tests()
    print(f"Tests passed: {test_result.successes}/{test_result.testsRun}")
    
    if test_result.failures > 0 or test_result.errors > 0:
        print("Failed tests:")
        for result in test_result.test_results:
            if not result["pass"]:
                print(f"  {result['function_call']}: expected {result['expected_output']}, got {result['actual_output']}")

# ============================================================================
# Example 8: Complex Problem - Fibonacci Sequence
# ============================================================================
print("\n\n8. Complex Problem - Fibonacci Sequence")
print("-" * 50)

result = code_generator.generate_code(
    student_response="that calculates the nth number in the Fibonacci sequence where F(0)=0, F(1)=1, and F(n)=F(n-1)+F(n-2)",
    function_name="fibonacci"
)

generated_code = result["code"]
print(f"Generated code: {generated_code[0]}")

test_cases = [
    {"parameters": {"n": 0}, "expected": 0},   # F(0) = 0
    {"parameters": {"n": 1}, "expected": 1},   # F(1) = 1
    {"parameters": {"n": 2}, "expected": 1},   # F(2) = 1
    {"parameters": {"n": 5}, "expected": 5},   # F(5) = 5
    {"parameters": {"n": 8}, "expected": 21},  # F(8) = 21
]

code_tester = CodeTester(
    code=generated_code[0],
    test_cases=test_cases,
    function_name="fibonacci",
    language="python"
)
test_result = code_tester.run_tests()
print(f"Tests passed: {test_result.successes}/{test_result.testsRun}")

print("\n" + "="*80)
print("CGBG EXAMPLES COMPLETE")
print("="*80)
print("\nKey CGBG Concepts:")
print("1. Generate code from natural language descriptions")
print("2. Specify clear, detailed problem descriptions")
print("3. Use realistic test cases that cover edge cases")
print("4. Generate multiple implementations for robustness")
print("5. Test across different programming languages")
print("6. Handle both simple and complex algorithmic problems")
