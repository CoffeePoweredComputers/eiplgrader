"""
EiplGrader Function Redefinition Examples
==========================================

This file demonstrates Function Redefinition mode - generating code from 
function signatures and assumptions about parameters. This is useful when
students are asked to implement a specific function signature.

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
print("FUNCTION REDEFINITION EXAMPLES")
print("="*80)

# ============================================================================
# Example 1: Basic Python Function Redefinition
# ============================================================================
print("\n1. Basic Python Function Redefinition")
print("-" * 50)

code_generator = CodeGenerator(api_key, language="python")

# In redef mode, student_response is the function name to redefine
result = code_generator.generate_code(
    student_response="calculate_area",  # Function name to implement
    gen_type="redef",
    function_name="calculate_area"
)

generated_code = result["code"]
print(f"Generated code for function redefinition: {generated_code[0]}")

# Test assuming it calculates area of a rectangle
test_cases = [
    {"parameters": {"length": 5, "width": 3}, "expected": 15},
    {"parameters": {"length": 10, "width": 2}, "expected": 20},
    {"parameters": {"length": 1, "width": 1}, "expected": 1},
]

code_tester = CodeTester(
    code=generated_code[0],
    test_cases=test_cases,
    function_name="calculate_area",
    language="python"
)
test_result = code_tester.run_tests()
print(f"Tests passed: {test_result.successes}/{test_result.testsRun}")

# ============================================================================
# Example 2: Python Function with Specific Signature and Assumptions
# ============================================================================
print("\n\n2. Python Function with Specific Signature and Assumptions")
print("-" * 50)

result = code_generator.generate_code(
    student_response="process_grades",  # Function name
    gen_type="redef",
    function_name="process_grades",
    params="grades: List[float]",  # Parameter specification
    assumptions="grades is a list of test scores between 0-100, function should return the average grade"
)

generated_code = result["code"]
print(f"Generated code: {generated_code[0]}")

test_cases = [
    {"parameters": {"grades": [85.0, 90.0, 78.0, 92.0]}, "expected": 86.25},
    {"parameters": {"grades": [100.0, 95.0, 88.0]}, "expected": 94.33333333333333},
    {"parameters": {"grades": [70.0]}, "expected": 70.0},
    {"parameters": {"grades": []}, "expected": 0.0},  # Edge case: empty list
]

code_tester = CodeTester(
    code=generated_code[0],
    test_cases=test_cases,
    function_name="process_grades",
    language="python"
)
test_result = code_tester.run_tests()
print(f"Tests passed: {test_result.successes}/{test_result.testsRun}")

# ============================================================================
# Example 3: JavaScript Function Redefinition
# ============================================================================
print("\n\n3. JavaScript Function Redefinition")
print("-" * 50)

js_generator = CodeGenerator(api_key, language="javascript")

result = js_generator.generate_code(
    student_response="validateEmail",  # Function name
    gen_type="redef",
    function_name="validateEmail"
)

generated_code = result["code"]
print(f"Generated JavaScript code: {generated_code[0]}")

test_cases = [
    {"parameters": {"email": "user@example.com"}, "expected": True},
    {"parameters": {"email": "test.email+tag@domain.co.uk"}, "expected": True},
    {"parameters": {"email": "invalid.email"}, "expected": False},
    {"parameters": {"email": "@domain.com"}, "expected": False},
    {"parameters": {"email": "user@"}, "expected": False},
]

code_tester = CodeTester(
    code=generated_code[0],
    test_cases=test_cases,
    function_name="validateEmail",
    language="javascript"
)
test_result = code_tester.run_tests()
print(f"Tests passed: {test_result.successes}/{test_result.testsRun}")

# ============================================================================
# Example 4: Java Function Redefinition with Complex Signature
# ============================================================================
print("\n\n4. Java Function Redefinition with Complex Signature")
print("-" * 50)

java_generator = CodeGenerator(api_key, language="java")

result = java_generator.generate_code(
    student_response="mergeArrays",  # Function name
    gen_type="redef",
    function_name="mergeArrays",
    assumptions="Takes two sorted integer arrays and merges them into one sorted array"
)

generated_code = result["code"]
print(f"Generated Java code: {generated_code[0]}")

test_cases = [
    {"parameters": {"arr1": [1, 3, 5], "arr2": [2, 4, 6]}, "expected": [1, 2, 3, 4, 5, 6]},
    {"parameters": {"arr1": [1, 5, 9], "arr2": [2, 3, 8]}, "expected": [1, 2, 3, 5, 8, 9]},
    {"parameters": {"arr1": [], "arr2": [1, 2, 3]}, "expected": [1, 2, 3]},
    {"parameters": {"arr1": [1, 2, 3], "arr2": []}, "expected": [1, 2, 3]},
]

code_tester = CodeTester(
    code=generated_code[0],
    test_cases=test_cases,
    function_name="mergeArrays",
    language="java"
)
test_result = code_tester.run_tests()
print(f"Tests passed: {test_result.successes}/{test_result.testsRun}")

# ============================================================================
# Example 5: C Function Redefinition
# ============================================================================
print("\n\n5. C Function Redefinition")
print("-" * 50)

c_generator = CodeGenerator(api_key, language="c")

result = c_generator.generate_code(
    student_response="count_characters",  # Function name
    gen_type="redef",
    function_name="count_characters",
    assumptions="Takes a string and a character, returns the number of times the character appears in the string"
)

generated_code = result["code"]
print(f"Generated C code: {generated_code[0]}")

# Note: C testing requires compilation
print("Note: C code testing requires compilation and careful memory management")

# ============================================================================
# Example 6: Multiple Implementations of Same Function
# ============================================================================
print("\n\n6. Multiple Implementations of Same Function")
print("-" * 50)

result = code_generator.generate_code(
    student_response="is_prime",  # Function name
    gen_type="redef",
    function_name="is_prime",
    assumptions="Takes an integer n and returns True if n is a prime number, False otherwise",
    num_to_gen=3  # Generate 3 different implementations
)

generated_codes = result["code"]
print(f"Generated {len(generated_codes)} different implementations")

test_cases = [
    {"parameters": {"n": 2}, "expected": True},   # Smallest prime
    {"parameters": {"n": 17}, "expected": True},  # Prime number
    {"parameters": {"n": 4}, "expected": False},  # Composite number
    {"parameters": {"n": 1}, "expected": False},  # Edge case: 1 is not prime
    {"parameters": {"n": 0}, "expected": False},  # Edge case: 0 is not prime
    {"parameters": {"n": 29}, "expected": True},  # Another prime
]

# Test all generated variants
for i, code in enumerate(generated_codes):
    print(f"\n--- Testing Implementation {i+1} ---")
    print(f"Code: {code}")
    
    code_tester = CodeTester(
        code=code,
        test_cases=test_cases,
        function_name="is_prime",
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
# Example 7: Go Function Redefinition
# ============================================================================
print("\n\n7. Go Function Redefinition")
print("-" * 50)

go_generator = CodeGenerator(api_key, language="go")

result = go_generator.generate_code(
    student_response="findCommonElements",  # Function name
    gen_type="redef",
    function_name="findCommonElements",
    assumptions="Takes two slices of integers and returns a slice containing elements that appear in both slices"
)

generated_code = result["code"]
print(f"Generated Go code: {generated_code[0]}")

print("\n" + "="*80)
print("FUNCTION REDEFINITION EXAMPLES COMPLETE")
print("="*80)
print("\nKey Function Redefinition Concepts:")
print("1. Use gen_type='redef' to trigger redefinition mode")
print("2. student_response becomes the function name to implement")
print("3. Provide assumptions about parameters and expected behavior")
print("4. Use params to specify function signature details")
print("5. Generate multiple implementations for comparison")
print("6. Test edge cases and boundary conditions")
print("7. Works across all supported programming languages")

