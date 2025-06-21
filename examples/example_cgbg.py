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
print("\nNOTE: Type annotations are now optional for Python, JavaScript, and Go!")
print("      Type annotations are still required for C, C++, Java, and Haskell.")
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
    gen_type="cgbg",  # Explicitly specify CGBG mode
    temperature=0.0   # Deterministic output
)

generated_code = result["code"]
print(f"Generated code: {generated_code[0]}")

# For Python, types are optional and will be inferred!
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
    function_name="sum_even_numbers",
    temperature=0.0
)

generated_code = result["code"]
print(f"Generated code: {generated_code[0]}")

# Types are inferred from the values!
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
    function_name="countVowels",
    temperature=0.0
)

generated_code = result["code"]
print(f"Generated JavaScript code: {generated_code[0]}")

# JavaScript also supports type inference!
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
print("\n\n4. Java CGBG - Array Operations (REQUIRES TYPE ANNOTATIONS)")
print("-" * 50)
print("Note: Java is statically typed and requires explicit type annotations")

java_generator = CodeGenerator(api_key, language="java")

result = java_generator.generate_code(
    student_response="that finds the second largest number in an array of integers",
    function_name="findSecondLargest",
    temperature=0.0
)

generated_code = result["code"]
print(f"Generated Java code: {generated_code[0]}")

test_cases = [
    {"parameters": {"arr": [1, 5, 3, 9, 2]}, "parameter_types": {"arr": "int[]"}, "expected": 5, "expected_type": "int"},
    {"parameters": {"arr": [10, 10, 5]}, "parameter_types": {"arr": "int[]"}, "expected": 5, "expected_type": "int"},      # Handle duplicates
    {"parameters": {"arr": [1, 2]}, "parameter_types": {"arr": "int[]"}, "expected": 1, "expected_type": "int"},           # Minimum case
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
# Example 5: C CGBG - Algorithm Implementation
# ============================================================================
print("\n\n5. C CGBG - Algorithm Implementation (REQUIRES TYPE ANNOTATIONS)")
print("-" * 50)
print("Note: C is statically typed and requires explicit type annotations")

c_generator = CodeGenerator(api_key, language="c")

result = c_generator.generate_code(
    student_response="that implements bubble sort to sort an array of integers in ascending order",
    function_name="bubbleSort",
    temperature=0.0
)

generated_code = result["code"]
print(f"Generated C code: {generated_code[0]}")

test_cases = [
    {
        "parameters": {
            "arr": [3, 1, 4, 1, 5], 
            "n": 5
        },
        "parameter_types": {
            "arr": "int*",
            "n": "int"
        },
        "expected": [1, 1, 3, 4, 5],
        "expected_type": "int*"
    },  # In-place sort
    {
        "parameters": {
            "arr": [-15, -42, -317, -2, -100], 
            "n": 5
        },
        "parameter_types": {
            "arr": "int*",
            "n": "int"
        },
        "expected": [-317, -100, -42, -15, -2],
        "expected_type": "int*"
    }
]

code_tester = CodeTester(
    code=generated_code[0],
    test_cases=test_cases,
    function_name="bubbleSort",
    language="c",
    inplace="1"  # Bubble sort modifies array in-place
)
test_result = code_tester.run_tests()
print(f"Tests passed: {test_result.successes}/{test_result.testsRun}")

# ============================================================================
# Example 6: C++ CGBG - STL Vector Operations
# ============================================================================
print("\n\n6. C++ CGBG - STL Vector Operations (REQUIRES TYPE ANNOTATIONS)")
print("-" * 50)
print("Note: C++ is statically typed and requires explicit type annotations")

cpp_generator = CodeGenerator(api_key, language="cpp")

result = cpp_generator.generate_code(
    student_response="that takes a vector of integers and returns a new vector containing only the unique elements in sorted order using STL algorithms",
    function_name="getUniqueElements",
    temperature=0.0
)

generated_code = result["code"]
print(f"Generated C++ code: {generated_code[0]}")

test_cases = [
    {"parameters": {"input": [3, 1, 4, 1, 5, 3, 2]}, "parameter_types": {"input": "std::vector<int>"}, "expected": [1, 2, 3, 4, 5], "expected_type": "std::vector<int>"},
    {"parameters": {"input": [5, 5, 5]}, "parameter_types": {"input": "std::vector<int>"}, "expected": [5], "expected_type": "std::vector<int>"},
    {"parameters": {"input": [1, 2, 3]}, "parameter_types": {"input": "std::vector<int>"}, "expected": [1, 2, 3], "expected_type": "std::vector<int>"},
]

code_tester = CodeTester(
    code=generated_code[0],
    test_cases=test_cases,
    function_name="getUniqueElements",
    language="cpp"
)
test_result = code_tester.run_tests()
print(f"Tests passed: {test_result.successes}/{test_result.testsRun}")

# ============================================================================
# Example 7: Go CGBG - String Processing (NO TYPES REQUIRED!)
# ============================================================================
print("\n\n7. Go CGBG - String Processing (TYPES OPTIONAL!)")
print("-" * 50)
print("Note: Go now supports type inference from values like Python/JS!")

go_generator = CodeGenerator(api_key, language="go")

result = go_generator.generate_code(
    student_response="that checks if a string is a palindrome (reads the same forwards and backwards)",
    function_name="isPalindrome",
    temperature=0.0
)

generated_code = result["code"]
print(f"Generated Go code: {generated_code[0]}")

# Go now supports type inference! No need to specify types.
test_cases = [
    {"parameters": {"s": "racecar"}, "expected": True},
    {"parameters": {"s": "hello"}, "expected": False},
    {"parameters": {"s": "a"}, "expected": True},
    {"parameters": {"s": ""}, "expected": True},
]

code_tester = CodeTester(
    code=generated_code[0],
    test_cases=test_cases,
    function_name="isPalindrome",
    language="go"
)
test_result = code_tester.run_tests()
print(f"Tests passed: {test_result.successes}/{test_result.testsRun}")

# Show errors if any
if test_result.failures > 0 or test_result.errors > 0:
    print("Go test failures:")
    for result in test_result.test_results:
        if not result["pass"]:
            print(f"  Test: {result.get('function_call', 'Unknown')}")
            print(f"  Expected: {result['expected_output']}")
            print(f"  Got: {result['actual_output']}")
            if result['error']:
                print(f"  Error: {result['error']}")
            print()

# ============================================================================
# Example 8: Haskell CGBG - Functional Programming
# ============================================================================
print("\n\n8. Haskell CGBG - Functional Programming (REQUIRES TYPE ANNOTATIONS)")
print("-" * 50)
print("Note: Haskell is statically typed and requires explicit Haskell type annotations")

haskell_generator = CodeGenerator(api_key, language="haskell")

result = haskell_generator.generate_code(
    student_response="that takes a list of integers and returns the list with all elements that are prime doubled",
    function_name="doubleAll",
    temperature=0.0
)

generated_code = result["code"]
print(f"Generated Haskell code: {generated_code[0]}")

# Haskell requires explicit types with proper Haskell type names
test_cases = [
    {"parameters": {"xs": [1, 2, 3]}, "parameter_types": {"xs": "[Int]"}, "expected": [2, 4, 6], "expected_type": "[Int]"},
    {"parameters": {"xs": [0, -1, 5]}, "parameter_types": {"xs": "[Int]"}, "expected": [0, -2, 10], "expected_type": "[Int]"},
    {"parameters": {"xs": []}, "parameter_types": {"xs": "[Int]"}, "expected": [], "expected_type": "[Int]"},
]

code_tester = CodeTester(
    code=generated_code[0],
    test_cases=test_cases,
    function_name="doubleAll",
    language="haskell"
)
test_result = code_tester.run_tests()
print(f"Tests passed: {test_result.successes}/{test_result.testsRun}")

# ============================================================================
# Example 9: Multiple Code Generation for Robustness
# ============================================================================
print("\n\n9. Multiple Code Generation (Python) for Robustness Testing")
print("-" * 50)

result = code_generator.generate_code(
    student_response="that calculates the factorial of a non-negative integer",
    function_name="factorial",
    num_to_gen=3,  # Generate 3 different implementations
    temperature=0.0
)

generated_codes = result["code"]
print(f"Generated {len(generated_codes)} different implementations")

# Python with type inference - clean and simple!
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
print("\nLanguage Testing Status:")
print("✅ Python: Fully functional (type inference supported)")
print("✅ JavaScript: Fully functional (type inference supported)")
print("✅ Go: Fully functional (type inference supported)")
print("✅ Java: Fully functional (types required)")
print("✅ C++: Fully functional (types required)")
print("✅ C: Fully functional (types required)")
print("✅ Haskell: Fully functional (types required)")
print("\nType Requirements Summary:")
print("- JSON-capable languages (Python, JS, Go): Types optional, inferred from values")
print("- Non-JSON languages (C, C++, Java, Haskell): Exact native types required")
