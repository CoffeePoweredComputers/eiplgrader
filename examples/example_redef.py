"""
EiplGrader Function Redefinition Examples
==========================================

This file demonstrates Function Redefinition mode - generating code from 
function signatures and assumptions about parameters. This is useful when
students are asked to implement a specific function signature.

Setup: Set your API key environment variable or create a .env file.
See .env.example for all supported providers and their API keys.
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

# Choose your provider and get the appropriate API key
# Options: "openai", "meta", "ollama"
client_type = "openai"  # Change this to your preferred provider
model = "gpt-4o"  # Default model for OpenAI

# Get the appropriate API key based on the provider
if client_type == "openai":
    api_key = os.getenv("OPENAI_API_KEY")
    if not api_key:
        print("ERROR: Please set OPENAI_API_KEY environment variable")
        print("See .env.example for setup instructions")
        exit(1)
elif client_type == "meta":
    api_key = os.getenv("META_API_KEY")
    model = "Llama-4-Maverick-17B-128E-Instruct-FP8"  # Default model for Meta
    if not api_key:
        print("ERROR: Please set META_API_KEY environment variable")
        print("See .env.example for setup instructions")
        exit(1)
elif client_type == "ollama":
    api_key = "not-needed"  # Ollama doesn't need an API key
    model = "codellama:instruct"  # Default model for Ollama
else:
    print(f"ERROR: Unsupported client_type: {client_type}")
    exit(1)

print("="*80)
print("FUNCTION REDEFINITION EXAMPLES")
print("="*80)

# ============================================================================
# Example 1: Basic Python Function Redefinition
# ============================================================================
print("\n1. Basic Python Function Redefinition")
print("-" * 50)

code_generator = CodeGenerator(api_key, client_type=client_type, language="python")

# In redef mode, student_response is the function name to redefine
result = code_generator.generate_code(
    gen_type="redef",
    model=model,
    student_response="calculate_area",
    params="length: int, width: int",
    assumptions="This function takes two integers which are positive."
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
# Example 2: JavaScript Function Redefinition
# ============================================================================
print("\n\n2. JavaScript Function Redefinition")
print("-" * 50)

js_generator = CodeGenerator(api_key, client_type=client_type, language="javascript")

result = js_generator.generate_code(
    student_response="validateEmail",  # Function name
    model=model,
    gen_type="redef",
    params="x: string",  
    assumptions="This takes a string containing only alphanumeric characters"
)

generated_code = result["code"]
print(f"Generated JavaScript code: {generated_code[0]}")

test_cases = [
    {"parameters": {"email": "user@example.com"}, "parameter_types": {"email": "string"}, "expected": True, "expected_type": "boolean"},
    {"parameters": {"email": "test.email+tag@domain.co.uk"}, "parameter_types": {"email": "string"}, "expected": True, "expected_type": "boolean"},
    {"parameters": {"email": "invalid.email"}, "parameter_types": {"email": "string"}, "expected": False, "expected_type": "boolean"},
    {"parameters": {"email": "@domain.com"}, "parameter_types": {"email": "string"}, "expected": False, "expected_type": "boolean"},
    {"parameters": {"email": "user@"}, "parameter_types": {"email": "string"}, "expected": False, "expected_type": "boolean"},
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
# Example 3: Java Function Redefinition with Complex Signature
# ============================================================================
print("\n\n3. Java Function Redefinition with Complex Signature")
print("-" * 50)

java_generator = CodeGenerator(api_key, client_type=client_type, language="java")

result = java_generator.generate_code(
    student_response="mergeArrays",  # Function name
    model=model,
    gen_type="redef",
    params="arr1: int[], arr2: int[]",  
    assumptions="Takes two sorted integer arrays and returns an array."
)

generated_code = result["code"]
print(f"Generated Java code: {generated_code[0]}")

test_cases = [
    {"parameters": {"arr1": [1, 3, 5], "arr2": [2, 4, 6]}, "parameter_types": {"arr1": "int[]", "arr2": "int[]"}, "expected": [1, 2, 3, 4, 5, 6], "expected_type": "int[]"},
    {"parameters": {"arr1": [1, 5, 9], "arr2": [2, 3, 8]}, "parameter_types": {"arr1": "int[]", "arr2": "int[]"}, "expected": [1, 2, 3, 5, 8, 9], "expected_type": "int[]"},
    {"parameters": {"arr1": [], "arr2": [1, 2, 3]}, "parameter_types": {"arr1": "int[]", "arr2": "int[]"}, "expected": [1, 2, 3], "expected_type": "int[]"},
    {"parameters": {"arr1": [1, 2, 3], "arr2": []}, "parameter_types": {"arr1": "int[]", "arr2": "int[]"}, "expected": [1, 2, 3], "expected_type": "int[]"},
]

code_tester = CodeTester(
    code=generated_code[0],
    test_cases=test_cases,
    function_name="mergeArrays",
    language="java",
    inplace="2"
)
test_result = code_tester.run_tests()
print(f"Tests passed: {test_result.successes}/{test_result.testsRun}")

# ============================================================================
# Example 4: C Function Redefinition
# ============================================================================
print("\n\n4. C Function Redefinition")
print("-" * 50)

c_generator = CodeGenerator(api_key, client_type=client_type, language="c")

test_cases = [
        {"parameters": {"str": "hello world", "c": "o"}, "parameter_types": {"str": "char*", "c": "char"}, "expected": 2, "expected_type": "int"},
        {"parameters": {"str": "test string", "c": "t"}, "parameter_types": {"str": "char*", "c": "char"}, "expected": 3, "expected_type": "int"},
        {"parameters": {"str": "aaaaaa", "c": "a"}, "parameter_types": {"str": "char*", "c": "char"}, "expected": 6, "expected_type": "int"},  # All same character
        ]

result = c_generator.generate_code(
    student_response="count_characters",  # Function name
    model=model,
    gen_type="redef",
    params="char* str, char c",  
    assumptions="Takes a string and a character"
)

generated_code = result["code"]
print(f"Generated C code: {generated_code[0]}")

# Note: C testing requires compilation
code_tester = CodeTester(
    code=generated_code[0],
    test_cases=test_cases,
    function_name="count_characters",
    language="c"
)
test_result = code_tester.run_tests()
print(f"Tests passed: {test_result.successes}/{test_result.testsRun}")


# ============================================================================
# Example 5: Multiple Implementations of Same Function
# ============================================================================
print("\n\n5. Multiple Implementations of Same Function")
print("-" * 50)

result = code_generator.generate_code(
    student_response="is_prime",  # Function name
    model=model,
    gen_type="redef",
    params="n: int",  
    assumptions="Takes an integer n that is positive in value",
    num_to_gen=3  # Generate 3 different implementations
)

generated_codes = result["code"]
print(f"Generated {len(generated_codes)} different implementations")

test_cases = [
    {"parameters": {"n": 2}, "parameter_types": {"n": "int"}, "expected": True, "expected_type": "bool"},   # Smallest prime
    {"parameters": {"n": 17}, "parameter_types": {"n": "int"}, "expected": True, "expected_type": "bool"},  # Prime number
    {"parameters": {"n": 4}, "parameter_types": {"n": "int"}, "expected": False, "expected_type": "bool"},  # Composite number
    {"parameters": {"n": 1}, "parameter_types": {"n": "int"}, "expected": False, "expected_type": "bool"},  # Edge case: 1 is not prime
    {"parameters": {"n": 0}, "parameter_types": {"n": "int"}, "expected": False, "expected_type": "bool"},  # Edge case: 0 is not prime
    {"parameters": {"n": 29}, "parameter_types": {"n": "int"}, "expected": True, "expected_type": "bool"},  # Another prime
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
# Example 6: Go Function Redefinition
# ============================================================================
print("\n\n6. Go Function Redefinition")
print("-" * 50)

go_generator = CodeGenerator(api_key, client_type=client_type, language="go")

test_cases = [
        {"parameters": {"arr1": [1, 2, 3], "arr2": [2, 3, 4]}, "parameter_types": {"arr1": "[]int", "arr2": "[]int"}, "expected": [2, 3], "expected_type": "[]int"},
        {"parameters": {"arr1": [5, 6, 7], "arr2": [6, 7, 8]}, "parameter_types": {"arr1": "[]int", "arr2": "[]int"}, "expected": [6, 7], "expected_type": "[]int"},
        {"parameters": {"arr1": [], "arr2": [1, 2, 3]}, "parameter_types": {"arr1": "[]int", "arr2": "[]int"}, "expected": [], "expected_type": "[]int"},  # Empty first array
        {"parameters": {"arr1": [1, 2, 3], "arr2": []}, "parameter_types": {"arr1": "[]int", "arr2": "[]int"}, "expected": [], "expected_type": "[]int"},  # Empty second array
]

result = go_generator.generate_code(
    student_response="findCommonElements",  # Function name
    model=model,
    gen_type="redef",
    params="arr1 []int, arr2 []int",  
    assumptions="Each of the arrays contains integers. One or both may be empty"
)

generated_code = result["code"]
print(f"Generated Go code: {generated_code[0]}")

code_tester = CodeTester(
    code=generated_code[0],
    test_cases=test_cases,
    function_name="findCommonElements",
    language="go"
)
test_result = code_tester.run_tests()
print(f"Tests passed: {test_result.successes}/{test_result.testsRun}")


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

