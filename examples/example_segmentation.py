"""
EiplGrader Segmentation Examples
================================

This file demonstrates Segmentation functionality - mapping student explanations
to specific parts of generated code. This is useful for understanding how
students conceptualize problem-solving steps.

Setup: Set your OPENAI_API_KEY environment variable or create a .env file with:
OPENAI_API_KEY=your_api_key_here

Note: Segmentation requires a few-shot learning file for best results.
"""

# Set the package to be the package in the parent directory
import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from eiplgrader.codegen import CodeGenerator
from eiplgrader.tester import CodeTester
import dotenv
import json

# Load environment variables
dotenv.load_dotenv()
api_key = os.getenv("OPENAI_API_KEY")

if not api_key:
    print("ERROR: Please set OPENAI_API_KEY environment variable")
    exit(1)

print("="*80)
print("SEGMENTATION EXAMPLES")
print("="*80)

# ============================================================================
# Setup: Create a sample few-shot segmentation file
# ============================================================================
print("\n0. Setting up Segmentation Few-Shot Examples")
print("-" * 50)

# Create a sample few-shot file for segmentation
segmentation_examples = {
    "examples": [
        {
            "explanation": "First I initialize a sum variable to zero, then I loop through each number in the list and add it to the sum, finally I return the sum",
            "code": "def sum_list(numbers):\n    total = 0\n    for num in numbers:\n        total += num\n    return total",
            "segmentation": [
                {
                    "explanation_portion": "First I initialize a sum variable to zero",
                    "code": "total = 0"
                },
                {
                    "explanation_portion": "then I loop through each number in the list and add it to the sum",
                    "code": "for num in numbers:\n        total += num"
                },
                {
                    "explanation_portion": "finally I return the sum",
                    "code": "return total"
                }
            ]
        },
        {
            "explanation": "I need to check each character in the string. If it's a vowel, I increment my counter. At the end I return the counter value.",
            "code": "def count_vowels(text):\n    vowels = 'aeiou'\n    count = 0\n    for char in text.lower():\n        if char in vowels:\n            count += 1\n    return count",
            "segmentation": [
                {
                    "explanation_portion": "I need to check each character in the string",
                    "code": "for char in text.lower():"
                },
                {
                    "explanation_portion": "If it's a vowel, I increment my counter",
                    "code": "if char in vowels:\n            count += 1"
                },
                {
                    "explanation_portion": "At the end I return the counter value",
                    "code": "return count"
                }
            ]
        }
    ]
}

# Save the few-shot file
few_shot_file = "/tmp/segmentation_few_shot.json"
with open(few_shot_file, 'w') as f:
    json.dump(segmentation_examples, f, indent=2)

print(f"Created few-shot file: {few_shot_file}")
print("This file contains examples of how explanations map to code segments")

# ============================================================================
# Example 1: Basic Segmentation with Simple Function
# ============================================================================
print("\n\n1. Basic Segmentation - List Sum Function")
print("-" * 50)

code_generator = CodeGenerator(api_key, language="python")

# Note: Segmentation only works with CGBG (gen_type="cgbg")
result = code_generator.generate_code(
    student_response="that takes a list of numbers and returns their sum. First I'll set up a total variable, then loop through all numbers adding each to the total, and finally return the result.",
    function_name="sum_numbers",
    gen_type="cgbg",  # Segmentation only works with CGBG
    segmentation_few_shot_file=few_shot_file,
    num_to_gen=1
)

generated_code = result["code"]
segmentation = result.get("segmentation", [])

print(f"Generated code: {generated_code[0]}")
print("\nSegmentation results:")
if segmentation:
    for i, seg in enumerate(segmentation[0].get("groups", [])):
        print(f"  Segment {i+1}:")
        print(f"    Explanation: {seg['explanation_portion']}")
        print(f"    Code: {seg['code']}")
else:
    print("  No segmentation results available")

# Test the generated code
test_cases = [
    {"parameters": {"numbers": [1, 2, 3, 4, 5]}, "parameter_types": {"numbers": "List[int]"}, "expected": 15, "expected_type": "int"},
    {"parameters": {"numbers": [10, 20, 30]}, "parameter_types": {"numbers": "List[int]"}, "expected": 60, "expected_type": "int"},
    {"parameters": {"numbers": []}, "parameter_types": {"numbers": "List[int]"}, "expected": 0, "expected_type": "int"},
    {"parameters": {"numbers": [-1, 1, -2, 2]}, "parameter_types": {"numbers": "List[int]"}, "expected": 0, "expected_type": "int"},
]

code_tester = CodeTester(
    code=generated_code[0],
    test_cases=test_cases,
    function_name="sum_numbers",
    language="python"
)
test_result = code_tester.run_tests()
print(f"\nTests passed: {test_result.successes}/{test_result.testsRun}")

# ============================================================================
# Example 2: Complex Segmentation with Detailed Explanation
# ============================================================================
print("\n\n2. Complex Segmentation - Prime Number Checker")
print("-" * 50)

result = code_generator.generate_code(
    student_response="""that checks if a number is prime. My approach is: first I handle the special cases where the number is less than 2 (not prime), then I check if the number is 2 (which is prime), then for other numbers I loop from 2 to the square root of the number checking if any number divides evenly (if so, it's not prime), and if no divisors are found then it's prime.""",
    function_name="is_prime_detailed",
    gen_type="cgbg",
    segmentation_few_shot_file=few_shot_file,
    num_to_gen=1
)

generated_code = result["code"]
segmentation = result.get("segmentation", [])

print(f"Generated code: {generated_code[0]}")
print("\nDetailed segmentation results:")
if segmentation:
    for i, seg in enumerate(segmentation[0].get("groups", [])):
        print(f"  Segment {i+1}:")
        print(f"    Explanation: '{seg['explanation_portion']}'")
        print(f"    Mapped Code: '{seg['code']}'")
        print()
else:
    print("  No segmentation results available")

# Test the generated code
test_cases = [
    {"parameters": {"n": 2}, "parameter_types": {"n": "int"}, "expected": True, "expected_type": "bool"},
    {"parameters": {"n": 17}, "parameter_types": {"n": "int"}, "expected": True, "expected_type": "bool"},
    {"parameters": {"n": 4}, "parameter_types": {"n": "int"}, "expected": False, "expected_type": "bool"},
    {"parameters": {"n": 1}, "parameter_types": {"n": "int"}, "expected": False, "expected_type": "bool"},
    {"parameters": {"n": 0}, "parameter_types": {"n": "int"}, "expected": False, "expected_type": "bool"},
]

code_tester = CodeTester(
    code=generated_code[0],
    test_cases=test_cases,
    function_name="is_prime_detailed",
    language="python"
)
test_result = code_tester.run_tests()
print(f"Tests passed: {test_result.successes}/{test_result.testsRun}")

# ============================================================================
# Example 3: Multiple Code Generation with Segmentation
# ============================================================================
print("\n\n3. Multiple Code Generation with Segmentation")
print("-" * 50)

result = code_generator.generate_code(
    student_response="""that finds the maximum value in a list. I start by setting the first element as the current maximum, then I go through each remaining element comparing it to the current max, and if I find a larger element I update the maximum, finally I return the maximum value found.""",
    function_name="find_maximum",
    gen_type="cgbg",
    segmentation_few_shot_file=few_shot_file,
    num_to_gen=2  # Generate 2 implementations with segmentation
)

generated_codes = result["code"]
segmentations = result.get("segmentation", [])

for impl_idx, (code, seg) in enumerate(zip(generated_codes, segmentations)):
    print(f"\n--- Implementation {impl_idx + 1} ---")
    print(f"Generated code: {code}")
    print("Segmentation:")
    
    if seg and "groups" in seg:
        for i, segment in enumerate(seg["groups"]):
            print(f"  {i+1}. '{segment['explanation_portion']}'")
            print(f"     -> '{segment['code']}'")
    else:
        print("  No segmentation available for this implementation")

# Test both implementations
test_cases = [
    {"parameters": {"lst": [1, 5, 3, 9, 2]}, "parameter_types": {"lst": "List[int]"}, "expected": 9, "expected_type": "int"},
    {"parameters": {"lst": [-1, -5, -2]}, "parameter_types": {"lst": "List[int]"}, "expected": -1, "expected_type": "int"},
    {"parameters": {"lst": [42]}, "parameter_types": {"lst": "List[int]"}, "expected": 42, "expected_type": "int"},
]

print(f"\nTesting all {len(generated_codes)} implementations:")
for i, code in enumerate(generated_codes):
    print(f"\nTesting implementation {i+1}:")
    code_tester = CodeTester(
        code=code,
        test_cases=test_cases,
        function_name="find_maximum",
        language="python"
    )
    test_result = code_tester.run_tests()
    print(f"Tests passed: {test_result.successes}/{test_result.testsRun}")

# ============================================================================
# Example 4: Segmentation with String Processing
# ============================================================================
print("\n\n4. Segmentation with String Processing")
print("-" * 50)

result = code_generator.generate_code(
    student_response="""that counts words in a sentence. My plan is to first remove extra spaces and convert to lowercase for consistency, then split the sentence into individual words using spaces as separators, then count how many words we got, and return that count.""",
    function_name="count_words",
    gen_type="cgbg",
    segmentation_few_shot_file=few_shot_file
)

generated_code = result["code"]
segmentation = result.get("segmentation", [])

print(f"Generated code: {generated_code[0]}")
print("\nString processing segmentation:")
if segmentation:
    for i, seg in enumerate(segmentation[0].get("groups", [])):
        print(f"  Step {i+1}: {seg['explanation_portion']}")
        print(f"  Code: {seg['code']}")
        print()

# Test the word counting function
test_cases = [
    {"parameters": {"sentence": "Hello world this is a test"}, "parameter_types": {"sentence": "str"}, "expected": 6, "expected_type": "int"},
    {"parameters": {"sentence": "  Extra   spaces   everywhere  "}, "parameter_types": {"sentence": "str"}, "expected": 3, "expected_type": "int"},
    {"parameters": {"sentence": "Single"}, "parameter_types": {"sentence": "str"}, "expected": 1, "expected_type": "int"},
    {"parameters": {"sentence": ""}, "parameter_types": {"sentence": "str"}, "expected": 0, "expected_type": "int"},
]

code_tester = CodeTester(
    code=generated_code[0],
    test_cases=test_cases,
    function_name="count_words",
    language="python"
)
test_result = code_tester.run_tests()
print(f"Tests passed: {test_result.successes}/{test_result.testsRun}")

# ============================================================================
# Example 5: Advanced Segmentation Analysis
# ============================================================================
print("\n\n5. Advanced Segmentation Analysis")
print("-" * 50)

result = code_generator.generate_code(
    student_response="""that implements bubble sort algorithm. I start with the full array, then I make multiple passes through the array, in each pass I compare adjacent elements and swap them if they're in wrong order, after each pass the largest element bubbles to the end, I continue until no more swaps are needed, then return the sorted array.""",
    function_name="bubble_sort",
    gen_type="cgbg",
    segmentation_few_shot_file=few_shot_file
)

generated_code = result["code"]
segmentation = result.get("segmentation", [])

print(f"Generated code: {generated_code[0]}")
print("\nBubble sort algorithm segmentation:")
if segmentation and segmentation[0].get("groups"):
    print(f"Found {len(segmentation[0]['groups'])} conceptual segments:")
    for i, seg in enumerate(segmentation[0]["groups"]):
        print(f"\n  Concept {i+1}: {seg['explanation_portion']}")
        print(f"  Implementation: {seg['code']}")
        
    # Analyze segmentation quality
    total_explanation_length = len(" ".join([s['explanation_portion'] for s in segmentation[0]["groups"]]))
    total_code_length = len(" ".join([s['code'] for s in segmentation[0]["groups"]]))
    
    print(f"\nSegmentation Analysis:")
    print(f"  - Number of segments: {len(segmentation[0]['groups'])}")
    print(f"  - Total explanation coverage: {total_explanation_length} chars")
    print(f"  - Total code coverage: {total_code_length} chars")
else:
    print("No segmentation results available")

# ============================================================================
# Example 6: Error Handling in Segmentation
# ============================================================================
print("\n\n6. Error Handling in Segmentation")
print("-" * 50)

try:
    # Try segmentation without few-shot file (should still work but may be less accurate)
    result = code_generator.generate_code(
        student_response="that adds numbers in a list step by step",
        function_name="add_step_by_step",
        gen_type="cgbg",
        # Note: No segmentation_few_shot_file provided
    )
    
    print("Code generation without few-shot file:")
    print(f"Generated code: {result['code'][0]}")
    print("Segmentation: Not requested (no few-shot file)")
    
except Exception as e:
    print(f"Error in segmentation: {e}")

# ============================================================================
# Example 7: Segmentation vs Non-Segmentation Comparison
# ============================================================================
print("\n\n7. Segmentation vs Non-Segmentation Comparison")
print("-" * 50)

# Generate without segmentation
result_no_seg = code_generator.generate_code(
    student_response="that reverses a string character by character",
    function_name="reverse_string_simple",
    gen_type="cgbg"
)

# Generate with segmentation
result_with_seg = code_generator.generate_code(
    student_response="that reverses a string. I start with an empty result string, then I go through each character in the original string from end to beginning, adding each character to my result string, and finally return the reversed result.",
    function_name="reverse_string_detailed",
    gen_type="cgbg",
    segmentation_few_shot_file=few_shot_file
)

print("Without segmentation:")
print(f"Code: {result_no_seg['code'][0]}")
print("Segmentation: None")

print("\nWith segmentation:")
print(f"Code: {result_with_seg['code'][0]}")
if result_with_seg.get("segmentation"):
    print("Segmentation mapping:")
    for i, seg in enumerate(result_with_seg["segmentation"][0].get("groups", [])):
        print(f"  {i+1}. {seg['explanation_portion']} -> {seg['code']}")

# Clean up temporary file
os.remove(few_shot_file)
print(f"\nCleaned up temporary file: {few_shot_file}")

print("\n" + "="*80)
print("SEGMENTATION EXAMPLES COMPLETE")
print("="*80)
print("\nKey Segmentation Concepts:")
print("1. Segmentation only works with gen_type='cgbg'")
print("2. Requires segmentation_few_shot_file for best results")
print("3. Maps explanation portions to specific code segments")
print("4. Useful for understanding student problem-solving approaches")
print("5. Can work with multiple code generations")
print("6. Provides insight into code comprehension and planning")
print("7. Helps identify gaps between explanation and implementation")