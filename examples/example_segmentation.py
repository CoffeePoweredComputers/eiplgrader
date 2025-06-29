"""
EiplGrader Segmentation Examples
================================

This file demonstrates Segmentation functionality - mapping student explanations
to specific parts of generated code. This is useful for understanding how
students conceptualize problem-solving steps.

Setup: Set your API key environment variable or create a .env file.
See .env.example for all supported providers and their API keys.

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

# Choose your provider and get the appropriate API key
# Options: "openai", "meta", "ollama"
client_type = "meta"  # Default to Meta for this example
model = "Llama-4-Maverick-17B-128E-Instruct-FP8"  # Default model

# Get the appropriate API key based on the provider
if client_type == "openai":
    api_key = os.getenv("OPENAI_API_KEY")
    model = "gpt-4o"
    if not api_key:
        print("ERROR: Please set OPENAI_API_KEY environment variable")
        print("See .env.example for setup instructions")
        exit(1)
elif client_type == "meta":
    api_key = os.getenv("META_API_KEY")
    if not api_key:
        print("ERROR: Please set META_API_KEY environment variable")
        print("See .env.example for setup instructions")
        exit(1)
elif client_type == "ollama":
    api_key = "not-needed"  # Ollama doesn't need an API key
    model = "codellama:instruct"
else:
    print(f"ERROR: Unsupported client_type: {client_type}")
    exit(1)

print("="*80)
print("SEGMENTATION EXAMPLES")
print("="*80)

# Use the existing segmentation few-shot file
few_shot_dir = "segmentation_few_shot_examples"

# ============================================================================
# Example 1.a: Basic Segmentation with Simple Function
# ============================================================================
print("\n\n1.a. Basic Segmentation - List Sum Function -- Multistructural")
print("-" * 50)

code_generator = CodeGenerator(api_key, client_type=client_type, language="python")

# Note: Segmentation only works with CGBG (gen_type="cgbg")
response="that takes a list of numbers and returns their sum. First I'll set up a total variable, then loop through all numbers adding each to the total, and finally return the result."
result = code_generator.generate_code(
    student_response=response,
    model=model,
    function_name="sum_numbers",
    gen_type="cgbg",  # Segmentation only works with CGBG
    segmentation_few_shot_file=f"{few_shot_dir}/sum_numbers.json",
    num_to_gen=1
)

generated_code = result["code"]
segmentation = result.get("segmentation", [])

print(f"Generated code: \n{generated_code[0]}")
print(f"Student response: {response}")
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
# Example 1.b: Basic Segmentation with Simple Function
# ============================================================================
print("\n\n1.a. Basic Segmentation - List Sum Function -- Relational")
print("-" * 50)

code_generator = CodeGenerator(api_key, client_type=client_type, language="python")

# Note: Segmentation only works with CGBG (gen_type="cgbg")
response="Returns the sum of a list of numbers"
result = code_generator.generate_code(
    student_response=response,
    model=model,
    function_name="sum_numbers",
    gen_type="cgbg",  # Segmentation only works with CGBG
    segmentation_few_shot_file=f"{few_shot_dir}/sum_numbers.json",
    num_to_gen=1
)

generated_code = result["code"]
segmentation = result.get("segmentation", [])

print(f"Generated code: \n{generated_code[0]}")
print(f"Student response: {response}")
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

response="""that checks if a number is prime. My approach is: first I handle the special cases where the number is less than 2 (not prime), then I check if the number is 2 (which is prime), then for other numbers I loop from 2 to the square root of the number checking if any number divides evenly (if so, it's not prime), and if no divisors are found then it's prime."""
result = code_generator.generate_code(
    student_response=response,
    model=model,
    function_name="is_prime_detailed",
    gen_type="cgbg",
    segmentation_few_shot_file=f"{few_shot_dir}/prime_checker.json",
    num_to_gen=1
)

generated_code = result["code"]
segmentation = result.get("segmentation", [])

print(f"Generated code: {generated_code[0]}")
print(f"Student response: {response}")
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

response="""that finds the maximum value in a list. I start by setting the first element as the current maximum, then I go through each remaining element comparing it to the current max, and if I find a larger element I update the maximum, finally I return the maximum value found."""
result = code_generator.generate_code(
    student_response="""that finds the maximum value in a list. I start by setting the first element as the current maximum, then I go through each remaining element comparing it to the current max, and if I find a larger element I update the maximum, finally I return the maximum value found.""",
    model=model,
    function_name="find_maximum",
    gen_type="cgbg",
    segmentation_few_shot_file=f"{few_shot_dir}/find_maximum.json",
    num_to_gen=2  # Generate 2 implementations with segmentation
)

generated_codes = result["code"]
segmentations = result.get("segmentation", [])

for impl_idx, (code, seg) in enumerate(zip(generated_codes, segmentations)):
    print(f"\n--- Implementation {impl_idx + 1} ---")
    print(f"Generated code:\n {code}")
    print("Student response:", response)
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

response="""that counts words in a sentence. My plan is to first remove extra spaces and convert to lowercase for consistency, then split the sentence into individual words using spaces as separators, then count how many words we got, and return that count."""
result = code_generator.generate_code(
    student_response=response,
    model=model,
    function_name="count_words",
    gen_type="cgbg",
    segmentation_few_shot_file=f"{few_shot_dir}/count_words.json"
)

generated_code = result["code"]
segmentation = result.get("segmentation", [])

print(f"Generated code:\n {generated_code[0]}")
print(f"Student response: {response}")
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
print("\n\n5a. Advanced Segmentation Analysis -- Multistructural")
print("-" * 50)

response="""I start with the full array, then I make multiple passes through the array, in each pass I compare adjacent elements and swap them if they're in wrong order, after each pass the largest element bubbles to the end, I continue until no more swaps are needed, then return the sorted array."""
result = code_generator.generate_code(
    student_response=response,
    model=model,
    function_name="bubble_sort",
    gen_type="cgbg",
    segmentation_few_shot_file=f"{few_shot_dir}/bubble_sort.json"
)

generated_code = result["code"]
segmentation = result.get("segmentation", [])

print(f"Generated code: {generated_code[0]}")
print(f"Student response: {response}")
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
# Example 5: Advanced Segmentation Analysis
# ============================================================================
print("\n\n5.b Advanced Segmentation Analysis -- Relational")
print("-" * 50)

response="The function implements the bubble sort algorithm"
result = code_generator.generate_code(
    student_response=response,
    model=model,
    function_name="bubble_sort",
    gen_type="cgbg",
    segmentation_few_shot_file=f"{few_shot_dir}/bubble_sort.json"
)

generated_code = result["code"]
segmentation = result.get("segmentation", [])

print(f"Generated code:\n {generated_code[0]}")
print(f"Student response: {response}")
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


