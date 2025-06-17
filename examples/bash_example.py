import json
import os
import sys
from dotenv import load_dotenv

# Load environment variables from .env file
load_dotenv()

sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from eiplgrader.codegen import CodeGenerator
from eiplgrader.tester import CodeTester

# Initialize code generator with OpenAI
code_generator = CodeGenerator(
    api_key=os.getenv("OPENAI_API_KEY"),
    client_type="openai"
)

#
# Example of standard Code Gen use
#

print("1. Standard Code Gen Example")
generated_code = code_generator.generate_code(
    "that adds two numbers.",
    params="a, b",
    assumptions="a and b are numbers",
    model="gpt-4o",
    language="bash"
)
print(f"Generated code:\n{generated_code}")

test_cases = [
    {"parameters": {"a": 1, "b": 2}, "expected": 3},
    {"parameters": {"a": 5, "b": 7}, "expected": 12},
    {"parameters": {"a": -1, "b": 1}, "expected": 0},
]

# Test the generated code
code_tester = CodeTester(generated_code['code'], test_cases, language="bash")
test_result = code_tester.run_tests()
print(test_result, end="\n\n")


#
# Example of function redefinition use
#

print("2. Function Redefinition Example")
generated_code = code_generator.generate_code(
    "sum_nums", 
    gen_type="redef", 
    params="a, b", 
    assumptions="a and b are numbers",
    model="gpt-4o",
    language="bash"
)

print(f"Generated code:\n{generated_code}")

test_cases = [
    {"parameters": {"a": 1, "b": 2}, "expected": 4},
    {"parameters": {"a": 5, "b": 7}, "expected": 13},
    {"parameters": {"a": -1, "b": 1}, "expected": 0},
]

# Test the generated code
code_tester = CodeTester(generated_code['code'], test_cases, function_name="sum_nums", language="bash")
test_result = code_tester.run_tests()
print(test_result, end="\n\n")

#
# Example of code generation with multiple generated functions
#

print("3. Multiple Function Generation Example")
generated_code = code_generator.generate_code(
    "that adds two numbers.", 
    num_to_gen=5, 
    params="a, b", 
    assumptions="a and b are numbers",
    model="gpt-4o",
    language="bash"
)
test_cases = [
    {"parameters": {"a": 1, "b": 2}, "expected": 3},
    {"parameters": {"a": 5, "b": 7}, "expected": 12},
    {"parameters": {"a": -1, "b": 1}, "expected": 0},
]

print(f"Generated code:\n{generated_code}")

# Test the generated code
code_tester = CodeTester(generated_code['code'], test_cases, language="bash")
test_result = code_tester.run_tests()
print(f"Test result: {test_result}", end="\n\n")    


#
# Example of code generation with multiple generated functions and redefinition
#

print("4. Multiple Function Generation with Redefinition Example")
generated_code = code_generator.generate_code(
    "sums_numbers", 
    gen_type="redef", 
    num_to_gen=5, 
    params="a, b", 
    assumptions="a and b are numbers",
    model="gpt-4o",
    language="bash"
)
print(f"Generated code:\n{generated_code}")
test_cases = [
    {"parameters": {"a": 1, "b": 2}, "expected": 4},
    {"parameters": {"a": 5, "b": 7}, "expected": 12},
    {"parameters": {"a": -1, "b": 1}, "expected": 0},
]

# Test the generated code
code_tester = CodeTester(generated_code['code'], test_cases, function_name="sums_numbers", language="bash")
test_result = code_tester.run_tests()
print(f"Test result: {test_result}", end="\n\n")


#
# Example of code generation with segmentation
#

print("5. Segmentation Example")
generated_code = code_generator.generate_code(
    "iterates through a list of numbers at each point adding the current number to a sum var and, at the end, returning that var.",
    segmentation_few_shot_file="segmentation_few_shot.json",
    params="lst",
    assumptions="lst is a list of numbers",
    model="gpt-4o",
    language="bash"
)

test_cases = [
    {"parameters": {"lst": [1, 2, 3]}, "expected": 6},
    {"parameters": {"lst": [5, 7, 8]}, "expected": 20},
    {"parameters": {"lst": [-1, 1, 0]}, "expected": 0},
]

print(f"Generated code:\n{generated_code['code']}")
print(f"Segmentation code:\n{json.dumps(generated_code['segmentation'], indent=4)}", end="\n\n")

# Test the generated code
code_tester = CodeTester(generated_code['code'], test_cases, language="bash")
test_result = code_tester.run_tests()
print(f"Test result: {test_result}", end="\n\n")


#
# Example of code generation with segmentation on multiple functions
#

print("6. Segmentation with Multiple Function Generation Example")
generated_code = code_generator.generate_code(
    "iterates through a list of numbers at each point adding the current number to a sum var and, at the end, returning that var.",
    segmentation_few_shot_file="segmentation_few_shot.json",
    num_to_gen=5,
    params="lst",
    assumptions="lst is a list of numbers",
    model="gpt-4o",
    language="bash"
)

test_cases = [
    {"parameters": {"a": 1, "b": 2}, "expected": 3},
    {"parameters": {"a": 5, "b": 7}, "expected": 12},
    {"parameters": {"a": -1, "b": 1}, "expected": 0},
]

print(f"Generated code:\n{generated_code['code']}")
print(f"Segmentation code:\n{json.dumps(generated_code['segmentation'], indent=4)}", end="\n\n")

code_tester = CodeTester(generated_code['code'], test_cases, language="bash")
test_result = code_tester.run_tests()
print(f"Test result: {test_result}", end="\n\n")