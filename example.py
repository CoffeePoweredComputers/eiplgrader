from eiplgrader.codegen import CodeGenerator
from eiplgrader.tester import CodeTester

code_generator = CodeGenerator("YOUR API KEY HERE")

#
# Example of standard Code Gen use
#

generated_code = code_generator.generate_code("that adds two numbers.")
test_cases = [
    {"parameters:": {"a": 1, "b": 2}, "expected": 3},
    {"parameters:": {"a": 5, "b": 7}, "expected": 12},
    {"parameters:": {"a": -1, "b": 1}, "expected": 0},
]

# Test the generated code
code_tester = CodeTester(generated_code, test_cases)
test_result = code_tester.run_tests()


#
# Example of function redefinition use
#

generated_code = code_generator.generate_code("sum_two_numbers", gen_type="redef")
test_cases = [
    {"parameters:": {"a": 1, "b": 2}, "expected": 3},
    {"parameters:": {"a": 5, "b": 7}, "expected": 12},
    {"parameters:": {"a": -1, "b": 1}, "expected": 0},
]

# Test the generated code
code_tester = CodeTester(generated_code, test_cases)
test_result = code_tester.run_tests()

#
# Exmaple of code generation with multiple generated function
#

generated_code = code_generator.generate_code("that adds two numbers.", num_to_gen=5)
test_cases = [
    {"parameters:": {"a": 1, "b": 2}, "expected": 3},
    {"parameters:": {"a": 5, "b": 7}, "expected": 12},
    {"parameters:": {"a": -1, "b": 1}, "expected": 0},
]

# Test the generated code
code_tester = CodeTester(generated_code, test_cases)
test_result = code_tester.run_tests()


#
# Example of code generation with multiple generated function and redefinition
#

generated_code = code_generator.generate_code(
    "sum_two_numbers", gen_type="redef", num_to_gen=5
)

test_cases = [
    {"parameters:": {"a": 1, "b": 2}, "expected": 3},
    {"parameters:": {"a": 5, "b": 7}, "expected": 12},
    {"parameters:": {"a": -1, "b": 1}, "expected": 0},
]

# Test the generated code
code_tester = CodeTester(generated_code, test_cases)
test_result = code_tester.run_tests()


#
# Example of code generation with segmentation
#

generated_code = code_generator.generate_code(
    "iterates through a list of numbers at each point adding the current number to a sum var and, at the end, returning that var.",
    segmentation_few_shot_file="segmentation_few_shot.json",
    num_to_gen=5,
)

test_cases = [
    {"parameters:": {"lst": [1, 2, 3]}, "expected": 6},
    {"parameters:": {"lst": [5, 7, 8]}, "expected": 20},
    {"parameters:": {"lst": [-1, 1, 0]}, "expected": 0},
]

# Test the generated code
code_tester = CodeTester(generated_code, test_cases)
test_result = code_tester.run_tests()


#
# Example of code generation with segmentation on multiple functions
#

generated_code = code_generator.generate_code(
    "iterates through a list of numbers at each point adding the current number to a sum var and, at the end, returning that var.",
    segmentation_few_shot_file="segmentation_few_shot.json",
)

test_cases = [
    {"parameters:": {"a": 1, "b": 2}, "expected": 3},
    {"parameters:": {"a": 5, "b": 7}, "expected": 12},
    {"parameters:": {"a": -1, "b": 1}, "expected": 0},
]

# Test the generated code
code_tester = CodeTester(generated_code, test_cases)
test_result = code_tester.run_tests()
