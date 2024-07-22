from eiplgrader.codegen import CodeGenerator
from eiplgrader.tester import CodeTester

code_generator = CodeGenerator(
        "YOUR API KEY HERE"
        )
generated_code = code_generator.generate_code("that adds two numbers.")

# define your tests

test_cases = [
    [[1, 2], 3],
    [[-1, 1], 0],
    [[-1, -1], -2]
]

# Test the generated code
code_tester = CodeTester(generated_code, test_cases)
test_result = code_tester.run_tests()

