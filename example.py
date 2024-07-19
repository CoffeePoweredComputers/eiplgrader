from eipecgbg.code_generator import CodeGenerator
from eipecgbg.tester import CodeTester

# Specify a system prompt and generate code
code_generator = CodeGenerator(
        "that adds two numbers.",
        "YOUR API KEY HERE"
        )
generated_code = code_generator.generate_code()

# Provide test cases as functions
def test_foo_function_1():
    assert foo(1, 2) == 3

def test_foo_function_2():
    assert foo(0, 0) == 0

def test_foo_function_3():
    assert foo(-1, 1) == 0

test_cases = [
    test_foo_function_1,
    test_foo_function_2,
    test_foo_function_3
]

# Test the generated code
code_tester = CodeTester(generated_code, test_cases)
test_result = code_tester.run_tests()

