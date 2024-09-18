import os
import json

from eiplgrader.codegen import CodeGenerator
from eiplgrader.tester import CodeTester

def load_test_cases(file_path):
    with open(file_path, 'r') as f:
        test_cases = json.load(f)
    return test_cases

def main():
    api_key = os.getenv('API_KEY')
    prompt = os.getenv('PROMPT')
    user_code = os.getenv('USER_CODE')
    test_cases_file = os.getenv('TEST_CASES_FILE')

    if not test_cases_file:
        raise ValueError("TEST_CASES_FILE must be provided")

    test_cases = load_test_cases(test_cases_file)

    if user_code:
        generated_code = user_code
    elif api_key and prompt:
        code_generator = CodeGenerator(api_key)
        generated_code = code_generator.generate_code(prompt)
    else:
        raise ValueError("Either USER_CODE or both API_KEY and PROMPT must be provided")

    code_tester = CodeTester(generated_code, test_cases)
    test_result = code_tester.run_tests(suppress_output=True)

    if test_result.wasSuccessful():
        print("All tests passed successfully!")
    else:
        print("Some tests failed.")
        print(test_result)

if __name__ == "__main__":
    main()

