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
    test_cases_file = os.getenv('TEST_CASES_FILE')

    if not all([api_key, prompt, test_cases_file]):
        raise ValueError("API_KEY, PROMPT, and TEST_CASES_FILE must all be provided")

    test_cases = load_test_cases(test_cases_file)

    code_generator = CodeGenerator(api_key)

    generated_code = code_generator.generate_code(prompt)

    code_tester = CodeTester(generated_code, test_cases)
    test_result = code_tester.run_tests(suppress_output=True)

    if test_result.wasSuccessful():
        print("All tests passed successfully!")
    else:
        print("Some tests failed.")
        print(test_result)

if __name__ == "__main__":
    main()