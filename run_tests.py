import os
import json

from eiplgrader.codegen import CodeGenerator
from eiplgrader.tester import CodeTester


def load_test_cases(file_path, language="python"):
    with open(file_path, "r") as f:
        test_cases = json.load(f)
    
    # Validate test case format for non-Python languages
    if language != "python":
        for i, test_case in enumerate(test_cases):
            # Check for old format and provide helpful error message
            if "parameter_types" not in test_case or "expected_type" not in test_case:
                old_format_detected = True
                missing_fields = []
                if "parameter_types" not in test_case:
                    missing_fields.append("parameter_types")
                if "expected_type" not in test_case:
                    missing_fields.append("expected_type")
                
                error_msg = (
                    f"Test case {i} is using old format for language '{language}'.\n"
                    f"Missing fields: {', '.join(missing_fields)}\n"
                    f"For non-Python languages, test cases must include:\n"
                    f"  - 'parameter_types': mapping of parameter names to their types\n"
                    f"  - 'expected_type': the type of the expected output\n"
                    f"Example format:\n"
                    f'{{\n'
                    f'  "parameters": {{"x": 5, "y": 3}},\n'
                    f'  "parameter_types": {{"x": "int", "y": "int"}},\n'
                    f'  "expected": 8,\n'
                    f'  "expected_type": "int"\n'
                    f'}}'
                )
                raise ValueError(error_msg)
    
    return test_cases


def main():
    api_key = os.getenv("API_KEY")
    prompt = os.getenv("PROMPT")
    user_code = os.getenv("USER_CODE")
    test_cases_file = os.getenv("TEST_CASES_FILE")
    language = os.getenv("LANGUAGE", "python")  # Default to python if not specified
    function_name = os.getenv("FUNCTION_NAME", "foo")
    inplace = os.getenv("INPLACE", "0")

    if not test_cases_file:
        raise ValueError("TEST_CASES_FILE must be provided")

    test_cases = load_test_cases(test_cases_file, language)

    if user_code:
        generated_code = user_code
    elif api_key and prompt:
        code_generator = CodeGenerator(api_key)
        generated_code = code_generator.generate_code(prompt)
    else:
        raise ValueError("Either USER_CODE or both API_KEY and PROMPT must be provided")

    code_tester = CodeTester(generated_code, test_cases, inplace=inplace, 
                           function_name=function_name, language=language)
    test_result = code_tester.run_tests()

    print(json.dumps(test_result.test_results, indent=4))
    return test_result


if __name__ == "__main__":
    main()
