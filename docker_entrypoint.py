#!/usr/bin/env python
"""
Docker entry point for eiplgrader.
Reads parameters from environment variables and runs the grader.
"""

import os
import sys
import json
import traceback
from typing import Dict, Any, List, Optional

# Add current directory to Python path
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from eiplgrader import CodeGenerator, CodeTester


def get_env_var(name: str, required: bool = True, default: str = None) -> Optional[str]:
    """Get environment variable with optional requirement check."""
    value = os.environ.get(name, default)
    if required and value is None:
        raise ValueError(f"Required environment variable {name} not set")
    return value


def parse_test_cases(test_cases_str: str) -> List[Dict[str, Any]]:
    """Parse test cases from JSON string."""
    try:
        test_cases = json.loads(test_cases_str)
        if not isinstance(test_cases, list):
            raise ValueError("TEST_CASES must be a JSON array")
        return test_cases
    except json.JSONDecodeError as e:
        raise ValueError(f"Invalid JSON in TEST_CASES: {e}")


def main():
    """Main entry point for Docker container."""
    result = {
        "success": False,
        "error": None,
        "generated_code": None,
        "test_results": None,
        "language": None,
    }

    try:
        # Get required parameters
        api_key = get_env_var("API_KEY")
        student_response = get_env_var("STUDENT_RESPONSE")
        test_cases_str = get_env_var("TEST_CASES")
        language = get_env_var("LANGUAGE")
        function_name = get_env_var("FUNCTION_NAME")
        gen_type = get_env_var("GEN_TYPE", default="cgbg")
        model = get_env_var("MODEL")
        client_type = get_env_var("CLIENT_TYPE")

        # Get optional parameters
        num_generations = int(get_env_var("NUM_GENERATIONS", required=False, default="1"))
        temperature = float(get_env_var("TEMPERATURE", required=False, default="0.0"))
        segmentation = get_env_var("SEGMENTATION", required=False, default="no").lower() == "yes"
        segmentation_file = get_env_var("SEGMENTATION_FILE", required=False, default="")
        inplace_mode = get_env_var("INPLACE_MODE", required=False, default="0")

        # Parse test cases
        test_cases = parse_test_cases(test_cases_str)

        # Additional parameters for redef mode
        params = ""
        assumptions = ""
        if gen_type == "redef":
            params = get_env_var("PARAMS", required=False, default="")
            assumptions = get_env_var("ASSUMPTIONS", required=False, default="")

        # Initialize code generator
        code_generator = CodeGenerator(
            api_key=api_key,
            client_type=client_type,
            language=language
        )

        # Generate code
        generation_result = code_generator.generate_code(
            student_response=student_response,
            model=model,
            gen_type=gen_type,
            params=params,
            assumptions=assumptions,
            num_to_gen=num_generations,
            segmentation_few_shot_file=segmentation_file if segmentation else "",
            temperature=temperature,
            function_name=function_name,
            language=language
        )

        generated_codes = generation_result.get("code", [])
        result["generated_code"] = generated_codes
        result["language"] = language

        # Test each generated code variant
        all_test_results = []
        for i, code in enumerate(generated_codes):
            try:
                # Initialize code tester
                code_tester = CodeTester(
                    code=code,
                    test_cases=test_cases,
                    function_name=function_name,
                    language=language,
                    inplace=inplace_mode
                )

                # Run tests
                test_result = code_tester.run_tests()

                # Convert test result to dict
                test_result_dict = {
                    "variant": i + 1,
                    "code": code,
                    "total_tests": test_result.testsRun,
                    "passed": test_result.successes,
                    "failed": test_result.failures,
                    "errors": test_result.errors,
                    "test_details": test_result.test_results
                }

                all_test_results.append(test_result_dict)

            except Exception as e:
                test_result_dict = {
                    "variant": i + 1,
                    "code": code,
                    "error": str(e),
                    "traceback": traceback.format_exc()
                }
                all_test_results.append(test_result_dict)

        result["test_results"] = all_test_results
        result["success"] = True

        # Include segmentation results if requested
        if segmentation and "segmentation" in generation_result:
            result["segmentation"] = generation_result["segmentation"]

    except Exception as e:
        result["error"] = str(e)
        result["traceback"] = traceback.format_exc()

    # Output JSON result
    print(json.dumps(result, indent=2))
    
    # Exit with appropriate code
    sys.exit(0 if result["success"] else 1)


if __name__ == "__main__":
    main()