import subprocess
from copy import deepcopy
from typing import List, Dict, Any, Union
from .languages import language_registry


class CodeTestResult:
    """Simple, language-agnostic test result container."""

    def __init__(self):
        self.test_results = []
        self.successes = 0
        self.failures = 0
        self.errors = 0

    def add_success(self, function_call, expected_output, actual_output):
        """Add a successful test result."""
        self.test_results.append(
            {
                "function_call": function_call,
                "expected_output": expected_output,
                "actual_output": actual_output,
                "pass": True,
                "error": None,
            }
        )
        self.successes += 1

    def add_failure(self, function_call, expected_output, actual_output, error_msg):
        """Add a failed test result."""
        self.test_results.append(
            {
                "function_call": function_call,
                "expected_output": expected_output,
                "actual_output": actual_output,
                "pass": False,
                "error": error_msg,
            }
        )
        self.failures += 1

    def add_error(self, function_call, error_msg):
        """Add an error result."""
        self.test_results.append(
            {
                "function_call": function_call,
                "expected_output": "N/A",
                "actual_output": "N/A",
                "pass": False,
                "error": error_msg,
            }
        )
        self.errors += 1

    def was_successful(self):
        """Return True if all tests passed."""
        return self.failures == 0 and self.errors == 0

    @property
    def testsRun(self):
        """Compatibility property for existing code."""
        return len(self.test_results)

    def __str__(self):
        return f"<TestResult run={self.testsRun} errors={self.errors} failures={self.failures}>"

    def __repr__(self):
        return self.__str__()


class CodeTester:
    """
    A class that runs tests on a user's code with given test cases
    and returns the results of the tests in json format
    """

    def __init__(
        self,
        code: Union[str, List[str]],
        test_cases: List[Dict[str, Any]],
        inplace: str = "0",
        function_name: str = "foo",
        language: str = "python",
    ):
        self.code = code
        self.test_cases = test_cases
        self.inplace = inplace
        self.function_name = function_name
        self.language = language

    def run_tests(self) -> Union[CodeTestResult, List[CodeTestResult]]:
        """Run tests on the provided code and return results."""
        if isinstance(self.code, list):
            return [self._run_test(code_item) for code_item in self.code]

        if isinstance(self.code, str):
            return self._run_test(self.code)

        raise ValueError("Code must be a string or a list of strings")

    def _run_test(self, code: str) -> CodeTestResult:
        """Unified test execution for all languages."""

        # Get the language executor
        executor = language_registry.get_executor(self.language)
        if not executor:
            raise ValueError(f"Unsupported language: {self.language}")

        result = CodeTestResult()

        for test_case in self.test_cases:
            if not isinstance(test_case, dict):
                raise ValueError("Test case must be a dictionary")

            if "parameters" not in test_case:
                raise ValueError("Test case must contain 'parameters' key")

            if "expected" not in test_case:
                raise ValueError("Test case must contain 'expected' key")

            # Prepare test case with metadata
            test_case = deepcopy(test_case)
            test_case["function_name"] = self.function_name
            test_case["inplace"] = self.inplace

            try:
                # Execute the test
                test_result = executor.execute_test(code, test_case)

                # Add result directly to our simple TestResult
                if test_result["passed"]:
                    result.add_success(
                        test_result.get("function_call", "N/A"),
                        test_case["expected"],
                        test_result.get("actual", None),
                    )
                else:
                    result.add_failure(
                        test_result.get("function_call", "N/A"),
                        test_case["expected"],
                        test_result.get("actual", None),
                        test_result.get("error", "Test failed"),
                    )
            except (RuntimeError, ValueError, OSError, subprocess.TimeoutExpired) as e:
                result.add_error("Error executing test", str(e))

        # Clean up
        executor.cleanup()
        return result
