import unittest
import tempfile
import importlib
import os
from copy import deepcopy


class CodeTestResult(unittest.TestResult):
    """
    A test result class that stores the results of the tests
    """

    def __init__(self):
        super().__init__()
        self.test_results = []

    def addSuccess(self, test):
        super().addSuccess(test)
        self.test_results.append(
            {
                "function_call": test.function_call,
                "expected_output": test.expected_output,
                "actual_output": test.actual_output,
                "pass": True,
            }
        )

    def addFailure(self, test, err):
        super().addFailure(test, err)
        self.test_results.append(
            {
                "function_call": test.function_call,
                "expected_output": test.expected_output,
                "actual_output": test.actual_output,
                "pass": False,
            }
        )

    def addError(self, test, err):
        super().addError(test, err)
        self.test_results.append(
            {
                "function_call": test.function_call,
                "expected_output": "N/A",
                "actual_output": str(err),
                "pass": False,
            }
        )


class CodeRunner:
    """
    A test runner class that returns the results of the tests in a dictionary
    """

    def run(self, test_suite):
        result = CodeTestResult()
        test_suite.run(result)
        return result


class CodeFunctionTest(unittest.FunctionTestCase):
    """
    A test case class that runs a single test for a function
    """

    def __init__(self, function_call, args, expected_output, inplace="0", function_name="foo"):
        """
        Args:
            function_call (str): The function call to be tested
            args (list): The arguments to be passed to the function
            expected_output (any): The expected output of the function
            inplace_mode (str): The mode of the inplace operation
                - "0": The function does not perform an inplace operation
                - "1": The function performs an inplace
                - "2": The function performs an inplace and returns a value
            function_name (str): The name of the function to test (default: "foo")
        """

        super().__init__(self.test_user_function)
        self.function_call = function_call
        self.args = args
        self.expected_output = expected_output
        self.actual_output = None
        self.inplace = inplace
        self.function_name = function_name

    def test_user_function(self):
        # Get the function from globals using the specified function name
        user_func = globals().get(self.function_name)

        if self.inplace == "0":
            self.actual_output = user_func(*self.args)

        elif self.inplace == "1":
            self.actual_output = deepcopy(self.args[0])
            user_func(self.actual_output)

        elif self.inplace == "2":
            actual_output_original = deepcopy(self.args[0])
            actual_output_returned = user_func(*self.args)
            result = actual_output_returned
            if result is not None:
                self.actual_output = result
            else:
                self.actual_output = actual_output_original
        else:
            raise ValueError(
                f"Invalid inplace mode: {self.inplace}."
                "Must be one of '0', '1', or '2'"
            )

        # Use unittest's assertEqual method to properly track failures
        self.assertEqual(self.actual_output, self.expected_output)


class CodeTester:
    """
    A class that runs tests on a user's code with given test cases
    and returns the results of the tests in json format
    """

    def __init__(self, code, test_cases, inplace="0", function_name="foo"):
        self.code = code
        self.test_cases = test_cases
        self.current_test = None
        self.inplace = inplace
        self.function_name = function_name

    def run_tests(self, suppress_output=False):

        if isinstance(self.code, list):
            return list(map(lambda x: self._run_test(x, suppress_output), self.code))
        elif isinstance(self.code, str):
            return self._run_test(self.code, suppress_output)
        else:
            raise ValueError(
                "Code must be a string or a list of strings"
            )


    def _run_test(self, code, suppress_output=False):

        with tempfile.NamedTemporaryFile(delete=False, suffix=".py") as temp_file:
            temp_file.write(code.encode("utf-8"))
            temp_file_path = temp_file.name

        spec = importlib.util.spec_from_file_location(
            "temp_module",
            temp_file_path,
        )

        temp_module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(temp_module)

        os.remove(temp_file_path)

        # Get the function from the module using the specified function name
        if not hasattr(temp_module, self.function_name):
            raise AttributeError(f"Function '{self.function_name}' not found in the code")

        globals()[self.function_name] = getattr(temp_module, self.function_name)

        test_suite = unittest.TestSuite()

        for test_case in self.test_cases:

            if not isinstance(test_case, dict):
                raise ValueError("Test case must be a dictionary")

            if "parameters" not in test_case:
                raise ValueError("Test case must contain 'parameters' key")

            if "expected" not in test_case:
                raise ValueError("Test case must contain 'expected' key")

            params = test_case["parameters"]
            args = list(params.values())
            expected_output = test_case["expected"]

            function_call = f"{self.function_name}({', '.join(map(repr, args))})"

            cf_test = CodeFunctionTest(
                function_call=function_call,
                args=args,
                expected_output=expected_output,
                inplace=self.inplace,
                function_name=self.function_name,
            )

            test_suite.addTest(cf_test)

        runner = CodeRunner()
        result = runner.run(test_suite)

        if suppress_output:
            with open(os.devnull, "w", encoding="utf-8") as _:
                pass  # Use with block for safety

        return result
