"""Python language executor for code testing."""

import tempfile
import importlib
import importlib.util
import os
import json
from copy import deepcopy
from typing import Dict, Any
from .base_executors import InterpretedLanguageExecutor
from .string_utils import process_test_parameters


class PythonExecutor(InterpretedLanguageExecutor):
    """Executor for Python language code testing."""

    def __init__(self):
        super().__init__(interpreter_cmd=["python3"], file_ext=".py")
        self.temp_module = None
        self.temp_files = []

    def prepare_code(self, code: str, test_case: Dict[str, Any]) -> str:
        """Prepare Python code for execution with test harness."""
        # For Python, we typically don't need to modify the code
        # The test execution will handle importing and calling the function
        return code

    def execute_test(self, code: str, test_case: Dict[str, Any]) -> Dict[str, Any]:
        """Execute Python code with test case."""
        # Auto-infer types if not provided
        test_case = self.validate_or_infer_types(test_case)
        
        # Process parameters to decode escape sequences in strings
        if "parameters" in test_case:
            test_case["parameters"] = process_test_parameters(test_case["parameters"])

        try:
            # Write code to temporary file
            with tempfile.NamedTemporaryFile(delete=False, suffix=".py") as temp_file:
                temp_file.write(code.encode("utf-8"))
                temp_file_path = temp_file.name
                self.temp_files.append(temp_file_path)

            # Import the module
            spec = importlib.util.spec_from_file_location("temp_module", temp_file_path)
            if spec is None or spec.loader is None:
                return {
                    "passed": False,
                    "error": "Could not load the temporary module",
                    "actual": None,
                    "expected": test_case.get("expected"),
                }

            temp_module = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(temp_module)

            # Get the function
            function_name = test_case.get("function_name", "foo")
            if not hasattr(temp_module, function_name):
                return {
                    "passed": False,
                    "error": f"Function '{function_name}' not found in the code",
                    "actual": None,
                    "expected": test_case.get("expected"),
                }

            func = getattr(temp_module, function_name)

            # Prepare arguments
            params = test_case.get("parameters", {})
            args = list(params.values())

            # Handle different inplace modes
            inplace_mode = test_case.get("inplace", "0")

            if inplace_mode == "0":
                # Normal function call - function returns a value
                actual = func(*args)
            elif inplace_mode == "1":
                # Function modifies arguments in-place
                # Make a deep copy of the first argument
                if args:
                    actual = deepcopy(args[0])
                    func(actual, *args[1:])
                else:
                    actual = func()
            elif inplace_mode == "2":
                # Function both modifies in-place and returns a value
                if args:
                    modified_arg = deepcopy(args[0])
                    result = func(modified_arg, *args[1:])
                    # Return both the modified argument and the return value
                    actual = result if result is not None else modified_arg
                else:
                    actual = func()
            else:
                return {
                    "passed": False,
                    "error": f"Invalid inplace mode: {inplace_mode}",
                    "actual": None,
                    "expected": test_case.get("expected"),
                }

            # Compare results
            expected = test_case.get("expected")
            passed = actual == expected

            # Create function call string for display
            function_call = f"{function_name}({', '.join(map(repr, args))})"

            return {
                "passed": passed,
                "actual": actual,
                "expected": expected,
                "function_call": function_call,
            }

        except Exception as e:
            return {
                "passed": False,
                "error": str(e),
                "actual": None,
                "expected": test_case.get("expected"),
            }
        finally:
            # Clean up the temporary file
            self.cleanup()

    def cleanup(self) -> None:
        """Clean up temporary files."""
        for temp_file in self.temp_files:
            try:
                if os.path.exists(temp_file):
                    os.remove(temp_file)
            except:
                pass
        self.temp_files = []
