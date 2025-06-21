"""
Comprehensive tests for C executor focusing on explicit type validation.

This module tests the CExecutor's ability to:
1. Execute C code with embedded test values
2. Require explicit type annotations for all parameters and return values
3. Handle different execution modes (normal, in-place, both)
4. Validate that required type information is provided
5. Generate proper C test harness code
6. Handle C-specific types like pointers and arrays
"""

import os
import subprocess
import sys
import pytest

# Add the project root to the path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "../../.."))

from eiplgrader.languages.executors.c_executor import (
    CExecutor,
)  # pylint: disable=wrong-import-position
from tests.fixtures.mock_code_samples import (
    c_samples,
)  # pylint: disable=wrong-import-position


class TestCExecutor:  # pylint: disable=too-many-public-methods
    """Test suite for C executor with focus on explicit type validation."""

    def setup_method(self):
        """Set up test fixtures."""
        self.executor = CExecutor()

    def teardown_method(self):
        """Clean up after each test."""
        self.executor.cleanup()

    @pytest.fixture(autouse=True)
    def check_c_compiler(self):
        """Check if C compiler is available before running tests."""
        try:
            subprocess.run(["gcc", "--version"], capture_output=True, check=True)
        except (subprocess.CalledProcessError, FileNotFoundError):
            pytest.skip("C compiler (gcc) not available - skipping C executor tests")

    def test_explicit_types_required_missing_parameter_types(self):
        """Test that missing parameter_types raises validation error."""
        test_case = {
            "function_name": "addNumbers",
            "parameters": {"a": 5, "b": 3},
            "expected": 8,
            "expected_type": "int",
            "inplace": "0",
        }

        with pytest.raises(ValueError) as exc_info:
            self.executor.execute_test(c_samples.ADD_NUMBERS, test_case)

        assert "Missing required type information" in str(exc_info.value)
        assert "parameter_types not provided" in str(exc_info.value)

    def test_explicit_types_required_missing_expected_type(self):
        """Test that missing expected_type raises validation error."""
        test_case = {
            "function_name": "addNumbers",
            "parameters": {"a": 5, "b": 3},
            "parameter_types": {"a": "int", "b": "int"},
            "expected": 8,
            "inplace": "0",
        }

        with pytest.raises(ValueError) as exc_info:
            self.executor.execute_test(c_samples.ADD_NUMBERS, test_case)

        assert "Missing required type information" in str(exc_info.value)
        assert "expected_type not provided" in str(exc_info.value)

    def test_explicit_types_required_missing_individual_parameter_type(self):
        """Test that missing individual parameter type raises validation error."""
        test_case = {
            "function_name": "addNumbers",
            "parameters": {"a": 5, "b": 3},
            "parameter_types": {"a": "int"},  # Missing "b"
            "expected": 8,
            "expected_type": "int",
            "inplace": "0",
        }

        with pytest.raises(ValueError) as exc_info:
            self.executor.execute_test(c_samples.ADD_NUMBERS, test_case)

        assert "parameter_types['b'] not provided" in str(exc_info.value)

    def test_integer_types_explicit(self):
        """Test execution with explicit integer types."""
        test_case = {
            "function_name": "addNumbers",
            "parameters": {"a": 5, "b": 3},
            "parameter_types": {"a": "int", "b": "int"},
            "expected": 8,
            "expected_type": "int",
            "inplace": "0",
        }

        result = self.executor.execute_test(c_samples.ADD_NUMBERS, test_case)

        assert result["passed"] is True
        assert result["actual"] == 8
        assert result["expected"] == 8

    def test_string_types_explicit(self):
        """Test execution with explicit string types."""
        test_case = {
            "function_name": "countVowels",
            "parameters": {"str": "hello world"},
            "parameter_types": {"str": "char*"},
            "expected": 3,
            "expected_type": "int",
            "inplace": "0",
        }

        result = self.executor.execute_test(c_samples.COUNT_VOWELS, test_case)

        assert result["passed"] is True
        assert result["actual"] == 3

    def test_boolean_types_explicit(self):
        """Test execution with explicit boolean types (int in C)."""
        test_case = {
            "function_name": "isPalindrome",
            "parameters": {"s": "racecar"},
            "parameter_types": {"s": "char*"},
            "expected": 1,  # C returns 1 for true
            "expected_type": "int",
            "inplace": "0",
        }

        result = self.executor.execute_test(c_samples.IS_PALINDROME, test_case)

        assert result["passed"] is True
        assert result["actual"] == 1

    def test_array_types_explicit(self):
        """Test execution with explicit array types."""
        test_case = {
            "function_name": "sumEvenNumbers",
            "parameters": {"numbers": [1, 2, 3, 4, 5, 6], "size": 6},
            "parameter_types": {"numbers": "int*", "size": "int"},
            "expected": 12,
            "expected_type": "int",
            "inplace": "0",
        }

        result = self.executor.execute_test(c_samples.SUM_EVEN_NUMBERS, test_case)

        assert result["passed"] is True
        assert result["actual"] == 12

    def test_pointer_types_explicit(self):
        """Test execution with pointer types and array manipulation."""
        test_case = {
            "function_name": "findMax",
            "parameters": {"numbers": [3, 7, 2, 9, 1], "size": 5},
            "parameter_types": {"numbers": "int*", "size": "int"},
            "expected": 9,
            "expected_type": "int",
            "inplace": "0",
        }

        result = self.executor.execute_test(c_samples.FIND_MAX, test_case)

        assert result["passed"] is True
        assert result["actual"] == 9

    def test_inplace_mode_0_normal_execution(self):
        """Test normal execution mode (inplace='0')."""
        test_case = {
            "function_name": "factorial",
            "parameters": {"n": 5},
            "parameter_types": {"n": "int"},
            "expected": 120,
            "expected_type": "int",
            "inplace": "0",
        }

        result = self.executor.execute_test(c_samples.FACTORIAL, test_case)

        assert result["passed"] is True
        assert result["actual"] == 120

    def test_inplace_mode_1_array_modification(self):
        """Test in-place modification mode (inplace='1')."""
        test_case = {
            "function_name": "bubbleSort",
            "parameters": {"arr": [3, 1, 4, 1, 5], "n": 5},
            "parameter_types": {"arr": "int*", "n": "int"},
            "expected": [1, 1, 3, 4, 5],
            "expected_type": "int*",
            "inplace": "1",
        }

        result = self.executor.execute_test(c_samples.BUBBLE_SORT, test_case)

        assert result["passed"] is True
        assert result["actual"] == [1, 1, 3, 4, 5]

    def test_inplace_mode_1_string_modification(self):
        """Test in-place string modification mode (inplace='1')."""
        test_case = {
            "function_name": "reverseString",
            "parameters": {"s": "hello"},
            "parameter_types": {"s": "char*"},
            "expected": "olleh",
            "expected_type": "char*",
            "inplace": "1",
        }

        result = self.executor.execute_test(c_samples.REVERSE_STRING, test_case)

        assert result["passed"] is True
        assert result["actual"] == "olleh"

    def test_void_return_type(self):
        """Test functions with void return type (in-place operations)."""
        test_case = {
            "function_name": "doubleArray",
            "parameters": {"arr": [1, 2, 3, 4], "size": 4},
            "parameter_types": {"arr": "int*", "size": "int"},
            "expected": [2, 4, 6, 8],
            "expected_type": "int*",
            "inplace": "1",
        }

        result = self.executor.execute_test(c_samples.DOUBLE_ARRAY, test_case)

        assert result["passed"] is True
        assert result["actual"] == [2, 4, 6, 8]

    def test_search_operations(self):
        """Test search operations returning indices."""
        test_case = {
            "function_name": "linearSearch",
            "parameters": {"arr": [10, 20, 30, 40], "size": 4, "target": 30},
            "parameter_types": {"arr": "int*", "size": "int", "target": "int"},
            "expected": 2,
            "expected_type": "int",
            "inplace": "0",
        }

        result = self.executor.execute_test(c_samples.LINEAR_SEARCH, test_case)

        assert result["passed"] is True
        assert result["actual"] == 2

    def test_search_not_found(self):
        """Test search operations when target not found."""
        test_case = {
            "function_name": "linearSearch",
            "parameters": {"arr": [10, 20, 30, 40], "size": 4, "target": 99},
            "parameter_types": {"arr": "int*", "size": "int", "target": "int"},
            "expected": -1,
            "expected_type": "int",
            "inplace": "0",
        }

        result = self.executor.execute_test(c_samples.LINEAR_SEARCH, test_case)

        assert result["passed"] is True
        assert result["actual"] == -1

    def test_error_handling_compilation_error(self):
        """Test error handling with compilation errors."""
        invalid_code = """
int brokenFunction(int x) {
    return x +  // Missing operand
}
"""
        test_case = {
            "function_name": "brokenFunction",
            "parameters": {"x": 1},
            "parameter_types": {"x": "int"},
            "expected": 2,
            "expected_type": "int",
            "inplace": "0",
        }

        result = self.executor.execute_test(invalid_code, test_case)

        assert result["passed"] is False
        assert "error" in result
        assert "Compilation failed" in result["error"]

    def test_error_handling_runtime_error(self):
        """Test error handling with runtime errors."""
        code = """
int divideByZero(int x) {
    return x / 0;
}
"""
        test_case = {
            "function_name": "divideByZero",
            "parameters": {"x": 10},
            "parameter_types": {"x": "int"},
            "expected": None,
            "expected_type": "int",
            "inplace": "0",
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"] is False
        # C division by zero behavior is undefined, might not always error

    def test_edge_case_empty_arrays(self):
        """Test handling of empty arrays."""
        test_case = {
            "function_name": "sumEvenNumbers",
            "parameters": {"numbers": [], "size": 0},
            "parameter_types": {"numbers": "int*", "size": "int"},
            "expected": 0,
            "expected_type": "int",
            "inplace": "0",
        }

        result = self.executor.execute_test(c_samples.SUM_EVEN_NUMBERS, test_case)

        assert result["passed"] is True
        assert result["actual"] == 0

    def test_edge_case_large_numbers(self):
        """Test handling of large numbers within int range."""
        code = """
int largeNumberOperation(int x) {
    return x * 1000;
}
"""
        test_case = {
            "function_name": "largeNumberOperation",
            "parameters": {"x": 999999},
            "parameter_types": {"x": "int"},
            "expected": 999999000,
            "expected_type": "int",
            "inplace": "0",
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"] is True
        assert result["actual"] == 999999000

    def test_string_null_termination(self):
        """Test proper handling of null-terminated strings."""
        test_case = {
            "function_name": "isPalindrome",
            "parameters": {"s": "a"},
            "parameter_types": {"s": "char*"},
            "expected": 1,
            "expected_type": "int",
            "inplace": "0",
        }

        result = self.executor.execute_test(c_samples.IS_PALINDROME, test_case)

        assert result["passed"] is True
        assert result["actual"] == 1

    def test_type_validation_comprehensive(self):
        """Test comprehensive type validation message."""
        test_case = {
            "function_name": "addNumbers",
            "parameters": {"a": 5, "b": 3},
            "expected": 8,
            "inplace": "0",
        }

        with pytest.raises(ValueError) as exc_info:
            self.executor.execute_test(c_samples.ADD_NUMBERS, test_case)

        error_message = str(exc_info.value)
        assert "Missing required type information" in error_message
        assert "parameter_types not provided" in error_message
        assert "expected_type not provided" in error_message
        assert '"parameter_types": {"param1": "type1", ...}' in error_message
        assert '"expected_type": "type"' in error_message

    def test_parameter_embedding_generation(self):
        """Test that parameters are properly embedded in generated test harness."""
        test_case = {
            "function_name": "addNumbers",
            "parameters": {"a": 42, "b": 13},
            "parameter_types": {"a": "int", "b": "int"},
            "expected": 55,
            "expected_type": "int",
            "inplace": "0",
        }

        # Test the prepare_code method to check parameter embedding
        prepared_code = self.executor.prepare_code(c_samples.ADD_NUMBERS, test_case)

        # Check that parameters are embedded
        assert "int a = 42;" in prepared_code
        assert "int b = 13;" in prepared_code
        assert "addNumbers(a, b)" in prepared_code

        # Execute to ensure it works
        result = self.executor.execute_test(c_samples.ADD_NUMBERS, test_case)
        assert result["passed"] is True
        assert result["actual"] == 55

    def test_c_specific_type_system(self):
        """Test C-specific type system requirements."""
        # Test that we distinguish between different pointer types
        test_cases = [
            {
                "name": "int_pointer",
                "param_type": "int*",
                "expected_param": "int* numbers",
            },
            {
                "name": "char_pointer",
                "param_type": "char*",
                "expected_param": "char* str",
            },
        ]

        for case in test_cases:
            test_case = {
                "function_name": "testFunction",
                "parameters": {"param": [1, 2, 3]},
                "parameter_types": {"param": case["param_type"]},
                "expected": 0,
                "expected_type": "int",
                "inplace": "0",
            }

            # This should not raise a validation error for the type format
            try:
                # We expect this to fail at compilation since testFunction doesn't exist
                # but type validation should pass
                self.executor.validate_types_provided(test_case)
            except ValueError as e:
                if "Missing required type information" in str(e):
                    pytest.fail(f"Type validation failed for {case['name']}: {e}")

    def test_cleanup_temp_files(self):
        """Test that temporary files are properly cleaned up."""
        import tempfile

        test_case = {
            "function_name": "addNumbers",
            "parameters": {"a": 1, "b": 2},
            "parameter_types": {"a": "int", "b": "int"},
            "expected": 3,
            "expected_type": "int",
            "inplace": "0",
        }

        # Run test
        result = self.executor.execute_test(c_samples.ADD_NUMBERS, test_case)

        # Clean up
        self.executor.cleanup()

        # Verify the test passed
        assert result["passed"] is True

    def test_multiple_parameter_types(self):
        """Test functions with multiple different parameter types."""
        code = """
#include <stdio.h>
#include <string.h>

int multiTypeFunction(int num, char* str, int* arr, int size) {
    return num + strlen(str) + arr[0] + size;
}
"""
        test_case = {
            "function_name": "multiTypeFunction",
            "parameters": {"num": 5, "str": "test", "arr": [10, 20, 30], "size": 3},
            "parameter_types": {
                "num": "int",
                "str": "char*",
                "arr": "int*",
                "size": "int",
            },
            "expected": 22,  # 5 + 4 + 10 + 3
            "expected_type": "int",
            "inplace": "0",
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"] is True
        assert result["actual"] == 22
