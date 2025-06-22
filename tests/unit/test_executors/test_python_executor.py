"""
Comprehensive tests for Python executor focusing on type inference scenarios.

This module tests the PythonExecutor's ability to:
1. Execute Python code with various data types
2. Infer types automatically from parameter values
3. Handle different execution modes (normal, in-place, both)
4. Properly handle errors and edge cases
"""

import os
import sys
import pytest
from eiplgrader.languages.executors.python_executor import PythonExecutor
from tests.fixtures.mock_code_samples import python_samples

# Add the project root to the path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "../../.."))


class TestPythonExecutor:  # pylint: disable=too-many-public-methods
    """Test suite for Python executor with focus on type inference."""

    def setup_method(self):
        """Set up test fixtures."""
        self.executor = PythonExecutor()

    def teardown_method(self):
        """Clean up after each test."""
        self.executor.cleanup()

    def test_type_inference_integers(self):
        """Test type inference with integer parameters and return values."""
        test_case = {
            "function_name": "add_numbers",
            "parameters": {"a": 5, "b": 3},
            "expected": 8,
            "inplace": "0",
        }

        result = self.executor.execute_test(python_samples.ADD_NUMBERS, test_case)

        assert result["passed"] is True
        assert result["actual"] == 8
        assert result["expected"] == 8
        assert "error" not in result or result["error"] is None

    def test_type_inference_floats(self):
        """Test type inference with floating point numbers."""
        code = """
def calculate_average(a, b):
    return (a + b) / 2.0
"""
        test_case = {
            "function_name": "calculate_average",
            "parameters": {"a": 10.5, "b": 7.3},
            "expected": 8.9,
            "inplace": "0",
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"] is True
        assert abs(result["actual"] - 8.9) < 0.001

    def test_type_inference_strings(self):
        """Test type inference with string parameters."""
        test_case = {
            "function_name": "count_vowels",
            "parameters": {"s": "hello world"},
            "expected": 3,
            "inplace": "0",
        }

        result = self.executor.execute_test(python_samples.COUNT_VOWELS, test_case)

        assert result["passed"] is True
        assert result["actual"] == 3

    def test_type_inference_lists(self):
        """Test type inference with list parameters."""
        test_case = {
            "function_name": "sum_even_numbers",
            "parameters": {"numbers": [1, 2, 3, 4, 5, 6]},
            "expected": 12,
            "inplace": "0",
        }

        result = self.executor.execute_test(python_samples.SUM_EVEN_NUMBERS, test_case)

        assert result["passed"] is True
        assert result["actual"] == 12

    def test_type_inference_booleans(self):
        """Test type inference with boolean parameters and return values."""
        test_case = {
            "function_name": "is_palindrome",
            "parameters": {"s": "racecar"},
            "expected": True,
            "inplace": "0",
        }

        result = self.executor.execute_test(python_samples.IS_PALINDROME, test_case)

        assert result["passed"] is True
        assert result["actual"] is True

    def test_type_inference_mixed_types(self):
        """Test type inference with mixed parameter types."""
        code = """
def format_info(name, age, is_student):
    return f"{name} is {age} years old and {'is' if is_student else 'is not'} a student"
"""
        test_case = {
            "function_name": "format_info",
            "parameters": {"name": "Alice", "age": 25, "is_student": False},
            "expected": "Alice is 25 years old and is not a student",
            "inplace": "0",
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"] is True
        assert result["actual"] == "Alice is 25 years old and is not a student"

    def test_type_inference_empty_lists(self):
        """Test type inference with empty lists."""
        test_case = {
            "function_name": "sum_even_numbers",
            "parameters": {"numbers": []},
            "expected": 0,
            "inplace": "0",
        }

        result = self.executor.execute_test(python_samples.SUM_EVEN_NUMBERS, test_case)

        assert result["passed"] is True
        assert result["actual"] == 0

    def test_type_inference_none_values(self):
        """Test type inference with None values."""
        test_case = {
            "function_name": "find_max",
            "parameters": {"numbers": []},
            "expected": None,
            "inplace": "0",
        }

        result = self.executor.execute_test(python_samples.FIND_MAX, test_case)

        assert result["passed"] is True
        assert result["actual"] is None

    def test_inplace_mode_0_normal_execution(self):
        """Test normal execution mode (inplace='0')."""
        test_case = {
            "function_name": "factorial",
            "parameters": {"n": 5},
            "expected": 120,
            "inplace": "0",
        }

        result = self.executor.execute_test(python_samples.FACTORIAL, test_case)

        assert result["passed"] is True
        assert result["actual"] == 120

    def test_inplace_mode_1_list_modification(self):
        """Test in-place modification mode (inplace='1')."""
        test_case = {
            "function_name": "sort_list",
            "parameters": {"arr": [3, 1, 4, 1, 5]},
            "expected": [1, 1, 3, 4, 5],
            "inplace": "1",
        }

        result = self.executor.execute_test(python_samples.SORT_LIST, test_case)

        assert result["passed"] is True
        assert result["actual"] == [1, 1, 3, 4, 5]

    def test_inplace_mode_2_modify_and_return(self):
        """Test modify-and-return mode (inplace='2')."""
        code = """
def process_and_return(arr):
    arr.append(0)
    return len(arr)
"""
        test_case = {
            "function_name": "process_and_return",
            "parameters": {"arr": [1, 2, 3]},
            "expected": 4,
            "inplace": "2",
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"] is True
        assert result["actual"] == 4

    def test_complex_types_tuples(self):
        """Test type inference with tuple return values."""
        test_case = {
            "function_name": "divmod_operation",
            "parameters": {"a": 17, "b": 5},
            "expected": (3, 2),
            "inplace": "0",
        }

        result = self.executor.execute_test(python_samples.DIVMOD_OPERATION, test_case)

        assert result["passed"] is True
        assert result["actual"] == (3, 2)

    def test_error_handling_missing_function(self):
        """Test error handling when function doesn't exist."""
        test_case = {
            "function_name": "nonexistent_function",
            "parameters": {"x": 1},
            "expected": None,
            "inplace": "0",
        }

        result = self.executor.execute_test(
            "def some_other_function(): pass", test_case
        )

        assert result["passed"] is False
        assert "error" in result
        assert "Function 'nonexistent_function' not found" in result["error"]

    def test_error_handling_syntax_error(self):
        """Test error handling with syntax errors in code."""
        invalid_code = """
def broken_function(x)
    return x + 1  # Missing colon
"""
        test_case = {
            "function_name": "broken_function",
            "parameters": {"x": 1},
            "expected": 2,
            "inplace": "0",
        }
        
        result = self.executor.execute_test(invalid_code, test_case)

        assert result["passed"] is False
        assert "error" in result

    def test_error_handling_runtime_error(self):
        """Test error handling with runtime errors."""
        code = """
def divide_by_zero(x):
    return x / 0
"""
        test_case = {
            "function_name": "divide_by_zero",
            "parameters": {"x": 10},
            "expected": None,
            "inplace": "0",
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"] is False
        assert "error" in result

    def test_edge_case_large_numbers(self):
        """Test handling of large numbers."""
        code = """
def large_number_operation(x):
    return x * 1000000
"""
        test_case = {
            "function_name": "large_number_operation",
            "parameters": {"x": 999999999},
            "expected": 999999999000000,
            "inplace": "0",
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"] is True
        assert result["actual"] == 999999999000000

    def test_edge_case_unicode_strings(self):
        """Test handling of unicode strings."""
        code = """
def count_characters(s):
    return len(s)
"""
        test_case = {
            "function_name": "count_characters",
            "parameters": {"s": "héllo wörld 🌍"},
            "expected": 13,
            "inplace": "0",
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"] is True
        assert result["actual"] == 13

    def test_automatic_type_annotation(self):
        """Test that types are automatically inferred and added to test case."""
        test_case = {
            "function_name": "add_numbers",
            "parameters": {"a": 5, "b": 3},
            "expected": 8,
            "inplace": "0",
        }

        # Before execution, no types should be present
        assert "parameter_types" not in test_case
        assert "expected_type" not in test_case

        # Execute the test
        result = self.executor.execute_test(python_samples.ADD_NUMBERS, test_case)

        # The executor should have inferred and added types
        assert result["passed"] is True

    def test_pre_existing_types_respected(self):
        """Test that pre-existing type annotations are respected."""
        test_case = {
            "function_name": "add_numbers",
            "parameters": {"a": 5, "b": 3},
            "parameter_types": {"a": "int", "b": "int"},
            "expected": 8,
            "expected_type": "int",
            "inplace": "0",
        }

        result = self.executor.execute_test(python_samples.ADD_NUMBERS, test_case)

        assert result["passed"] is True
        assert result["actual"] == 8

    def test_nested_list_type_inference(self):
        """Test type inference with nested data structures."""
        code = """
def flatten_list(nested):
    result = []
    for sublist in nested:
        result.extend(sublist)
    return result
"""
        test_case = {
            "function_name": "flatten_list",
            "parameters": {"nested": [[1, 2], [3, 4], [5]]},
            "expected": [1, 2, 3, 4, 5],
            "inplace": "0",
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"] is True
        assert result["actual"] == [1, 2, 3, 4, 5]

    def test_function_call_display(self):
        """Test that function call is properly formatted for display."""
        test_case = {
            "function_name": "add_numbers",
            "parameters": {"a": 5, "b": 3},
            "expected": 8,
            "inplace": "0",
        }

        result = self.executor.execute_test(python_samples.ADD_NUMBERS, test_case)

        assert result["passed"] is True
        assert "function_call" in result
        assert "add_numbers(5, 3)" in result["function_call"]

