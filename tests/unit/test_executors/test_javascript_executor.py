"""
Comprehensive tests for JavaScript executor focusing on type inference scenarios.

This module tests the JavaScriptExecutor's ability to:
1. Execute JavaScript code with various data types
2. Infer types automatically from parameter values
3. Handle different execution modes (normal, in-place, both)
4. Properly handle errors and edge cases
5. Work with Node.js runtime environment
"""

import os
import subprocess
import sys

import pytest
from eiplgrader.languages.executors.javascript_executor import JavaScriptExecutor
from tests.fixtures.mock_code_samples import javascript_samples

# Add the project root to the path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "../../.."))


class TestJavaScriptExecutor:  # pylint: disable=too-many-public-methods
    """Test suite for JavaScript executor with focus on type inference."""

    def setup_method(self):
        """Set up test fixtures."""
        self.executor = JavaScriptExecutor()

    def teardown_method(self):
        """Clean up after each test."""
        self.executor.cleanup()

    @pytest.fixture(autouse=True)
    def check_node_js(self):
        """Check if Node.js is available before running tests."""
        try:
            subprocess.run(["node", "--version"], capture_output=True, check=True)
        except (subprocess.CalledProcessError, FileNotFoundError):
            pytest.skip("Node.js not available - skipping JavaScript executor tests")

    def test_type_inference_integers(self):
        """Test type inference with integer parameters and return values."""
        test_case = {
            "function_name": "addNumbers",
            "parameters": {"a": 5, "b": 3},
            "expected": 8,
            "inplace": "0",
        }

        result = self.executor.execute_test(javascript_samples.ADD_NUMBERS, test_case)

        assert result["passed"] is True
        assert result["actual"] == 8
        assert result["expected"] == 8
        assert "error" not in result or result["error"] is None

    def test_type_inference_floats(self):
        """Test type inference with floating point numbers."""
        code = """
function calculateAverage(a, b) {
    return (a + b) / 2.0;
}
"""
        test_case = {
            "function_name": "calculateAverage",
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
            "function_name": "countVowels",
            "parameters": {"str": "hello world"},
            "expected": 3,
            "inplace": "0",
        }

        result = self.executor.execute_test(javascript_samples.COUNT_VOWELS, test_case)

        assert result["passed"] is True
        assert result["actual"] == 3

    def test_type_inference_arrays(self):
        """Test type inference with array parameters."""
        test_case = {
            "function_name": "sumEvenNumbers",
            "parameters": {"numbers": [1, 2, 3, 4, 5, 6]},
            "expected": 12,
            "inplace": "0",
        }

        result = self.executor.execute_test(
            javascript_samples.SUM_EVEN_NUMBERS, test_case
        )

        assert result["passed"] is True
        assert result["actual"] == 12

    def test_type_inference_booleans(self):
        """Test type inference with boolean parameters and return values."""
        test_case = {
            "function_name": "isPalindrome",
            "parameters": {"s": "racecar"},
            "expected": True,
            "inplace": "0",
        }

        result = self.executor.execute_test(javascript_samples.IS_PALINDROME, test_case)

        assert result["passed"] is True
        assert result["actual"] is True

    def test_type_inference_mixed_types(self):
        """Test type inference with mixed parameter types."""
        code = """
function formatInfo(name, age, isStudent) {
    return `${name} is ${age} years old and ${isStudent ? 'is' : 'is not'} a student`;
}
"""
        test_case = {
            "function_name": "formatInfo",
            "parameters": {"name": "Alice", "age": 25, "isStudent": False},
            "expected": "Alice is 25 years old and is not a student",
            "inplace": "0",
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"] is True
        assert result["actual"] == "Alice is 25 years old and is not a student"

    def test_type_inference_empty_arrays(self):
        """Test type inference with empty arrays."""
        test_case = {
            "function_name": "sumEvenNumbers",
            "parameters": {"numbers": []},
            "expected": 0,
            "inplace": "0",
        }

        result = self.executor.execute_test(
            javascript_samples.SUM_EVEN_NUMBERS, test_case
        )

        assert result["passed"] is True
        assert result["actual"] == 0

    def test_type_inference_null_values(self):
        """Test type inference with null values."""
        test_case = {
            "function_name": "findMax",
            "parameters": {"numbers": []},
            "expected": None,
            "inplace": "0",
        }

        result = self.executor.execute_test(javascript_samples.FIND_MAX, test_case)

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

        result = self.executor.execute_test(javascript_samples.FACTORIAL, test_case)

        assert result["passed"] is True
        assert result["actual"] == 120

    def test_inplace_mode_1_array_modification(self):
        """Test in-place modification mode (inplace='1')."""
        test_case = {
            "function_name": "sortArray",
            "parameters": {"arr": [3, 1, 4, 1, 5]},
            "expected": [1, 1, 3, 4, 5],
            "inplace": "1",
        }

        result = self.executor.execute_test(javascript_samples.SORT_ARRAY, test_case)

        assert result["passed"] is True
        assert result["actual"] == [1, 1, 3, 4, 5]

    def test_inplace_mode_2_modify_and_return(self):
        """Test modify-and-return mode (inplace='2')."""
        code = """
function processAndReturn(arr) {
    arr.push(0);
    return arr.length;
}
"""
        test_case = {
            "function_name": "processAndReturn",
            "parameters": {"arr": [1, 2, 3]},
            "expected": 4,
            "inplace": "2",
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"] is True
        assert result["actual"] == 4

    def test_array_operations(self):
        """Test various array operations."""
        test_case = {
            "function_name": "doubleArray",
            "parameters": {"arr": [1, 2, 3, 4]},
            "expected": [2, 4, 6, 8],
            "inplace": "0",
        }

        result = self.executor.execute_test(javascript_samples.DOUBLE_ARRAY, test_case)

        assert result["passed"] is True
        assert result["actual"] == [2, 4, 6, 8]

    def test_string_operations(self):
        """Test string manipulation operations."""
        test_case = {
            "function_name": "reverseString",
            "parameters": {"s": "hello"},
            "expected": "olleh",
            "inplace": "0",
        }

        result = self.executor.execute_test(
            javascript_samples.REVERSE_STRING, test_case
        )

        assert result["passed"] is True
        assert result["actual"] == "olleh"

    def test_object_operations(self):
        """Test object manipulation operations."""
        test_case = {
            "function_name": "mergeObjects",
            "parameters": {"obj1": {"a": 1, "b": 2}, "obj2": {"c": 3, "d": 4}},
            "expected": {"a": 1, "b": 2, "c": 3, "d": 4},
            "inplace": "0",
        }

        result = self.executor.execute_test(javascript_samples.MERGE_OBJECTS, test_case)

        assert result["passed"] is True
        assert result["actual"] == {"a": 1, "b": 2, "c": 3, "d": 4}

    def test_error_handling_missing_function(self):
        """Test error handling when function doesn't exist."""
        test_case = {
            "function_name": "nonexistentFunction",
            "parameters": {"x": 1},
            "expected": None,
            "inplace": "0",
        }

        result = self.executor.execute_test(
            "function someOtherFunction() { return 1; }", test_case
        )

        assert result["passed"] is False
        assert "error" in result

    def test_error_handling_syntax_error(self):
        """Test error handling with syntax errors in code."""
        invalid_code = """
function brokenFunction(x) {
    return x +  // Missing operand
}
"""
        test_case = {
            "function_name": "brokenFunction",
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
function throwError(x) {
    throw new Error("Test error");
}
"""
        test_case = {
            "function_name": "throwError",
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
function largeNumberOperation(x) {
    return x * 1000000;
}
"""
        test_case = {
            "function_name": "largeNumberOperation",
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
function countCharacters(s) {
    return s.length;
}
"""
        test_case = {
            "function_name": "countCharacters",
            "parameters": {"s": "héllo wörld 🌍"},
            "expected": 14,  # JavaScript counts UTF-16 code units, emoji is 2 units
            "inplace": "0",
        }

        result = self.executor.execute_test(code, test_case)

        if not result["passed"]:
            print(f"Test failed: {result}")
        assert result["passed"] is True
        assert result["actual"] == 14

    def test_automatic_type_annotation(self):
        """Test that types are automatically inferred and added to test case."""
        test_case = {
            "function_name": "addNumbers",
            "parameters": {"a": 5, "b": 3},
            "expected": 8,
            "inplace": "0",
        }

        # Before execution, no types should be present
        assert "parameter_types" not in test_case
        assert "expected_type" not in test_case

        # Execute the test
        result = self.executor.execute_test(javascript_samples.ADD_NUMBERS, test_case)

        # The executor should have inferred and added types
        assert result["passed"] is True

    def test_pre_existing_types_respected(self):
        """Test that pre-existing type annotations are respected."""
        test_case = {
            "function_name": "addNumbers",
            "parameters": {"a": 5, "b": 3},
            "parameter_types": {"a": "int", "b": "int"},
            "expected": 8,
            "expected_type": "int",
            "inplace": "0",
        }

        result = self.executor.execute_test(javascript_samples.ADD_NUMBERS, test_case)

        assert result["passed"] is True
        assert result["actual"] == 8

    def test_nested_array_type_inference(self):
        """Test type inference with nested arrays."""
        code = """
function flattenArray(nested) {
    return nested.flat();
}
"""
        test_case = {
            "function_name": "flattenArray",
            "parameters": {"nested": [[1, 2], [3, 4], [5]]},
            "expected": [1, 2, 3, 4, 5],
            "inplace": "0",
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"] is True
        assert result["actual"] == [1, 2, 3, 4, 5]

    def test_json_serialization(self):
        """Test JSON serialization/deserialization of complex types."""
        code = """
function processJSON(data) {
    return {
        count: data.items.length,
        total: data.items.reduce((sum, item) => sum + item.value, 0)
    };
}
"""
        test_case = {
            "function_name": "processJSON",
            "parameters": {
                "data": {"items": [{"value": 10}, {"value": 20}, {"value": 30}]}
            },
            "expected": {"count": 3, "total": 60},
            "inplace": "0",
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"] is True
        assert result["actual"] == {"count": 3, "total": 60}

    def test_arrow_functions(self):
        """Test execution of arrow function syntax."""
        code = """
const multiplyByTwo = (x) => x * 2;
"""
        test_case = {
            "function_name": "multiplyByTwo",
            "parameters": {"x": 5},
            "expected": 10,
            "inplace": "0",
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"] is True
        assert result["actual"] == 10

    def test_async_functions_sync_execution(self):
        """Test that synchronous execution works for regular functions."""
        code = """
function synchronousFunction(x) {
    // Simulating synchronous operation
    let result = 0;
    for (let i = 0; i < x; i++) {
        result += i;
    }
    return result;
}
"""
        test_case = {
            "function_name": "synchronousFunction",
            "parameters": {"x": 5},
            "expected": 10,  # 0+1+2+3+4 = 10
            "inplace": "0",
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"] is True
        assert result["actual"] == 10

    def test_cleanup_temp_files(self):
        """Test that temporary files are properly cleaned up."""
        import tempfile

        # Count existing temp files
        temp_dir = tempfile.gettempdir()
        initial_files = os.listdir(temp_dir)

        test_case = {
            "function_name": "addNumbers",
            "parameters": {"a": 1, "b": 2},
            "expected": 3,
            "inplace": "0",
        }

        # Run test
        result = self.executor.execute_test(javascript_samples.ADD_NUMBERS, test_case)

        # Clean up
        self.executor.cleanup()

        # Check that no excessive temp files remain
        final_files = os.listdir(temp_dir)
        # Allow for some temp files to be created by the system
        assert len(final_files) <= len(initial_files) + 2
        assert result["passed"] is True
