"""
Comprehensive tests for Go executor focusing on type inference capabilities.

This module tests the GoExecutor's ability to:
1. Execute Go code with JSON input/output
2. Infer types automatically from parameter values (like Python/JS)
3. Handle different execution modes (normal, in-place, both)
4. Properly handle errors and edge cases
5. Support both explicit types and automatic type inference
6. Handle Go-specific types like slices, interfaces, etc.
"""

import os
import subprocess
import sys

import pytest

# Add the project root to the path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "../../.."))

from eiplgrader.languages.executors.go_executor import (
    GoExecutor,
)  # pylint: disable=wrong-import-position
from tests.fixtures.mock_code_samples import (
    go_samples,
)  # pylint: disable=wrong-import-position


class TestGoExecutor:  # pylint: disable=too-many-public-methods
    """Test suite for Go executor with focus on type inference capabilities."""

    def setup_method(self):
        """Set up test fixtures."""
        self.executor = GoExecutor()

    def teardown_method(self):
        """Clean up after each test."""
        self.executor.cleanup()

    @pytest.fixture(autouse=True)
    def check_go_compiler(self):
        """Check if Go is available before running tests."""
        try:
            subprocess.run(["go", "version"], capture_output=True, check=True)
        except (subprocess.CalledProcessError, FileNotFoundError):
            pytest.skip("Go not available - skipping Go executor tests")

    def test_type_inference_integers(self):
        """Test type inference with integer parameters and return values."""
        test_case = {
            "function_name": "addNumbers",
            "parameters": {"a": 5, "b": 3},
            "expected": 8,
            "inplace": "0",
        }

        result = self.executor.execute_test(go_samples.ADD_NUMBERS, test_case)

        assert result["passed"] is True
        assert result["actual"] == 8
        assert result["expected"] == 8
        assert "error" not in result or result["error"] is None

    def test_type_inference_floats(self):
        """Test type inference with floating point numbers."""
        test_case = {
            "function_name": "calculateAverage",
            "parameters": {"a": 10.5, "b": 7.3},
            "expected": 8.9,
            "inplace": "0",
        }

        result = self.executor.execute_test(go_samples.CALCULATE_AVERAGE, test_case)

        assert result["passed"] is True
        assert abs(result["actual"] - 8.9) < 0.001

    def test_type_inference_strings(self):
        """Test type inference with string parameters."""
        test_case = {
            "function_name": "countVowels",
            "parameters": {"s": "hello world"},
            "expected": 3,
            "inplace": "0",
        }

        result = self.executor.execute_test(go_samples.COUNT_VOWELS, test_case)

        assert result["passed"] is True
        assert result["actual"] == 3

    def test_type_inference_slices(self):
        """Test type inference with slice parameters."""
        test_case = {
            "function_name": "sumEvenNumbers",
            "parameters": {"numbers": [1, 2, 3, 4, 5, 6]},
            "expected": 12,
            "inplace": "0",
        }

        result = self.executor.execute_test(go_samples.SUM_EVEN_NUMBERS, test_case)

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

        result = self.executor.execute_test(go_samples.IS_PALINDROME, test_case)

        assert result["passed"] is True
        assert result["actual"] is True

    def test_type_inference_mixed_types(self):
        """Test type inference with mixed parameter types."""
        test_case = {
            "function_name": "formatInfo",
            "parameters": {
                "name": "Alice",
                "age": 25,
                "isActive": False,
                "salary": 75000.50,
            },
            "expected": "Alice,25,false,75000.50",
            "inplace": "0",
        }

        result = self.executor.execute_test(go_samples.FORMAT_INFO, test_case)

        assert result["passed"] is True
        assert result["actual"] == "Alice,25,false,75000.50"

    def test_type_inference_empty_slices(self):
        """Test type inference with empty slices."""
        test_case = {
            "function_name": "sumEvenNumbers",
            "parameters": {"numbers": []},
            "expected": 0,
            "inplace": "0",
        }

        result = self.executor.execute_test(go_samples.SUM_EVEN_NUMBERS, test_case)

        assert result["passed"] is True
        assert result["actual"] == 0

    def test_type_inference_string_slices(self):
        """Test type inference with string slice parameters."""
        test_case = {
            "function_name": "joinWords",
            "parameters": {"words": ["hello", "world", "go"]},
            "expected": "hello world go",
            "inplace": "0",
        }

        result = self.executor.execute_test(go_samples.JOIN_WORDS, test_case)

        assert result["passed"] is True
        assert result["actual"] == "hello world go"

    def test_inplace_mode_0_normal_execution(self):
        """Test normal execution mode (inplace='0')."""
        test_case = {
            "function_name": "factorial",
            "parameters": {"n": 5},
            "expected": 120,
            "inplace": "0",
        }

        result = self.executor.execute_test(go_samples.FACTORIAL, test_case)

        assert result["passed"] is True
        assert result["actual"] == 120

    def test_inplace_mode_1_slice_modification(self):
        """Test in-place modification mode (inplace='1')."""
        test_case = {
            "function_name": "sortSlice",
            "parameters": {"arr": [3, 1, 4, 1, 5]},
            "expected": [1, 1, 3, 4, 5],
            "inplace": "1",
        }

        result = self.executor.execute_test(go_samples.SORT_SLICE, test_case)

        assert result["passed"] is True
        assert result["actual"] == [1, 1, 3, 4, 5]

    def test_inplace_mode_2_modify_and_return(self):
        """Test modify-and-return mode (inplace='2')."""
        code = """
func processAndReturn(arr []int) int {
    // Modify the slice (add 1 to each element)
    for i := range arr {
        arr[i] += 1
    }
    // Return the length
    return len(arr)
}
"""
        test_case = {
            "function_name": "processAndReturn",
            "parameters": {"arr": [1, 2, 3]},
            "expected": 3,
            "inplace": "2",
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"] is True
        assert result["actual"] == 3

    def test_slice_return_types(self):
        """Test functions that return slices."""
        test_case = {
            "function_name": "doubleSlice",
            "parameters": {"arr": [1, 2, 3, 4]},
            "expected": [2, 4, 6, 8],
            "inplace": "0",
        }

        result = self.executor.execute_test(go_samples.DOUBLE_SLICE, test_case)

        assert result["passed"] is True
        assert result["actual"] == [2, 4, 6, 8]

    def test_find_operations(self):
        """Test finding operations."""
        test_case = {
            "function_name": "findMax",
            "parameters": {"numbers": [3, 7, 2, 9, 1]},
            "expected": 9,
            "inplace": "0",
        }

        result = self.executor.execute_test(go_samples.FIND_MAX, test_case)

        assert result["passed"] is True
        assert result["actual"] == 9

    def test_search_operations(self):
        """Test search operations returning indices."""
        test_case = {
            "function_name": "linearSearch",
            "parameters": {"arr": [10, 20, 30, 40], "target": 30},
            "expected": 2,
            "inplace": "0",
        }

        result = self.executor.execute_test(go_samples.LINEAR_SEARCH, test_case)

        assert result["passed"] is True
        assert result["actual"] == 2

    def test_search_not_found(self):
        """Test search operations when target not found."""
        test_case = {
            "function_name": "linearSearch",
            "parameters": {"arr": [10, 20, 30, 40], "target": 99},
            "expected": -1,
            "inplace": "0",
        }

        result = self.executor.execute_test(go_samples.LINEAR_SEARCH, test_case)

        assert result["passed"] is True
        assert result["actual"] == -1

    def test_nested_slice_operations(self):
        """Test nested slice operations."""
        test_case = {
            "function_name": "flattenNested",
            "parameters": {"nested": [[1, 2], [3, 4], [5]]},
            "expected": [1, 2, 3, 4, 5],
            "inplace": "0",
        }

        result = self.executor.execute_test(go_samples.FLATTEN_NESTED, test_case)

        assert result["passed"] is True
        assert result["actual"] == [1, 2, 3, 4, 5]

    def test_float_slice_operations(self):
        """Test operations with float slices."""
        test_case = {
            "function_name": "average",
            "parameters": {"numbers": [1.0, 2.0, 3.0, 4.0, 5.0]},
            "expected": 3.0,
            "inplace": "0",
        }

        result = self.executor.execute_test(go_samples.AVERAGE, test_case)

        assert result["passed"] is True
        assert result["actual"] == 3.0

    def test_error_handling_compilation_error(self):
        """Test error handling with compilation errors."""
        invalid_code = """
func brokenFunction(x int) int {
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
        assert "Compilation failed" in result["error"]

    def test_error_handling_runtime_error(self):
        """Test error handling with runtime errors."""
        code = """
func divideByZero(x int) int {
    return x / 0
}
"""
        test_case = {
            "function_name": "divideByZero",
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
func largeNumberOperation(x int) int {
    return x * 1000000
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
        test_case = {
            "function_name": "countCharacters",
            "parameters": {"s": "héllo wörld 🌍"},
            "expected": 13,
            "inplace": "0",
        }

        result = self.executor.execute_test(go_samples.COUNT_CHARACTERS, test_case)

        assert result["passed"] is True
        assert result["actual"] == 13

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

        # Execute the test - Go executor should infer types automatically
        result = self.executor.execute_test(go_samples.ADD_NUMBERS, test_case)

        # The executor should handle type inference internally
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

        result = self.executor.execute_test(go_samples.ADD_NUMBERS, test_case)

        assert result["passed"] is True
        assert result["actual"] == 8

    def test_go_specific_features(self):
        """Test Go-specific language features."""
        # Test multiple return values (common Go pattern)
        code = """
func divMod(a, b int) (int, int) {
    return a / b, a % b
}
"""
        test_case = {
            "function_name": "divMod",
            "parameters": {"a": 17, "b": 5},
            "expected": [3, 2],  # Go returns multiple values as array in JSON
            "inplace": "0",
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"] is True
        assert result["actual"] == [3, 2]

    def test_interface_empty_handling(self):
        """Test handling of interface{} (empty interface) types."""
        # Go can handle any type through interface{}
        code = """
import "fmt"

func formatAny(value interface{}) string {
    return fmt.Sprintf("%v", value)
}
"""
        test_case = {
            "function_name": "formatAny",
            "parameters": {"value": 42},
            "expected": "42",
            "inplace": "0",
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"] is True
        assert result["actual"] == "42"

    def test_function_call_display(self):
        """Test that function call is properly formatted for display."""
        test_case = {
            "function_name": "addNumbers",
            "parameters": {"a": 5, "b": 3},
            "expected": 8,
            "inplace": "0",
        }

        result = self.executor.execute_test(go_samples.ADD_NUMBERS, test_case)

        assert result["passed"] is True
        # Go should format function calls appropriately
        if "function_call" in result:
            assert "addNumbers" in result["function_call"]

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
        result = self.executor.execute_test(go_samples.ADD_NUMBERS, test_case)

        # Clean up
        self.executor.cleanup()

        # Check that no excessive new temp files remain
        final_files = os.listdir(temp_dir)
        # Allow some tolerance for system temp files
        assert len(final_files) <= len(initial_files) + 5
        assert result["passed"] is True

    def test_type_inference_consistency(self):
        """Test that type inference is consistent across similar operations."""
        # Test with integers
        int_test = {
            "function_name": "addNumbers",
            "parameters": {"a": 5, "b": 3},
            "expected": 8,
            "inplace": "0",
        }

        # Test with floats
        float_test = {
            "function_name": "calculateAverage",
            "parameters": {"a": 10.0, "b": 6.0},
            "expected": 8.0,
            "inplace": "0",
        }

        int_result = self.executor.execute_test(go_samples.ADD_NUMBERS, int_test)
        float_result = self.executor.execute_test(
            go_samples.CALCULATE_AVERAGE, float_test
        )

        assert int_result["passed"] is True
        assert float_result["passed"] is True
        assert isinstance(int_result["actual"], int)
        assert isinstance(float_result["actual"], float)

    def test_go_json_marshaling(self):
        """Test that Go's JSON marshaling works correctly for complex types."""
        # Test that slices and basic types marshal/unmarshal correctly
        test_case = {
            "function_name": "doubleSlice",
            "parameters": {"arr": [1, 2, 3]},
            "expected": [2, 4, 6],
            "inplace": "0",
        }

        result = self.executor.execute_test(go_samples.DOUBLE_SLICE, test_case)

        assert result["passed"] is True
        assert result["actual"] == [2, 4, 6]
        # Verify JSON round-trip worked correctly
        assert all(isinstance(x, int) for x in result["actual"])
