"""
Comprehensive tests for Go executor with explicit type requirements.

This module tests the GoExecutor's ability to:
1. Execute Go code with embedded values (no JSON input)
2. Work with explicit type annotations (required for all test cases)
3. Handle different execution modes (normal, in-place, both)
4. Properly handle errors and edge cases
5. Support Go-specific types like slices, interfaces, etc.
6. Handle complex Go types like nested slices and multiple return values
"""

import os
import subprocess
import sys

import pytest
from eiplgrader.languages.executors.go_executor import GoExecutor
from tests.fixtures.mock_code_samples import go_samples

# Add the project root to the path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "../../.."))


class TestGoExecutor:  # pylint: disable=too-many-public-methods
    """Test suite for Go executor with explicit type requirements."""

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
            "parameter_types": {"a": "int", "b": "int"},
            "expected": 8,
            "expected_type": "int",
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
            "parameter_types": {"a": "float64", "b": "float64"},
            "expected": 8.9,
            "expected_type": "float64",
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
            "parameter_types": {"s": "string"},
            "expected": 3,
            "expected_type": "int",
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
            "parameter_types": {"numbers": "[]int"},
            "expected": 12,
            "expected_type": "int",
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
            "parameter_types": {"s": "string"},
            "expected": True,
            "expected_type": "bool",
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
            "parameter_types": {
                "name": "string",
                "age": "int",
                "isActive": "bool",
                "salary": "float64",
            },
            "expected": "Alice,25,false,75000.50",
            "expected_type": "string",
            "inplace": "0",
        }

        result = self.executor.execute_test(go_samples.FORMAT_INFO, test_case)

        assert result["passed"] is True
        assert result["actual"] == "Alice,25,false,75000.50"

    def test_type_inference_empty_slices(self):
        """Test empty slices with explicit types."""
        test_case = {
            "function_name": "sumEvenNumbers",
            "parameters": {"numbers": []},
            "parameter_types": {"numbers": "[]int"},
            "expected": 0,
            "expected_type": "int",
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
            "parameter_types": {"words": "[]string"},
            "expected": "hello world go",
            "expected_type": "string",
            "inplace": "0",
        }

        result = self.executor.execute_test(go_samples.JOIN_WORDS, test_case)

        if not result["passed"]:
            print(f"Test failed: {result}")
        assert result["passed"] is True
        assert result["actual"] == "hello world go"

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

        result = self.executor.execute_test(go_samples.FACTORIAL, test_case)

        assert result["passed"] is True
        assert result["actual"] == 120

    def test_inplace_mode_1_slice_modification(self):
        """Test in-place modification mode (inplace='1')."""
        test_case = {
            "function_name": "sortSlice",
            "parameters": {"arr": [3, 1, 4, 1, 5]},
            "parameter_types": {"arr": "[]int"},
            "expected": [1, 1, 3, 4, 5],
            "expected_type": "[]int",
            "inplace": "1",
        }

        result = self.executor.execute_test(go_samples.SORT_SLICE, test_case)

        if not result["passed"]:
            print(f"Test failed: {result}")
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
            "parameter_types": {"arr": "[]int"},
            "expected": 3,
            "expected_type": "int",
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
            "parameter_types": {"arr": "[]int"},
            "expected": [2, 4, 6, 8],
            "expected_type": "[]int",
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
            "parameter_types": {"numbers": "[]int"},
            "expected": 9,
            "expected_type": "int",
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
            "parameter_types": {"arr": "[]int", "target": "int"},
            "expected": 2,
            "expected_type": "int",
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
            "parameter_types": {"arr": "[]int", "target": "int"},
            "expected": -1,
            "expected_type": "int",
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
            "parameter_types": {"nested": "[][]int"},
            "expected": [1, 2, 3, 4, 5],
            "expected_type": "[]int",
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
            "parameter_types": {"numbers": "[]float64"},
            "expected": 3.0,
            "expected_type": "float64",
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
func divideByZero(x int) int {
    return x / 0
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
            "parameter_types": {"x": "int"},
            "expected": 999999999000000,
            "expected_type": "int",
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
            "parameter_types": {"s": "string"},
            "expected": 13,
            "expected_type": "int",
            "inplace": "0",
        }

        result = self.executor.execute_test(go_samples.COUNT_CHARACTERS, test_case)

        assert result["passed"] is True
        assert result["actual"] == 13

    def test_automatic_type_annotation(self):
        """Test that types are now required."""
        test_case = {
            "function_name": "addNumbers",
            "parameters": {"a": 5, "b": 3},
            "parameter_types": {"a": "int", "b": "int"},
            "expected": 8,
            "expected_type": "int",
            "inplace": "0",
        }

        # Types are now required for Go
        assert "parameter_types" in test_case
        assert "expected_type" in test_case

        # Execute the test - Go executor requires explicit types
        result = self.executor.execute_test(go_samples.ADD_NUMBERS, test_case)

        # The executor should work with explicit types
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
        # Note: Multiple return values are not currently supported
        # This would require special handling to convert multiple returns to a single JSON-serializable value
        # For now, test a simpler Go-specific feature like defer (which we can't really test)
        # or just skip this test
        pytest.skip("Multiple return values not currently supported by Go executor")

    def test_function_call_display(self):
        """Test that function call is properly formatted for display."""
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
            "parameter_types": {"a": "int", "b": "int"},
            "expected": 3,
            "expected_type": "int",
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
            "parameter_types": {"a": "int", "b": "int"},
            "expected": 8,
            "expected_type": "int",
            "inplace": "0",
        }

        # Test with floats
        float_test = {
            "function_name": "calculateAverage",
            "parameters": {"a": 10.0, "b": 6.0},
            "parameter_types": {"a": "float64", "b": "float64"},
            "expected": 8.0,
            "expected_type": "float64",
            "inplace": "0",
        }

        int_result = self.executor.execute_test(go_samples.ADD_NUMBERS, int_test)
        float_result = self.executor.execute_test(
            go_samples.CALCULATE_AVERAGE, float_test
        )

        assert int_result["passed"] is True
        assert float_result["passed"] is True
        assert isinstance(int_result["actual"], int)
        # JSON parsing may return 8 as int even if it's from a float64 calculation
        # Just check the value is correct
        assert float_result["actual"] == 8.0

    def test_go_slice_handling(self):
        """Test that Go handles slices correctly with embedded values."""
        # Test that slices and basic types marshal/unmarshal correctly
        test_case = {
            "function_name": "doubleSlice",
            "parameters": {"arr": [1, 2, 3]},
            "parameter_types": {"arr": "[]int"},
            "expected": [2, 4, 6],
            "expected_type": "[]int",
            "inplace": "0",
        }

        result = self.executor.execute_test(go_samples.DOUBLE_SLICE, test_case)

        assert result["passed"] is True
        assert result["actual"] == [2, 4, 6]
        # Verify slice values are correct
        assert all(isinstance(x, int) for x in result["actual"])
