"""
Comprehensive tests for C++ executor focusing on STL types and explicit type validation.

This module tests the CppExecutor's ability to:
1. Execute C++ code with embedded test values
2. Require explicit type annotations for all parameters and return values
3. Handle STL containers (vector, string, etc.)
4. Handle different execution modes (normal, in-place, both)
5. Validate that required type information is provided
6. Generate proper C++ test harness code
7. Handle C++-specific types and features
"""

import os
import subprocess
import sys
import pytest
from eiplgrader.languages.executors.cpp_executor import CppExecutor
from tests.fixtures.mock_code_samples import cpp_samples

# Add the project root to the path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "../../.."))


class TestCppExecutor:  # pylint: disable=too-many-public-methods
    """Test suite for C++ executor with focus on STL types and explicit type validation."""

    def setup_method(self):
        """Set up test fixtures."""
        self.executor = CppExecutor()

    def teardown_method(self):
        """Clean up after each test."""
        self.executor.cleanup()

    @pytest.fixture(autouse=True)
    def check_cpp_compiler(self):
        """Check if C++ compiler is available before running tests."""
        try:
            subprocess.run(["g++", "--version"], capture_output=True, check=True)
        except (subprocess.CalledProcessError, FileNotFoundError):
            pytest.skip(
                "C++ compiler (g++) not available - skipping C++ executor tests"
            )

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
            self.executor.execute_test(cpp_samples.ADD_NUMBERS, test_case)

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
            self.executor.execute_test(cpp_samples.ADD_NUMBERS, test_case)

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
            self.executor.execute_test(cpp_samples.ADD_NUMBERS, test_case)

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

        result = self.executor.execute_test(cpp_samples.ADD_NUMBERS, test_case)

        assert result["passed"] is True
        assert result["actual"] == 8
        assert result["expected"] == 8

    def test_string_types_explicit(self):
        """Test execution with explicit std::string types."""
        test_case = {
            "function_name": "countVowels",
            "parameters": {"str": "hello world"},
            "parameter_types": {"str": "std::string"},
            "expected": 3,
            "expected_type": "int",
            "inplace": "0",
        }

        result = self.executor.execute_test(cpp_samples.COUNT_VOWELS, test_case)

        assert result["passed"] is True
        assert result["actual"] == 3

    def test_boolean_types_explicit(self):
        """Test execution with explicit boolean types."""
        test_case = {
            "function_name": "isPalindrome",
            "parameters": {"s": "racecar"},
            "parameter_types": {"s": "std::string"},
            "expected": True,
            "expected_type": "bool",
            "inplace": "0",
        }

        result = self.executor.execute_test(cpp_samples.IS_PALINDROME, test_case)

        assert result["passed"] is True
        assert result["actual"] is True

    def test_vector_types_explicit(self):
        """Test execution with explicit std::vector types."""
        test_case = {
            "function_name": "sumEvenNumbers",
            "parameters": {"numbers": [1, 2, 3, 4, 5, 6]},
            "parameter_types": {"numbers": "std::vector<int>"},
            "expected": 12,
            "expected_type": "int",
            "inplace": "0",
        }

        result = self.executor.execute_test(cpp_samples.SUM_EVEN_NUMBERS, test_case)

        assert result["passed"] is True
        assert result["actual"] == 12

    def test_double_types_explicit(self):
        """Test execution with explicit double types."""
        test_case = {
            "function_name": "calculateAverage",
            "parameters": {"a": 10.5, "b": 7.3},
            "parameter_types": {"a": "double", "b": "double"},
            "expected": 8.9,
            "expected_type": "double",
            "inplace": "0",
        }

        result = self.executor.execute_test(cpp_samples.CALCULATE_AVERAGE, test_case)

        assert result["passed"] is True
        assert abs(result["actual"] - 8.9) < 0.001

    def test_string_vector_types_explicit(self):
        """Test execution with explicit std::vector<std::string> types."""
        test_case = {
            "function_name": "joinStrings",
            "parameters": {"words": ["hello", "world", "cpp"]},
            "parameter_types": {"words": "std::vector<std::string>"},
            "expected": "hello world cpp",
            "expected_type": "std::string",
            "inplace": "0",
        }

        result = self.executor.execute_test(cpp_samples.JOIN_STRINGS, test_case)

        assert result["passed"] is True
        assert result["actual"] == "hello world cpp"

    def test_double_vector_types_explicit(self):
        """Test execution with explicit std::vector<double> types."""
        test_case = {
            "function_name": "multiplyDoubles",
            "parameters": {"numbers": [1.5, 2.5, 3.5], "multiplier": 2.0},
            "parameter_types": {
                "numbers": "std::vector<double>",
                "multiplier": "double",
            },
            "expected": [3.0, 5.0, 7.0],
            "expected_type": "std::vector<double>",
            "inplace": "0",
        }

        result = self.executor.execute_test(cpp_samples.MULTIPLY_DOUBLES, test_case)

        assert result["passed"] is True
        assert result["actual"] == [3.0, 5.0, 7.0]

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

        result = self.executor.execute_test(cpp_samples.FACTORIAL, test_case)

        assert result["passed"] is True
        assert result["actual"] == 120

    def test_inplace_mode_1_vector_modification(self):
        """Test in-place modification mode (inplace='1')."""
        test_case = {
            "function_name": "sortVector",
            "parameters": {"arr": [3, 1, 4, 1, 5]},
            "parameter_types": {"arr": "std::vector<int>"},
            "expected": [1, 1, 3, 4, 5],
            "expected_type": "std::vector<int>",
            "inplace": "1",
        }

        result = self.executor.execute_test(cpp_samples.SORT_VECTOR, test_case)

        assert result["passed"] is True
        assert result["actual"] == [1, 1, 3, 4, 5]

    def test_inplace_mode_2_modify_and_return(self):
        """Test modify-and-return mode (inplace='2')."""
        code = """
#include <vector>

int processAndReturn(std::vector<int>& arr) {
    // Modify the vector (add 1 to each element)
    for (int& n : arr) {
        n += 1;
    }
    // Return the size
    return static_cast<int>(arr.size());
}
"""
        test_case = {
            "function_name": "processAndReturn",
            "parameters": {"arr": [1, 2, 3]},
            "parameter_types": {"arr": "std::vector<int>"},
            "expected": 3,
            "expected_type": "int",
            "inplace": "2",
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"] is True
        assert result["actual"] == 3

    def test_vector_return_types(self):
        """Test functions that return vectors."""
        test_case = {
            "function_name": "doubleVector",
            "parameters": {"arr": [1, 2, 3, 4]},
            "parameter_types": {"arr": "std::vector<int>"},
            "expected": [2, 4, 6, 8],
            "expected_type": "std::vector<int>",
            "inplace": "0",
        }

        result = self.executor.execute_test(cpp_samples.DOUBLE_VECTOR, test_case)

        assert result["passed"] is True
        assert result["actual"] == [2, 4, 6, 8]

    def test_find_operations(self):
        """Test finding operations."""
        test_case = {
            "function_name": "findMax",
            "parameters": {"numbers": [3, 7, 2, 9, 1]},
            "parameter_types": {"numbers": "std::vector<int>"},
            "expected": 9,
            "expected_type": "int",
            "inplace": "0",
        }

        result = self.executor.execute_test(cpp_samples.FIND_MAX, test_case)

        assert result["passed"] is True
        assert result["actual"] == 9

    def test_stl_unique_elements(self):
        """Test STL algorithms for unique elements."""
        test_case = {
            "function_name": "getUniqueElements",
            "parameters": {"input": [3, 1, 4, 1, 5, 3, 2]},
            "parameter_types": {"input": "std::vector<int>"},
            "expected": [1, 2, 3, 4, 5],
            "expected_type": "std::vector<int>",
            "inplace": "0",
        }

        result = self.executor.execute_test(cpp_samples.GET_UNIQUE_ELEMENTS, test_case)

        assert result["passed"] is True
        assert result["actual"] == [1, 2, 3, 4, 5]

    def test_search_operations(self):
        """Test search operations returning indices."""
        test_case = {
            "function_name": "linearSearch",
            "parameters": {"arr": [10, 20, 30, 40], "target": 30},
            "parameter_types": {"arr": "std::vector<int>", "target": "int"},
            "expected": 2,
            "expected_type": "int",
            "inplace": "0",
        }

        result = self.executor.execute_test(cpp_samples.LINEAR_SEARCH, test_case)

        assert result["passed"] is True
        assert result["actual"] == 2

    def test_nested_vector_operations(self):
        """Test nested vector operations."""
        test_case = {
            "function_name": "flattenNested",
            "parameters": {"nested": [[1, 2], [3, 4], [5]]},
            "parameter_types": {"nested": "std::vector<std::vector<int>>"},
            "expected": [1, 2, 3, 4, 5],
            "expected_type": "std::vector<int>",
            "inplace": "0",
        }

        result = self.executor.execute_test(cpp_samples.FLATTEN_NESTED, test_case)

        if not result["passed"]:
            print(f"Test failed: {result}")
        assert result["passed"] is True
        assert result["actual"] == [1, 2, 3, 4, 5]

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
        # C++ division by zero behavior is undefined

    def test_edge_case_empty_vectors(self):
        """Test handling of empty vectors."""
        test_case = {
            "function_name": "sumEvenNumbers",
            "parameters": {"numbers": []},
            "parameter_types": {"numbers": "std::vector<int>"},
            "expected": 0,
            "expected_type": "int",
            "inplace": "0",
        }

        result = self.executor.execute_test(cpp_samples.SUM_EVEN_NUMBERS, test_case)

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

    def test_edge_case_unicode_strings(self):
        """Test handling of unicode strings."""
        # Note: C++ std::string.length() counts bytes, not Unicode characters
        # "héllo wörld" has 13 bytes due to UTF-8 encoding
        test_case = {
            "function_name": "countCharacters",
            "parameters": {"s": "héllo wörld"},
            "parameter_types": {"s": "std::string"},
            "expected": 13,  # UTF-8 byte count, not character count
            "expected_type": "int",
            "inplace": "0",
        }

        result = self.executor.execute_test(cpp_samples.COUNT_CHARACTERS, test_case)

        if not result["passed"]:
            print(f"Test failed: {result}")
        assert result["passed"] is True
        assert result["actual"] == 13

    def test_type_validation_comprehensive(self):
        """Test comprehensive type validation message."""
        test_case = {
            "function_name": "addNumbers",
            "parameters": {"a": 5, "b": 3},
            "expected": 8,
            "inplace": "0",
        }

        with pytest.raises(ValueError) as exc_info:
            self.executor.execute_test(cpp_samples.ADD_NUMBERS, test_case)

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
        prepared_code = self.executor.prepare_code(cpp_samples.ADD_NUMBERS, test_case)

        # Check that parameters are embedded
        assert "int a = 42;" in prepared_code
        assert "int b = 13;" in prepared_code
        assert "addNumbers(a, b)" in prepared_code

        # Execute to ensure it works
        result = self.executor.execute_test(cpp_samples.ADD_NUMBERS, test_case)
        assert result["passed"] is True
        assert result["actual"] == 55

    def test_multiple_parameter_types(self):
        """Test functions with multiple different parameter types."""
        test_case = {
            "function_name": "formatInfo",
            "parameters": {
                "name": "Alice",
                "age": 30,
                "isActive": True,
                "salary": 75000.50,
            },
            "parameter_types": {
                "name": "std::string",
                "age": "int",
                "isActive": "bool",
                "salary": "double",
            },
            "expected": "Alice,30,true,75000.50",
            "expected_type": "std::string",
            "inplace": "0",
        }

        result = self.executor.execute_test(cpp_samples.FORMAT_INFO, test_case)

        assert result["passed"] is True
        assert result["actual"] == "Alice,30,true,75000.50"

    def test_cpp_specific_type_system(self):
        """Test C++-specific type system requirements."""
        # Test that we handle STL container types properly
        test_cases = [
            {"name": "vector_int", "param_type": "std::vector<int>", "valid": True},
            {
                "name": "vector_double",
                "param_type": "std::vector<double>",
                "valid": True,
            },
            {
                "name": "vector_string",
                "param_type": "std::vector<std::string>",
                "valid": True,
            },
            {
                "name": "nested_vector",
                "param_type": "std::vector<std::vector<int>>",
                "valid": True,
            },
            {"name": "string", "param_type": "std::string", "valid": True},
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
        test_case = {
            "function_name": "addNumbers",
            "parameters": {"a": 1, "b": 2},
            "parameter_types": {"a": "int", "b": "int"},
            "expected": 3,
            "expected_type": "int",
            "inplace": "0",
        }

        # Run test
        result = self.executor.execute_test(cpp_samples.ADD_NUMBERS, test_case)

        # Clean up
        self.executor.cleanup()

        # Verify the test passed
        assert result["passed"] is True
