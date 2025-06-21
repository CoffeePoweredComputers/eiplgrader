"""
Comprehensive tests for Java executor focusing on explicit type validation.

This module tests the JavaExecutor's ability to:
1. Execute Java code with embedded test values
2. Require explicit type annotations for all parameters and return values
3. Handle different execution modes (normal, in-place, both)
4. Validate that required type information is provided
5. Generate proper Java test harness code
"""

import os
import subprocess
import sys

import pytest

# Add the project root to the path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "../../.."))

from eiplgrader.languages.executors.java_executor import (
    JavaExecutor,
)  # pylint: disable=wrong-import-position
from tests.fixtures.mock_code_samples import (
    java_samples,
)  # pylint: disable=wrong-import-position


class TestJavaExecutor:  # pylint: disable=too-many-public-methods
    """Test suite for Java executor with focus on explicit type validation."""

    def setup_method(self):
        """Set up test fixtures."""
        self.executor = JavaExecutor()

    def teardown_method(self):
        """Clean up after each test."""
        self.executor.cleanup()

    @pytest.fixture(autouse=True)
    def check_java(self):
        """Check if Java is available before running tests."""
        try:
            subprocess.run(["javac", "-version"], capture_output=True, check=True)
            subprocess.run(["java", "-version"], capture_output=True, check=True)
        except (subprocess.CalledProcessError, FileNotFoundError):
            pytest.skip("Java not available - skipping Java executor tests")

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
            self.executor.execute_test(java_samples.ADD_NUMBERS, test_case)

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
            self.executor.execute_test(java_samples.ADD_NUMBERS, test_case)

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
            self.executor.execute_test(java_samples.ADD_NUMBERS, test_case)

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

        result = self.executor.execute_test(java_samples.ADD_NUMBERS, test_case)

        assert result["passed"] is True
        assert result["actual"] == 8
        assert result["expected"] == 8

    def test_string_types_explicit(self):
        """Test execution with explicit string types."""
        test_case = {
            "function_name": "countVowels",
            "parameters": {"str": "hello world"},
            "parameter_types": {"str": "String"},
            "expected": 3,
            "expected_type": "int",
            "inplace": "0",
        }

        result = self.executor.execute_test(java_samples.COUNT_VOWELS, test_case)

        assert result["passed"] is True
        assert result["actual"] == 3

    def test_boolean_types_explicit(self):
        """Test execution with explicit boolean types."""
        test_case = {
            "function_name": "isPalindrome",
            "parameters": {"s": "racecar"},
            "parameter_types": {"s": "String"},
            "expected": True,
            "expected_type": "boolean",
            "inplace": "0",
        }

        result = self.executor.execute_test(java_samples.IS_PALINDROME, test_case)

        assert result["passed"] is True
        assert result["actual"] is True

    def test_array_types_explicit(self):
        """Test execution with explicit array types."""
        test_case = {
            "function_name": "sumEvenNumbers",
            "parameters": {"numbers": [1, 2, 3, 4, 5, 6]},
            "parameter_types": {"numbers": "int[]"},
            "expected": 12,
            "expected_type": "int",
            "inplace": "0",
        }

        result = self.executor.execute_test(java_samples.SUM_EVEN_NUMBERS, test_case)

        assert result["passed"] is True
        assert result["actual"] == 12

    def test_double_types_explicit(self):
        """Test execution with explicit double types."""
        code = """
public static double calculateAverage(double a, double b) {
    return (a + b) / 2.0;
}
"""
        test_case = {
            "function_name": "calculateAverage",
            "parameters": {"a": 10.5, "b": 7.3},
            "parameter_types": {"a": "double", "b": "double"},
            "expected": 8.9,
            "expected_type": "double",
            "inplace": "0",
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"] is True
        assert abs(result["actual"] - 8.9) < 0.001

    def test_string_array_types_explicit(self):
        """Test execution with explicit string array types."""
        code = """
public static String joinStrings(String[] words) {
    StringBuilder result = new StringBuilder();
    for (int i = 0; i < words.length; i++) {
        if (i > 0) {
            result.append(" ");
        }
        result.append(words[i]);
    }
    return result.toString();
}
"""
        test_case = {
            "function_name": "joinStrings",
            "parameters": {"words": ["hello", "world", "java"]},
            "parameter_types": {"words": "String[]"},
            "expected": "hello world java",
            "expected_type": "String",
            "inplace": "0",
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"] is True
        assert result["actual"] == "hello world java"

    def test_double_array_types_explicit(self):
        """Test execution with explicit double array types."""
        code = """
public static double[] multiplyDoubles(double[] numbers, double multiplier) {
    double[] result = new double[numbers.length];
    for (int i = 0; i < numbers.length; i++) {
        result[i] = numbers[i] * multiplier;
    }
    return result;
}
"""
        test_case = {
            "function_name": "multiplyDoubles",
            "parameters": {"numbers": [1.5, 2.5, 3.5], "multiplier": 2.0},
            "parameter_types": {"numbers": "double[]", "multiplier": "double"},
            "expected": [3.0, 5.0, 7.0],
            "expected_type": "double[]",
            "inplace": "0",
        }

        result = self.executor.execute_test(code, test_case)

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

        result = self.executor.execute_test(java_samples.FACTORIAL, test_case)

        assert result["passed"] is True
        assert result["actual"] == 120

    def test_inplace_mode_1_array_modification(self):
        """Test in-place modification mode (inplace='1')."""
        test_case = {
            "function_name": "sortArray",
            "parameters": {"arr": [3, 1, 4, 1, 5]},
            "parameter_types": {"arr": "int[]"},
            "expected": [1, 1, 3, 4, 5],
            "expected_type": "int[]",
            "inplace": "1",
        }

        result = self.executor.execute_test(java_samples.SORT_ARRAY, test_case)

        assert result["passed"] is True
        assert result["actual"] == [1, 1, 3, 4, 5]

    def test_inplace_mode_2_modify_and_return(self):
        """Test modify-and-return mode (inplace='2')."""
        code = """
public static int processAndReturn(int[] arr) {
    // Modify the array (add 1 to each element)
    for (int i = 0; i < arr.length; i++) {
        arr[i] += 1;
    }
    // Return the length
    return arr.length;
}
"""
        test_case = {
            "function_name": "processAndReturn",
            "parameters": {"arr": [1, 2, 3]},
            "parameter_types": {"arr": "int[]"},
            "expected": 3,
            "expected_type": "int",
            "inplace": "2",
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"] is True
        assert result["actual"] == 3

    def test_array_return_types(self):
        """Test functions that return arrays."""
        test_case = {
            "function_name": "doubleArray",
            "parameters": {"arr": [1, 2, 3, 4]},
            "parameter_types": {"arr": "int[]"},
            "expected": [2, 4, 6, 8],
            "expected_type": "int[]",
            "inplace": "0",
        }

        result = self.executor.execute_test(java_samples.DOUBLE_ARRAY, test_case)

        assert result["passed"] is True
        assert result["actual"] == [2, 4, 6, 8]

    def test_find_operations(self):
        """Test finding operations."""
        test_case = {
            "function_name": "findMax",
            "parameters": {"numbers": [3, 7, 2, 9, 1]},
            "parameter_types": {"numbers": "int[]"},
            "expected": 9,
            "expected_type": "int",
            "inplace": "0",
        }

        result = self.executor.execute_test(java_samples.FIND_MAX, test_case)

        assert result["passed"] is True
        assert result["actual"] == 9

    def test_complex_algorithm(self):
        """Test complex algorithm implementation."""
        test_case = {
            "function_name": "findSecondLargest",
            "parameters": {"arr": [1, 5, 3, 9, 2]},
            "parameter_types": {"arr": "int[]"},
            "expected": 5,
            "expected_type": "int",
            "inplace": "0",
        }

        result = self.executor.execute_test(java_samples.FIND_SECOND_LARGEST, test_case)

        assert result["passed"] is True
        assert result["actual"] == 5

    def test_error_handling_compilation_error(self):
        """Test error handling with compilation errors."""
        invalid_code = """
public static int brokenFunction(int x) {
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
public static int divideByZero(int x) {
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
        assert "error" in result

    def test_edge_case_empty_arrays(self):
        """Test handling of empty arrays."""
        test_case = {
            "function_name": "sumEvenNumbers",
            "parameters": {"numbers": []},
            "parameter_types": {"numbers": "int[]"},
            "expected": 0,
            "expected_type": "int",
            "inplace": "0",
        }

        result = self.executor.execute_test(java_samples.SUM_EVEN_NUMBERS, test_case)

        assert result["passed"] is True
        assert result["actual"] == 0

    def test_edge_case_large_numbers(self):
        """Test handling of large numbers within int range."""
        code = """
public static int largeNumberOperation(int x) {
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
        code = """
public static int countCharacters(String s) {
    return s.length();
}
"""
        test_case = {
            "function_name": "countCharacters",
            "parameters": {"s": "héllo wörld"},
            "parameter_types": {"s": "String"},
            "expected": 11,
            "expected_type": "int",
            "inplace": "0",
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"] is True
        assert result["actual"] == 11

    def test_type_validation_comprehensive(self):
        """Test comprehensive type validation message."""
        test_case = {
            "function_name": "addNumbers",
            "parameters": {"a": 5, "b": 3},
            "expected": 8,
            "inplace": "0",
        }

        with pytest.raises(ValueError) as exc_info:
            self.executor.execute_test(java_samples.ADD_NUMBERS, test_case)

        error_message = str(exc_info.value)
        assert "Missing required type information" in error_message
        assert "parameter_types not provided" in error_message
        assert "expected_type not provided" in error_message
        assert '"parameter_types": {"param1": "type1", ...}' in error_message
        assert '"expected_type": "type"' in error_message

    def test_java_solution_class_extraction(self):
        """Test extraction of methods from Solution class."""
        code_with_solution = """
public class Solution {
    public static int addNumbers(int a, int b) {
        return a + b;
    }
}
"""
        test_case = {
            "function_name": "addNumbers",
            "parameters": {"a": 5, "b": 3},
            "parameter_types": {"a": "int", "b": "int"},
            "expected": 8,
            "expected_type": "int",
            "inplace": "0",
        }

        result = self.executor.execute_test(code_with_solution, test_case)

        assert result["passed"] is True
        assert result["actual"] == 8

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
        prepared_code = self.executor.prepare_code(java_samples.ADD_NUMBERS, test_case)

        # Check that parameters are embedded
        assert "int a = 42;" in prepared_code
        assert "int b = 13;" in prepared_code
        assert "Solution.addNumbers(a, b)" in prepared_code

        # Execute to ensure it works
        result = self.executor.execute_test(java_samples.ADD_NUMBERS, test_case)
        assert result["passed"] is True
        assert result["actual"] == 55

    def test_multiple_parameter_types(self):
        """Test functions with multiple different parameter types."""
        code = """
public static String formatData(String name, int age, boolean isActive, double salary) {
    return String.format("%s,%d,%b,%.2f", name, age, isActive, salary);
}
"""
        test_case = {
            "function_name": "formatData",
            "parameters": {
                "name": "Alice",
                "age": 30,
                "isActive": True,
                "salary": 75000.50,
            },
            "parameter_types": {
                "name": "String",
                "age": "int",
                "isActive": "boolean",
                "salary": "double",
            },
            "expected": "Alice,30,true,75000.50",
            "expected_type": "String",
            "inplace": "0",
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"] is True
        assert result["actual"] == "Alice,30,true,75000.50"

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
        result = self.executor.execute_test(java_samples.ADD_NUMBERS, test_case)

        # Clean up
        self.executor.cleanup()

        # Verify the test passed
        assert result["passed"] is True
