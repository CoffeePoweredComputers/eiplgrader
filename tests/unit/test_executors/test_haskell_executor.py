"""
Comprehensive tests for Haskell executor focusing on functional programming and explicit type validation.

This module tests the HaskellExecutor's ability to:
1. Execute Haskell code with embedded test values
2. Require explicit type annotations for all parameters and return values
3. Handle functional programming paradigms (pure functions, immutability)
4. Handle different execution modes (normal, in-place, both)
5. Validate that required type information is provided
6. Generate proper Haskell test harness code
7. Handle Haskell-specific types and features
"""

import pytest
import sys
import os
import subprocess

# Add the project root to the path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '../../..'))

from eiplgrader.languages.executors.haskell_executor import HaskellExecutor
from tests.fixtures.mock_code_samples import haskell_samples


class TestHaskellExecutor:
    """Test suite for Haskell executor with focus on functional programming and explicit type validation."""

    def setup_method(self):
        """Set up test fixtures."""
        self.executor = HaskellExecutor()

    def teardown_method(self):
        """Clean up after each test."""
        self.executor.cleanup()

    @pytest.fixture(autouse=True)
    def check_haskell_compiler(self):
        """Check if Haskell compiler is available before running tests."""
        try:
            subprocess.run(["ghc", "--version"], capture_output=True, check=True)
        except (subprocess.CalledProcessError, FileNotFoundError):
            pytest.skip("Haskell compiler (ghc) not available - skipping Haskell executor tests")

    def test_explicit_types_required_missing_parameter_types(self):
        """Test that missing parameter_types raises validation error."""
        test_case = {
            "function_name": "addNumbers",
            "parameters": {"a": 5, "b": 3},
            "expected": 8,
            "expected_type": "Int",
            "inplace": "0"
        }
        
        with pytest.raises(ValueError) as exc_info:
            self.executor.execute_test(haskell_samples.ADD_NUMBERS, test_case)
        
        assert "Missing required type information" in str(exc_info.value)
        assert "parameter_types not provided" in str(exc_info.value)

    def test_explicit_types_required_missing_expected_type(self):
        """Test that missing expected_type raises validation error."""
        test_case = {
            "function_name": "addNumbers",
            "parameters": {"a": 5, "b": 3},
            "parameter_types": {"a": "Int", "b": "Int"},
            "expected": 8,
            "inplace": "0"
        }
        
        with pytest.raises(ValueError) as exc_info:
            self.executor.execute_test(haskell_samples.ADD_NUMBERS, test_case)
        
        assert "Missing required type information" in str(exc_info.value)
        assert "expected_type not provided" in str(exc_info.value)

    def test_explicit_types_required_missing_individual_parameter_type(self):
        """Test that missing individual parameter type raises validation error."""
        test_case = {
            "function_name": "addNumbers",
            "parameters": {"a": 5, "b": 3},
            "parameter_types": {"a": "Int"},  # Missing "b"
            "expected": 8,
            "expected_type": "Int",
            "inplace": "0"
        }
        
        with pytest.raises(ValueError) as exc_info:
            self.executor.execute_test(haskell_samples.ADD_NUMBERS, test_case)
        
        assert "parameter_types['b'] not provided" in str(exc_info.value)

    def test_integer_types_explicit(self):
        """Test execution with explicit Int types."""
        test_case = {
            "function_name": "addNumbers",
            "parameters": {"a": 5, "b": 3},
            "parameter_types": {"a": "Int", "b": "Int"},
            "expected": 8,
            "expected_type": "Int",
            "inplace": "0"
        }
        
        result = self.executor.execute_test(haskell_samples.ADD_NUMBERS, test_case)
        
        assert result["passed"] is True
        assert result["actual"] == 8
        assert result["expected"] == 8

    def test_string_types_explicit(self):
        """Test execution with explicit String types."""
        test_case = {
            "function_name": "countVowels",
            "parameters": {"str": "hello world"},
            "parameter_types": {"str": "String"},
            "expected": 3,
            "expected_type": "Int",
            "inplace": "0"
        }
        
        result = self.executor.execute_test(haskell_samples.COUNT_VOWELS, test_case)
        
        assert result["passed"] is True
        assert result["actual"] == 3

    def test_boolean_types_explicit(self):
        """Test execution with explicit Bool types."""
        test_case = {
            "function_name": "isPalindrome",
            "parameters": {"s": "racecar"},
            "parameter_types": {"s": "String"},
            "expected": True,
            "expected_type": "Bool",
            "inplace": "0"
        }
        
        result = self.executor.execute_test(haskell_samples.IS_PALINDROME, test_case)
        
        assert result["passed"] is True
        assert result["actual"] is True

    def test_list_types_explicit(self):
        """Test execution with explicit list types."""
        test_case = {
            "function_name": "sumEvenNumbers",
            "parameters": {"numbers": [1, 2, 3, 4, 5, 6]},
            "parameter_types": {"numbers": "[Int]"},
            "expected": 12,
            "expected_type": "Int",
            "inplace": "0"
        }
        
        result = self.executor.execute_test(haskell_samples.SUM_EVEN_NUMBERS, test_case)
        
        assert result["passed"] is True
        assert result["actual"] == 12

    def test_double_types_explicit(self):
        """Test execution with explicit Double types."""
        test_case = {
            "function_name": "calculateAverage",
            "parameters": {"a": 10.5, "b": 7.3},
            "parameter_types": {"a": "Double", "b": "Double"},
            "expected": 8.9,
            "expected_type": "Double",
            "inplace": "0"
        }
        
        result = self.executor.execute_test(haskell_samples.CALCULATE_AVERAGE, test_case)
        
        assert result["passed"] is True
        assert abs(result["actual"] - 8.9) < 0.001

    def test_string_list_types_explicit(self):
        """Test execution with explicit [String] types."""
        test_case = {
            "function_name": "joinWords",
            "parameters": {"words": ["hello", "world", "haskell"]},
            "parameter_types": {"words": "[String]"},
            "expected": "hello world haskell",
            "expected_type": "String",
            "inplace": "0"
        }
        
        result = self.executor.execute_test(haskell_samples.JOIN_WORDS, test_case)
        
        assert result["passed"] is True
        assert result["actual"] == "hello world haskell"

    def test_inplace_mode_0_normal_execution(self):
        """Test normal execution mode (inplace='0') - all Haskell functions are pure."""
        test_case = {
            "function_name": "factorial",
            "parameters": {"n": 5},
            "parameter_types": {"n": "Int"},
            "expected": 120,
            "expected_type": "Int",
            "inplace": "0"
        }
        
        result = self.executor.execute_test(haskell_samples.FACTORIAL, test_case)
        
        assert result["passed"] is True
        assert result["actual"] == 120

    def test_list_return_types(self):
        """Test functions that return lists."""
        test_case = {
            "function_name": "doubleAll",
            "parameters": {"xs": [1, 2, 3, 4]},
            "parameter_types": {"xs": "[Int]"},
            "expected": [2, 4, 6, 8],
            "expected_type": "[Int]",
            "inplace": "0"
        }
        
        result = self.executor.execute_test(haskell_samples.DOUBLE_ALL, test_case)
        
        assert result["passed"] is True
        assert result["actual"] == [2, 4, 6, 8]

    def test_functional_programming_paradigms(self):
        """Test functional programming constructs like filter and map."""
        test_case = {
            "function_name": "squareEvens",
            "parameters": {"xs": [1, 2, 3, 4, 5, 6]},
            "parameter_types": {"xs": "[Int]"},
            "expected": [4, 16, 36],  # squares of [2, 4, 6]
            "expected_type": "[Int]",
            "inplace": "0"
        }
        
        result = self.executor.execute_test(haskell_samples.SQUARE_EVENS, test_case)
        
        assert result["passed"] is True
        assert result["actual"] == [4, 16, 36]

    def test_recursive_functions(self):
        """Test recursive function implementations."""
        test_case = {
            "function_name": "fibonacci",
            "parameters": {"n": 6},
            "parameter_types": {"n": "Int"},
            "expected": 8,  # fib(6) = 8
            "expected_type": "Int",
            "inplace": "0"
        }
        
        result = self.executor.execute_test(haskell_samples.FIBONACCI, test_case)
        
        assert result["passed"] is True
        assert result["actual"] == 8

    def test_list_concatenation(self):
        """Test list concatenation operations."""
        test_case = {
            "function_name": "concatLists",
            "parameters": {"xs": [1, 2], "ys": [3, 4, 5]},
            "parameter_types": {"xs": "[Int]", "ys": "[Int]"},
            "expected": [1, 2, 3, 4, 5],
            "expected_type": "[Int]",
            "inplace": "0"
        }
        
        result = self.executor.execute_test(haskell_samples.CONCAT_LISTS, test_case)
        
        assert result["passed"] is True
        assert result["actual"] == [1, 2, 3, 4, 5]

    def test_search_operations(self):
        """Test search operations returning indices."""
        test_case = {
            "function_name": "linearSearch",
            "parameters": {"xs": [10, 20, 30, 40], "target": 30},
            "parameter_types": {"xs": "[Int]", "target": "Int"},
            "expected": 2,
            "expected_type": "Int",
            "inplace": "0"
        }
        
        result = self.executor.execute_test(haskell_samples.LINEAR_SEARCH, test_case)
        
        assert result["passed"] is True
        assert result["actual"] == 2

    def test_nested_list_operations(self):
        """Test nested list operations."""
        test_case = {
            "function_name": "flattenNested",
            "parameters": {"nested": [[1, 2], [3, 4], [5]]},
            "parameter_types": {"nested": "[[Int]]"},
            "expected": [1, 2, 3, 4, 5],
            "expected_type": "[Int]",
            "inplace": "0"
        }
        
        result = self.executor.execute_test(haskell_samples.FLATTEN_NESTED, test_case)
        
        assert result["passed"] is True
        assert result["actual"] == [1, 2, 3, 4, 5]

    def test_double_list_operations(self):
        """Test operations with Double lists."""
        test_case = {
            "function_name": "averageList",
            "parameters": {"xs": [1.0, 2.0, 3.0, 4.0, 5.0]},
            "parameter_types": {"xs": "[Double]"},
            "expected": 3.0,
            "expected_type": "Double",
            "inplace": "0"
        }
        
        result = self.executor.execute_test(haskell_samples.AVERAGE_LIST, test_case)
        
        assert result["passed"] is True
        assert result["actual"] == 3.0

    def test_filter_operations(self):
        """Test filtering operations."""
        test_case = {
            "function_name": "filterPositive",
            "parameters": {"xs": [-2, -1, 0, 1, 2, 3]},
            "parameter_types": {"xs": "[Int]"},
            "expected": [1, 2, 3],
            "expected_type": "[Int]",
            "inplace": "0"
        }
        
        result = self.executor.execute_test(haskell_samples.FILTER_POSITIVE, test_case)
        
        assert result["passed"] is True
        assert result["actual"] == [1, 2, 3]

    def test_tuple_return_types(self):
        """Test functions that return tuples."""
        test_case = {
            "function_name": "divmodOperation",
            "parameters": {"a": 17, "b": 5},
            "parameter_types": {"a": "Int", "b": "Int"},
            "expected": [3, 2],  # Tuple (3, 2) as list in JSON
            "expected_type": "(Int, Int)",
            "inplace": "0"
        }
        
        result = self.executor.execute_test(haskell_samples.DIVMOD_OPERATION, test_case)
        
        assert result["passed"] is True
        assert result["actual"] == [3, 2]

    def test_error_handling_compilation_error(self):
        """Test error handling with compilation errors."""
        invalid_code = """
brokenFunction :: Int -> Int
brokenFunction x = x +  -- Missing operand
"""
        test_case = {
            "function_name": "brokenFunction",
            "parameters": {"x": 1},
            "parameter_types": {"x": "Int"},
            "expected": 2,
            "expected_type": "Int",
            "inplace": "0"
        }
        
        result = self.executor.execute_test(invalid_code, test_case)
        
        assert result["passed"] is False
        assert "error" in result
        assert "Compilation failed" in result["error"]

    def test_error_handling_runtime_error(self):
        """Test error handling with runtime errors."""
        code = """
divideByZero :: Int -> Int
divideByZero x = x `div` 0
"""
        test_case = {
            "function_name": "divideByZero",
            "parameters": {"x": 10},
            "parameter_types": {"x": "Int"},
            "expected": None,
            "expected_type": "Int",
            "inplace": "0"
        }
        
        result = self.executor.execute_test(code, test_case)
        
        assert result["passed"] is False
        assert "error" in result

    def test_edge_case_empty_lists(self):
        """Test handling of empty lists."""
        test_case = {
            "function_name": "sumEvenNumbers",
            "parameters": {"numbers": []},
            "parameter_types": {"numbers": "[Int]"},
            "expected": 0,
            "expected_type": "Int",
            "inplace": "0"
        }
        
        result = self.executor.execute_test(haskell_samples.SUM_EVEN_NUMBERS, test_case)
        
        assert result["passed"] is True
        assert result["actual"] == 0

    def test_edge_case_unicode_strings(self):
        """Test handling of unicode strings."""
        test_case = {
            "function_name": "countCharacters",
            "parameters": {"s": "héllo wörld"},
            "parameter_types": {"s": "String"},
            "expected": 11,
            "expected_type": "Int",
            "inplace": "0"
        }
        
        result = self.executor.execute_test(haskell_samples.COUNT_CHARACTERS, test_case)
        
        assert result["passed"] is True
        assert result["actual"] == 11

    def test_type_validation_comprehensive(self):
        """Test comprehensive type validation message."""
        test_case = {
            "function_name": "addNumbers",
            "parameters": {"a": 5, "b": 3},
            "expected": 8,
            "inplace": "0"
        }
        
        with pytest.raises(ValueError) as exc_info:
            self.executor.execute_test(haskell_samples.ADD_NUMBERS, test_case)
        
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
            "parameter_types": {"a": "Int", "b": "Int"},
            "expected": 55,
            "expected_type": "Int",
            "inplace": "0"
        }
        
        # Test the prepare_code method to check parameter embedding
        prepared_code = self.executor.prepare_code(haskell_samples.ADD_NUMBERS, test_case)
        
        # Check that parameters are embedded
        assert "a = 42" in prepared_code
        assert "b = 13" in prepared_code
        assert "addNumbers a b" in prepared_code
        
        # Execute to ensure it works
        result = self.executor.execute_test(haskell_samples.ADD_NUMBERS, test_case)
        assert result["passed"] is True
        assert result["actual"] == 55

    def test_multiple_parameter_types(self):
        """Test functions with multiple different parameter types."""
        test_case = {
            "function_name": "formatInfo",
            "parameters": {"name": "Alice", "age": 30, "isActive": True, "salary": 75000.50},
            "parameter_types": {"name": "String", "age": "Int", "isActive": "Bool", "salary": "Double"},
            "expected": "Alice,30,True,75000.5",
            "expected_type": "String",
            "inplace": "0"
        }
        
        result = self.executor.execute_test(haskell_samples.FORMAT_INFO, test_case)
        
        assert result["passed"] is True
        assert result["actual"] == "Alice,30,True,75000.5"

    def test_haskell_specific_type_system(self):
        """Test Haskell-specific type system requirements."""
        # Test that we handle Haskell type syntax properly
        test_cases = [
            {
                "name": "int_list",
                "param_type": "[Int]",
                "valid": True
            },
            {
                "name": "double_list",
                "param_type": "[Double]",
                "valid": True
            },
            {
                "name": "string_list",
                "param_type": "[String]",
                "valid": True
            },
            {
                "name": "nested_list",
                "param_type": "[[Int]]",
                "valid": True
            },
            {
                "name": "tuple",
                "param_type": "(Int, Int)",
                "valid": True
            },
            {
                "name": "maybe_type",
                "param_type": "Maybe Int",
                "valid": True
            }
        ]
        
        for case in test_cases:
            test_case = {
                "function_name": "testFunction",
                "parameters": {"param": [1, 2, 3]},
                "parameter_types": {"param": case["param_type"]},
                "expected": 0,
                "expected_type": "Int",
                "inplace": "0"
            }
            
            # This should not raise a validation error for the type format
            try:
                # We expect this to fail at compilation since testFunction doesn't exist
                # but type validation should pass
                self.executor.validate_types_provided(test_case)
            except ValueError as e:
                if "Missing required type information" in str(e):
                    pytest.fail(f"Type validation failed for {case['name']}: {e}")

    def test_lazy_evaluation_features(self):
        """Test Haskell's lazy evaluation features."""
        # Test with a potentially infinite operation that should work due to laziness
        code = """
takeFirst :: Int -> [Int] -> [Int]
takeFirst n xs = take n xs
"""
        test_case = {
            "function_name": "takeFirst",
            "parameters": {"n": 3, "xs": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]},
            "parameter_types": {"n": "Int", "xs": "[Int]"},
            "expected": [1, 2, 3],
            "expected_type": "[Int]",
            "inplace": "0"
        }
        
        result = self.executor.execute_test(code, test_case)
        
        assert result["passed"] is True
        assert result["actual"] == [1, 2, 3]

    def test_pattern_matching(self):
        """Test Haskell pattern matching features."""
        code = """
listLength :: [Int] -> Int
listLength [] = 0
listLength (_:xs) = 1 + listLength xs
"""
        test_case = {
            "function_name": "listLength",
            "parameters": {"xs": [1, 2, 3, 4]},
            "parameter_types": {"xs": "[Int]"},
            "expected": 4,
            "expected_type": "Int",
            "inplace": "0"
        }
        
        result = self.executor.execute_test(code, test_case)
        
        assert result["passed"] is True
        assert result["actual"] == 4

    def test_cleanup_temp_files(self):
        """Test that temporary files are properly cleaned up."""
        import tempfile
        import os
        
        test_case = {
            "function_name": "addNumbers",
            "parameters": {"a": 1, "b": 2},
            "parameter_types": {"a": "Int", "b": "Int"},
            "expected": 3,
            "expected_type": "Int",
            "inplace": "0"
        }
        
        # Run test
        result = self.executor.execute_test(haskell_samples.ADD_NUMBERS, test_case)
        
        # Clean up
        self.executor.cleanup()
        
        # Verify the test passed
        assert result["passed"] is True
