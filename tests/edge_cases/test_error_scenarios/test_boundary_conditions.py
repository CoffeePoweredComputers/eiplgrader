"""Test boundary conditions and edge cases for inputs and outputs."""

import pytest
from eiplgrader.tester import CodeTester
from eiplgrader.languages import language_registry


class TestBoundaryConditions:
    """Test boundary conditions for various input and output scenarios."""

    def test_empty_string_inputs_python(self):
        """Test functions with empty string inputs."""
        string_function_code = """
def process_string(s):
    return len(s)
"""
        test_cases = [
            {"parameters": {"s": ""}, "expected": 0},
            {"parameters": {"s": "a"}, "expected": 1},
            {"parameters": {"s": "a" * 10000}, "expected": 10000},
        ]

        tester = CodeTester(
            code=string_function_code,
            test_cases=test_cases,
            function_name="process_string",
            language="python",
        )

        result = tester.run_tests()
        print(result.test_results)
        assert result.was_successful()
        assert result.successes == 3

    def test_empty_list_inputs_python(self):
        """Test functions with empty list inputs."""
        list_function_code = """
def sum_list(numbers):
    if not numbers:
        return 0
    return sum(numbers)
"""
        test_cases = [
            # Empty list
            {"parameters": {"numbers": []}, "expected": 0},
            # Single element
            {"parameters": {"numbers": [5]}, "expected": 5},
            # Multiple elements
            {"parameters": {"numbers": [1, 2, 3, 4, 5]}, "expected": 15},
        ]

        tester = CodeTester(
            code=list_function_code,
            test_cases=test_cases,
            function_name="sum_list",
            language="python",
        )

        result = tester.run_tests()
        assert result.was_successful()
        assert result.successes == 3

    def test_zero_inputs_python(self):
        """Test functions with zero inputs."""
        arithmetic_code = """
def divide_numbers(a, b):
    if b == 0:
        return None  # Handle division by zero
    return a / b
"""
        test_cases = [
            # Division by zero
            {"parameters": {"a": 10, "b": 0}, "expected": None},
            # Zero dividend
            {"parameters": {"a": 0, "b": 5}, "expected": 0.0},
            # Both zero
            {"parameters": {"a": 0, "b": 0}, "expected": None},
        ]

        tester = CodeTester(
            code=arithmetic_code,
            test_cases=test_cases,
            function_name="divide_numbers",
            language="python",
        )

        result = tester.run_tests()
        assert result.was_successful()
        assert result.successes == 3

    def test_negative_inputs_python(self):
        """Test functions with negative inputs."""
        absolute_code = """
def absolute_value(x):
    return abs(x)
"""
        test_cases = [
            # Negative integer
            {"parameters": {"x": -5}, "expected": 5},
            # Negative float
            {"parameters": {"x": -3.14}, "expected": 3.14},
            # Zero
            {"parameters": {"x": 0}, "expected": 0},
            # Positive
            {"parameters": {"x": 7}, "expected": 7},
        ]

        tester = CodeTester(
            code=absolute_code,
            test_cases=test_cases,
            function_name="absolute_value",
            language="python",
        )

        result = tester.run_tests()
        assert result.was_successful()
        assert result.successes == 4

    def test_very_large_numbers_python(self):
        """Test functions with very large numbers."""
        large_number_code = """
def add_large_numbers(a, b):
    return a + b
"""
        test_cases = [
            # Very large integers
            {"parameters": {"a": 10**18, "b": 10**18}, "expected": 2 * 10**18},
            # Maximum float values
            {
                "parameters": {"a": 1.7976931348623157e308, "b": 0},
                "expected": 1.7976931348623157e308,
            },
            # Large negative numbers
            {"parameters": {"a": -(10**15), "b": -(10**15)}, "expected": -2 * 10**15},
        ]

        tester = CodeTester(
            code=large_number_code,
            test_cases=test_cases,
            function_name="add_large_numbers",
            language="python",
        )

        result = tester.run_tests()
        assert result.was_successful()
        assert result.successes == 3

    def test_unicode_string_inputs_python(self):
        """Test functions with Unicode string inputs."""
        unicode_code = """
def count_characters(s):
    return len(s)
"""
        test_cases = [
            # ASCII
            {"parameters": {"s": "hello"}, "expected": 5},
            # Unicode characters
            {"parameters": {"s": "héllo"}, "expected": 5},
            # Emoji
            {"parameters": {"s": "hello 😀"}, "expected": 7},
            # Chinese characters
            {"parameters": {"s": "你好"}, "expected": 2},
            # Mixed Unicode
            {"parameters": {"s": "test测试🎉"}, "expected": 7},
        ]

        tester = CodeTester(
            code=unicode_code,
            test_cases=test_cases,
            function_name="count_characters",
            language="python",
        )

        result = tester.run_tests()
        assert result.was_successful()
        assert result.successes == 5

    def test_none_inputs_python(self):
        """Test functions with None inputs."""
        none_handling_code = """
def handle_none(value):
    if value is None:
        return "none"
    return str(value)
"""
        test_cases = [
            {"parameters": {"value": None}, "expected": "none"},
            {"parameters": {"value": ""}, "expected": ""},
            {"parameters": {"value": 0}, "expected": "0"},
            {"parameters": {"value": False}, "expected": "False"},
        ]

        tester = CodeTester(
            code=none_handling_code,
            test_cases=test_cases,
            function_name="handle_none",
            language="python",
        )

        result = tester.run_tests()
        assert result.was_successful()
        assert result.successes == 4

    def test_boolean_edge_cases_python(self):
        """Test functions with boolean edge cases."""
        boolean_code = """
def logical_and(a, b):
    return a and b
"""
        test_cases = [
            {"parameters": {"a": True, "b": True}, "expected": True},
            {"parameters": {"a": True, "b": False}, "expected": False},
            {"parameters": {"a": False, "b": True}, "expected": False},
            {"parameters": {"a": False, "b": False}, "expected": False},
            {"parameters": {"a": 1, "b": 2}, "expected": 2},
            {"parameters": {"a": 0, "b": 5}, "expected": 0},
            {"parameters": {"a": "", "b": "hello"}, "expected": ""},
            {"parameters": {"a": "hello", "b": ""}, "expected": ""},
        ]

        tester = CodeTester(
            code=boolean_code,
            test_cases=test_cases,
            function_name="logical_and",
            language="python",
        )

        result = tester.run_tests()
        assert result.was_successful()
        assert result.successes == 8

    def test_nested_data_structures_python(self):
        """Test functions with deeply nested data structures."""
        nested_code = """
def flatten_list(nested_list):
    result = []
    for item in nested_list:
        if isinstance(item, list):
            result.extend(flatten_list(item))
        else:
            result.append(item)
    return result
"""
        test_cases = [
            # Simple nested list
            {"parameters": {"nested_list": [[1, 2], [3, 4]]}, "expected": [1, 2, 3, 4]},
            # Deeply nested
            {
                "parameters": {"nested_list": [[[1]], [[2, 3]], [4]]},
                "expected": [1, 2, 3, 4],
            },
            # Empty nested lists
            {"parameters": {"nested_list": [[], [1], []]}, "expected": [1]},
            # Single level
            {"parameters": {"nested_list": [1, 2, 3]}, "expected": [1, 2, 3]},
        ]

        tester = CodeTester(
            code=nested_code,
            test_cases=test_cases,
            function_name="flatten_list",
            language="python",
        )

        result = tester.run_tests()
        assert result.was_successful()
        assert result.successes == 4

    def test_special_float_values_python(self):
        """Test functions with special float values (inf, nan)."""
        float_handling_code = """
import math

def handle_special_floats(x):
    if math.isnan(x):
        return "nan"
    elif math.isinf(x):
        if x > 0:
            return "positive_inf"
        else:
            return "negative_inf"
    else:
        return x
"""
        test_cases = [
            {"parameters": {"x": float("inf")}, "expected": "positive_inf"},
            {"parameters": {"x": float("-inf")}, "expected": "negative_inf"},
            {"parameters": {"x": float("nan")}, "expected": "nan"},
            {"parameters": {"x": 3.14}, "expected": 3.14},
            {"parameters": {"x": 0.0}, "expected": 0.0},
        ]

        tester = CodeTester(
            code=float_handling_code,
            test_cases=test_cases,
            function_name="handle_special_floats",
            language="python",
        )

        result = tester.run_tests()
        assert result.was_successful()
        assert result.successes == 5

    def test_string_with_special_characters_python(self):
        """Test functions with strings containing special characters."""
        special_char_code = """
def process_special_string(s):
    # Count non-whitespace characters
    return len([c for c in s if not c.isspace()])
"""
        test_cases = [
            # Newlines and tabs
            {"parameters": {"s": "hello\\nworld\\t"}, "expected": 10},
            # Special characters
            {"parameters": {"s": "!@#$%^&*()"}, "expected": 10},
            # Mixed
            {"parameters": {"s": "test\r\n123!@#"}, "expected": 10},
            # Only whitespace
            {"parameters": {"s": "   \\n\\t\\r  "}, "expected": 0},
        ]

        tester = CodeTester(
            code=special_char_code,
            test_cases=test_cases,
            function_name="process_special_string",
            language="python",
        )

        result = tester.run_tests()
        assert result.was_successful()
        assert result.successes == 4

    def test_dictionary_edge_cases_python(self):
        """Test functions with dictionary edge cases."""
        dict_code = """
def merge_dicts(dict1, dict2):
    result = dict1.copy()
    result.update(dict2)
    return result
"""
        test_cases = [
            # Empty dictionaries
            {"parameters": {"dict1": {}, "dict2": {}}, "expected": {}},
            # One empty
            {"parameters": {"dict1": {"a": 1}, "dict2": {}}, "expected": {"a": 1}},
            # Overlapping keys
            {
                "parameters": {"dict1": {"a": 1, "b": 2}, "dict2": {"b": 3, "c": 4}},
                "expected": {"a": 1, "b": 3, "c": 4},
            },
            # Complex values
            {
                "parameters": {
                    "dict1": {"list": [1, 2]},
                    "dict2": {"nested": {"x": 1}},
                },
                "expected": {"list": [1, 2], "nested": {"x": 1}},
            },
        ]

        tester = CodeTester(
            code=dict_code,
            test_cases=test_cases,
            function_name="merge_dicts",
            language="python",
        )

        result = tester.run_tests()
        assert result.was_successful()
        assert result.successes == 4

    def test_list_with_mixed_types_python(self):
        """Test functions with lists containing mixed types."""
        mixed_type_code = """
def count_by_type(items):
    type_counts = {}
    for item in items:
        type_name = type(item).__name__
        type_counts[type_name] = type_counts.get(type_name, 0) + 1
    return type_counts
"""
        test_cases = [
            # Mixed basic types
            {
                "parameters": {"items": [1, "hello", 3.14, True, None]},
                "expected": {"int": 1, "str": 1, "float": 1, "bool": 1, "NoneType": 1},
            },
            # All same type
            {"parameters": {"items": [1, 2, 3, 4, 5]}, "expected": {"int": 5}},
            # Empty list
            {"parameters": {"items": []}, "expected": {}},
            # Complex types
            {
                "parameters": {"items": [[1, 2], {"a": 1}, (3, 4)]},
                "expected": {"list": 1, "dict": 1, "tuple": 1},
            },
        ]

        tester = CodeTester(
            code=mixed_type_code,
            test_cases=test_cases,
            function_name="count_by_type",
            language="python",
        )

        result = tester.run_tests()
        assert result.was_successful()
        assert result.successes == 4

    def test_function_parameter_limits_python(self):
        """Test functions with many parameters at boundary limits."""
        many_params_code = """
def add_many_numbers(a, b, c, d, e, f, g, h, i, j):
    return a + b + c + d + e + f + g + h + i + j
"""
        test_cases = [
            {
                "parameters": {
                    "a": 1,
                    "b": 2,
                    "c": 3,
                    "d": 4,
                    "e": 5,
                    "f": 6,
                    "g": 7,
                    "h": 8,
                    "i": 9,
                    "j": 10,
                },
                "expected": 55,
            }
        ]

        tester = CodeTester(
            code=many_params_code,
            test_cases=test_cases,
            function_name="add_many_numbers",
            language="python",
        )

        result = tester.run_tests()
        assert result.was_successful()
        assert result.successes == 1

    @pytest.mark.skipif(
        not language_registry.is_supported("java"), reason="Java not available"
    )
    def test_integer_overflow_java(self):
        """Test Java integer overflow behavior."""
        overflow_java = """
public class Solution {
    public static int add_numbers(int a, int b) {
        return a + b;  // May overflow
    }
}
"""
        test_cases = [
            # Integer overflow
            {
                "parameters": {"a": 2147483647, "b": 1},  # MAX_INT + 1
                "parameter_types": {"a": "int", "b": "int"},
                "expected": -2147483648,  # Wraps to MIN_INT
                "expected_type": "int",
            }
        ]

        tester = CodeTester(
            code=overflow_java,
            test_cases=test_cases,
            function_name="add_numbers",
            language="java",
        )

        result = tester.run_tests()
        assert result.was_successful()
        assert result.successes == 1
