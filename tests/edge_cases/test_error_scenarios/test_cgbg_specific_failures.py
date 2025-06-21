"""Test CGBG-specific failure scenarios."""

import pytest
from eiplgrader.tester import CodeTester
from eiplgrader.languages import language_registry


class TestCGBGSpecificFailures:
    """Test failures specific to Code Generation Based Grading workflow."""

    def test_multiple_functions_generated(self):
        """Test code with multiple functions when only one is expected."""
        multiple_functions_code = """
def helper_function(x):
    return x * 2

def add_numbers(a, b):
    return a + b

def another_helper(y):
    return y - 1
"""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]

        tester = CodeTester(
            code=multiple_functions_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python",
        )

        # Should work if target function exists and is correct
        result = tester.run_tests()
        assert result.was_successful()

    def test_no_functions_generated(self):
        """Test code with no functions at all."""
        no_functions_code = """
# This is just a comment
x = 5
y = 10
print(x + y)
"""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]

        tester = CodeTester(
            code=no_functions_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python",
        )

        result = tester.run_tests()
        assert not result.was_successful()
        assert result.errors > 0

    def test_class_instead_of_function(self):
        """Test generated code contains class instead of function."""
        class_code = """
class Calculator:
    def add_numbers(self, a, b):
        return a + b
"""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]

        tester = CodeTester(
            code=class_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python",
        )

        result = tester.run_tests()
        assert not result.was_successful()
        assert result.errors > 0

    def test_function_with_wrong_return_type(self):
        """Test function that returns wrong type."""
        wrong_type_code = """
def add_numbers(a, b):
    return str(a + b)  # Returns string instead of int
"""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]

        tester = CodeTester(
            code=wrong_type_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python",
        )

        result = tester.run_tests()
        assert not result.was_successful()
        assert result.failures > 0

    def test_function_with_side_effects(self):
        """Test function that has unexpected side effects."""
        side_effects_code = """
import sys

def add_numbers(a, b):
    print(f"Computing {a} + {b}")  # Side effect: print
    sys.stdout.flush()  # Side effect: flush
    return a + b
"""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]

        tester = CodeTester(
            code=side_effects_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python",
        )

        result = tester.run_tests()
        # Should still work if return value is correct
        assert result.was_successful()

    def test_function_modifies_global_state(self):
        """Test function that modifies global state."""
        global_state_code = """
global_counter = 0

def add_numbers(a, b):
    global global_counter
    global_counter += 1
    return a + b
"""
        test_cases = [
            {"parameters": {"a": 1, "b": 2}, "expected": 3},
            {"parameters": {"a": 5, "b": 5}, "expected": 10},
        ]

        tester = CodeTester(
            code=global_state_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python",
        )

        result = tester.run_tests()
        # Should work if return values are correct
        assert result.was_successful()

    def test_function_with_imports_that_fail(self):
        """Test function that imports modules that don't exist."""
        failing_import_code = """
import nonexistent_module
from another_fake_module import fake_function

def add_numbers(a, b):
    return fake_function(a, b)
"""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]

        tester = CodeTester(
            code=failing_import_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python",
        )

        result = tester.run_tests()
        assert not result.was_successful()
        assert result.errors > 0

    def test_recursive_function_without_base_case(self):
        """Test recursive function without proper base case."""
        infinite_recursion_code = """
def add_numbers(a, b):
    return add_numbers(a, b)  # Infinite recursion
"""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]

        tester = CodeTester(
            code=infinite_recursion_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python",
        )

        result = tester.run_tests()
        assert not result.was_successful()
        assert result.errors > 0

    def test_function_with_undefined_variables(self):
        """Test function that uses undefined variables."""
        undefined_vars_code = """
def add_numbers(a, b):
    return a + b + undefined_variable
"""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]

        tester = CodeTester(
            code=undefined_vars_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python",
        )

        result = tester.run_tests()
        assert not result.was_successful()
        assert result.errors > 0

    def test_function_with_incorrect_parameter_usage(self):
        """Test function that doesn't use parameters correctly."""
        wrong_params_code = """
def add_numbers(a, b):
    return x + y  # Uses x, y instead of a, b
"""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]

        tester = CodeTester(
            code=wrong_params_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python",
        )

        result = tester.run_tests()
        assert not result.was_successful()
        assert result.errors > 0

    def test_function_ignores_parameters(self):
        """Test function that ignores input parameters."""
        ignores_params_code = """
def add_numbers(a, b):
    return 42  # Always returns 42, ignoring inputs
"""
        test_cases = [
            {"parameters": {"a": 1, "b": 2}, "expected": 3},
            {"parameters": {"a": 5, "b": 5}, "expected": 10},
        ]

        tester = CodeTester(
            code=ignores_params_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python",
        )

        result = tester.run_tests()
        assert not result.was_successful()
        assert result.failures > 0

    def test_function_with_hardcoded_output(self):
        """Test function that works for first test case but fails others."""
        hardcoded_code = """
def add_numbers(a, b):
    if a == 1 and b == 2:
        return 3
    else:
        return 999  # Wrong for other cases
"""
        test_cases = [
            {"parameters": {"a": 1, "b": 2}, "expected": 3},
            {"parameters": {"a": 3, "b": 4}, "expected": 7},
            {"parameters": {"a": 0, "b": 0}, "expected": 0},
        ]

        tester = CodeTester(
            code=hardcoded_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python",
        )

        result = tester.run_tests()
        assert not result.was_successful()
        assert result.successes == 1  # Only first test passes
        assert result.failures == 2  # Other tests fail

    @pytest.mark.skipif(
        not language_registry.is_supported("java"), reason="Java not available"
    )
    def test_java_wrong_class_name(self):
        """Test Java code with wrong class name."""
        wrong_class_code = """
public class WrongClassName {
    public static int add_numbers(int a, int b) {
        return a + b;
    }
}
"""
        test_cases = [
            {
                "parameters": {"a": 1, "b": 2},
                "parameter_types": {"a": "int", "b": "int"},
                "expected": 3,
                "expected_type": "int",
            }
        ]

        tester = CodeTester(
            code=wrong_class_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="java",
        )

        result = tester.run_tests()
        # Java executor expects "Solution" class name
        assert not result.was_successful()

    @pytest.mark.skipif(
        not language_registry.is_supported("java"), reason="Java not available"
    )
    def test_java_non_static_method(self):
        """Test Java code with non-static method."""
        non_static_code = """
public class Solution {
    public int add_numbers(int a, int b) {  // Missing static
        return a + b;
    }
}
"""
        test_cases = [
            {
                "parameters": {"a": 1, "b": 2},
                "parameter_types": {"a": "int", "b": "int"},
                "expected": 3,
                "expected_type": "int",
            }
        ]

        tester = CodeTester(
            code=non_static_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="java",
        )

        result = tester.run_tests()
        # Java executor expects static methods
        assert not result.was_successful()

    def test_mixed_language_code(self):
        """Test code that mixes multiple language syntaxes."""
        mixed_code = """
def add_numbers(a, b):
    return a + b;  // C-style semicolon and comment
    
public static void main(String[] args) {  // Java syntax
    System.out.println("Hello");
}
"""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]

        tester = CodeTester(
            code=mixed_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python",
        )

        result = tester.run_tests()
        # Might work if Python function is syntactically valid
        # But the Java code will cause syntax errors
        assert not result.was_successful()

    def test_code_with_natural_language_comments(self):
        """Test code that contains natural language that looks like instructions."""
        natural_language_code = """
# Please implement a function that adds two numbers
# The function should be called add_numbers
# It should take two parameters a and b
# And return their sum

def add_numbers(a, b):
    # TODO: Implement this function
    # First, add the two numbers
    # Then return the result
    return a + b
"""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]

        tester = CodeTester(
            code=natural_language_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python",
        )

        result = tester.run_tests()
        # Should work fine as comments are ignored
        assert result.was_successful()

    def test_incomplete_function_implementation(self):
        """Test incomplete function implementations."""
        incomplete_implementations = [
            """
def add_numbers(a, b):
    # TODO: implement this
    pass
""",
            """
def add_numbers(a, b):
    return  # Missing return value
""",
            """
def add_numbers(a, b):
    return a +  # Incomplete expression
""",
            """
def add_numbers(a, b):
    if a > 0:
        return a + b
    # Missing else case
""",
        ]

        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]

        for incomplete_code in incomplete_implementations:
            tester = CodeTester(
                code=incomplete_code,
                test_cases=test_cases,
                function_name="add_numbers",
                language="python",
            )

            result = tester.run_tests()
            assert not result.was_successful()
