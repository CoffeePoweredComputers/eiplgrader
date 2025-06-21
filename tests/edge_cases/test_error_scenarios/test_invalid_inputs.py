"""Test invalid input scenarios and edge cases."""

import pytest
from eiplgrader.tester import CodeTester


class TestInvalidInputs:
    """Test invalid input scenarios that should be handled gracefully."""
    
    def test_none_code_input(self):
        """Test None as code input."""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]
        
        with pytest.raises((ValueError, TypeError)):
            tester = CodeTester(
                code=None,
                test_cases=test_cases,
                function_name="add_numbers",
                language="python"
            )
            tester.run_tests()
    
    def test_invalid_code_type(self):
        """Test invalid code type (not string or list)."""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]
        
        with pytest.raises(ValueError):
            tester = CodeTester(
                code=123,  # Invalid type
                test_cases=test_cases,
                function_name="add_numbers",
                language="python"
            )
            tester.run_tests()
    
    def test_empty_test_cases(self):
        """Test with empty test cases list."""
        valid_code = """
def add_numbers(a, b):
    return a + b
"""
        
        tester = CodeTester(
            code=valid_code,
            test_cases=[],  # Empty test cases
            function_name="add_numbers",
            language="python"
        )
        
        result = tester.run_tests()
        assert result.testsRun == 0
        assert result.was_successful()  # No tests to fail
    
    def test_none_test_cases(self):
        """Test with None as test cases."""
        valid_code = """
def add_numbers(a, b):
    return a + b
"""
        
        with pytest.raises((ValueError, TypeError)):
            tester = CodeTester(
                code=valid_code,
                test_cases=None,
                function_name="add_numbers",
                language="python"
            )
            tester.run_tests()
    
    def test_invalid_test_case_format(self):
        """Test with invalid test case format (not dictionary)."""
        valid_code = """
def add_numbers(a, b):
    return a + b
"""
        
        invalid_test_cases = [
            "not a dictionary",  # String instead of dict
            123,  # Number instead of dict
            ["a", "list"]  # List instead of dict
        ]
        
        for invalid_case in invalid_test_cases:
            with pytest.raises(ValueError):
                tester = CodeTester(
                    code=valid_code,
                    test_cases=[invalid_case],
                    function_name="add_numbers",
                    language="python"
                )
                tester.run_tests()
    
    def test_missing_parameters_key(self):
        """Test test case missing 'parameters' key."""
        valid_code = """
def add_numbers(a, b):
    return a + b
"""
        
        invalid_test_case = [{"expected": 3}]  # Missing 'parameters'
        
        with pytest.raises(ValueError, match="must contain 'parameters' key"):
            tester = CodeTester(
                code=valid_code,
                test_cases=invalid_test_case,
                function_name="add_numbers",
                language="python"
            )
            tester.run_tests()
    
    def test_missing_expected_key(self):
        """Test test case missing 'expected' key."""
        valid_code = """
def add_numbers(a, b):
    return a + b
"""
        
        invalid_test_case = [{"parameters": {"a": 1, "b": 2}}]  # Missing 'expected'
        
        with pytest.raises(ValueError, match="must contain 'expected' key"):
            tester = CodeTester(
                code=valid_code,
                test_cases=invalid_test_case,
                function_name="add_numbers",
                language="python"
            )
            tester.run_tests()
    
    def test_missing_types_for_static_language(self):
        """Test missing type information for static languages."""
        valid_java_code = """
public class Solution {
    public static int add_numbers(int a, int b) {
        return a + b;
    }
}
"""
        
        # Missing parameter_types and expected_type
        invalid_test_case = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]
        
        with pytest.raises(ValueError, match="parameter_types.*for java"):
            tester = CodeTester(
                code=valid_java_code,
                test_cases=invalid_test_case,
                function_name="add_numbers",
                language="java"
            )
            tester.run_tests()
    
    def test_partial_types_for_static_language(self):
        """Test partial type information for static languages."""
        valid_java_code = """
public class Solution {
    public static int add_numbers(int a, int b) {
        return a + b;
    }
}
"""
        
        # Missing expected_type
        invalid_test_case = [{
            "parameters": {"a": 1, "b": 2},
            "parameter_types": {"a": "int", "b": "int"},
            "expected": 3
        }]
        
        with pytest.raises(ValueError, match="expected_type.*for java"):
            tester = CodeTester(
                code=valid_java_code,
                test_cases=invalid_test_case,
                function_name="add_numbers",
                language="java"
            )
            tester.run_tests()
    
    def test_missing_parameter_type(self):
        """Test missing type for a specific parameter in static language."""
        valid_java_code = """
public class Solution {
    public static int add_numbers(int a, int b) {
        return a + b;
    }
}
"""
        
        # Missing type for parameter 'b'
        invalid_test_case = [{
            "parameters": {"a": 1, "b": 2},
            "parameter_types": {"a": "int"},  # Missing 'b'
            "expected": 3,
            "expected_type": "int"
        }]
        
        with pytest.raises(ValueError, match="Missing type for parameter 'b'"):
            tester = CodeTester(
                code=valid_java_code,
                test_cases=invalid_test_case,
                function_name="add_numbers",
                language="java"
            )
            tester.run_tests()
    
    def test_unsupported_language(self):
        """Test unsupported language."""
        valid_code = """
def add_numbers(a, b):
    return a + b
"""
        
        with pytest.raises(ValueError, match="Unsupported language"):
            tester = CodeTester(
                code=valid_code,
                test_cases=[{"parameters": {"a": 1, "b": 2}, "expected": 3}],
                function_name="add_numbers",
                language="unsupported_language"
            )
            tester.run_tests()
    
    def test_empty_function_name(self):
        """Test empty function name."""
        valid_code = """
def add_numbers(a, b):
    return a + b
"""
        
        tester = CodeTester(
            code=valid_code,
            test_cases=[{"parameters": {"a": 1, "b": 2}, "expected": 3}],
            function_name="",  # Empty function name
            language="python"
        )
        
        result = tester.run_tests()
        assert not result.was_successful()
        assert result.errors > 0
    
    def test_none_function_name(self):
        """Test None as function name."""
        valid_code = """
def add_numbers(a, b):
    return a + b
"""
        
        # Should use default behavior or raise error
        tester = CodeTester(
            code=valid_code,
            test_cases=[{"parameters": {"a": 1, "b": 2}, "expected": 3}],
            function_name=None,  # None function name
            language="python"
        )
        
        result = tester.run_tests()
        # This might work with default "foo" function name or fail
        # Behavior depends on implementation
    
    def test_special_characters_in_function_name(self):
        """Test special characters in function name."""
        valid_code = """
def add_numbers(a, b):
    return a + b
"""
        
        special_names = [
            "function-with-dashes",
            "function.with.dots", 
            "function with spaces",
            "function@with@symbols",
            "function!with!exclamation"
        ]
        
        for special_name in special_names:
            tester = CodeTester(
                code=valid_code,
                test_cases=[{"parameters": {"a": 1, "b": 2}, "expected": 3}],
                function_name=special_name,
                language="python"
            )
            
            result = tester.run_tests()
            # Should fail because function name doesn't match
            assert not result.was_successful()
    
    def test_extremely_long_function_name(self):
        """Test extremely long function name."""
        valid_code = """
def add_numbers(a, b):
    return a + b
"""
        
        extremely_long_name = "a" * 1000  # 1000 character function name
        
        tester = CodeTester(
            code=valid_code,
            test_cases=[{"parameters": {"a": 1, "b": 2}, "expected": 3}],
            function_name=extremely_long_name,
            language="python"
        )
        
        result = tester.run_tests()
        assert not result.was_successful()
    
    def test_unicode_function_name(self):
        """Test Unicode characters in function name."""
        valid_code = """
def add_numbers(a, b):
    return a + b
"""
        
        unicode_names = [
            "函数名称",  # Chinese
            "función",   # Spanish with accent
            "😀_function",  # Emoji
            "αβγ_function"  # Greek letters
        ]
        
        for unicode_name in unicode_names:
            tester = CodeTester(
                code=valid_code,
                test_cases=[{"parameters": {"a": 1, "b": 2}, "expected": 3}],
                function_name=unicode_name,
                language="python"
            )
            
            result = tester.run_tests()
            # Should fail because function name doesn't match
            assert not result.was_successful()
    
    def test_invalid_inplace_value(self):
        """Test invalid inplace parameter values."""
        valid_code = """
def add_numbers(a, b):
    return a + b
"""
        
        invalid_inplace_values = [
            "invalid",
            "3",
            "-1",
            "true",
            None
        ]
        
        for invalid_value in invalid_inplace_values:
            tester = CodeTester(
                code=valid_code,
                test_cases=[{"parameters": {"a": 1, "b": 2}, "expected": 3}],
                function_name="add_numbers",
                language="python",
                inplace=invalid_value
            )
            
            # Should either work with default or handle gracefully
            # Implementation details may vary
            result = tester.run_tests()
            # Test should complete without crashing
