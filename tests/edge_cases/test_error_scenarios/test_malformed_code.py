"""Test malformed code scenarios across all languages."""

import pytest
from eiplgrader.tester import CodeTester
from eiplgrader.languages import language_registry


class TestMalformedCode:
    """Test malformed code scenarios that should fail gracefully."""
    
    def test_syntax_error_python(self):
        """Test Python code with syntax errors."""
        malformed_code = """
def add_numbers(a, b)
    return a + b  # Missing colon
"""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]
        
        tester = CodeTester(
            code=malformed_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python"
        )
        
        result = tester.run_tests()
        assert not result.was_successful()
        assert result.errors > 0 or result.failures > 0
        # Check that error message indicates syntax issue
        error_msg = str(result.test_results[0]["error"]).lower()
        syntax_indicators = ["syntax", "invalid syntax", "expected", "unexpected token"]
        assert any(indicator in error_msg for indicator in syntax_indicators)
    
    def test_missing_function_python(self):
        """Test code that doesn't contain the expected function."""
        code_without_function = """
def wrong_function_name(a, b):
    return a + b
"""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]
        
        tester = CodeTester(
            code=code_without_function,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python"
        )
        
        result = tester.run_tests()
        assert not result.was_successful()
        assert result.errors > 0 or result.failures > 0
    
    def test_indentation_error_python(self):
        """Test Python code with indentation errors."""
        malformed_code = """
def add_numbers(a, b):
return a + b  # Wrong indentation
"""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]
        
        tester = CodeTester(
            code=malformed_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python"
        )
        
        result = tester.run_tests()
        assert not result.was_successful()
        assert result.errors > 0 or result.failures > 0
    
    def test_runtime_error_python(self):
        """Test Python code that compiles but fails at runtime."""
        runtime_error_code = """
def divide_by_zero(a):
    return a / 0
"""
        test_cases = [{"parameters": {"a": 10}, "expected": 0}]
        
        tester = CodeTester(
            code=runtime_error_code,
            test_cases=test_cases,
            function_name="divide_by_zero",
            language="python"
        )
        
        result = tester.run_tests()
        assert not result.was_successful()
        assert result.errors > 0 or result.failures > 0 or result.failures > 0
    
    def test_wrong_function_signature_python(self):
        """Test function with wrong signature."""
        wrong_signature_code = """
def add_numbers(a, b, c):  # Expected 2 params, got 3
    return a + b + c
"""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]
        
        tester = CodeTester(
            code=wrong_signature_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python"
        )
        
        result = tester.run_tests()
        assert not result.was_successful()
    
    @pytest.mark.skipif(not language_registry.is_supported("java"), 
                       reason="Java not available")
    def test_compilation_error_java(self):
        """Test Java code with compilation errors."""
        malformed_java_code = """
public class Solution {
    public static int add_numbers(int a, int b) {
        return a + b  // Missing semicolon
    }
}
"""
        test_cases = [{
            "parameters": {"a": 1, "b": 2},
            "parameter_types": {"a": "int", "b": "int"},
            "expected": 3,
            "expected_type": "int"
        }]
        
        tester = CodeTester(
            code=malformed_java_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="java"
        )
        
        result = tester.run_tests()
        assert not result.was_successful()
        assert result.errors > 0 or result.failures > 0
        assert any("compilation" in str(test_result.get("error", "")).lower() 
                  for test_result in result.test_results)
    
    @pytest.mark.skipif(not language_registry.is_supported("cpp"), 
                       reason="C++ not available")
    def test_compilation_error_cpp(self):
        """Test C++ code with compilation errors."""
        malformed_cpp_code = """
#include <iostream>
int add_numbers(int a, int b) {
    return a + b  // Missing semicolon
}
"""
        test_cases = [{
            "parameters": {"a": 1, "b": 2},
            "parameter_types": {"a": "int", "b": "int"},
            "expected": 3,
            "expected_type": "int"
        }]
        
        tester = CodeTester(
            code=malformed_cpp_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="cpp"
        )
        
        result = tester.run_tests()
        assert not result.was_successful()
        assert result.errors > 0 or result.failures > 0
    
    @pytest.mark.skipif(not language_registry.is_supported("go"), 
                       reason="Go not available")
    def test_compilation_error_go(self):
        """Test Go code with compilation errors."""
        malformed_go_code = """
package main
import "fmt"

func add_numbers(a int, b int) int {
    return a + b  // Missing expected semicolon or newline
    fmt.Println("unreachable")
"""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]
        
        tester = CodeTester(
            code=malformed_go_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="go"
        )
        
        result = tester.run_tests()
        assert not result.was_successful()
        assert result.errors > 0 or result.failures > 0
    
    def test_empty_code(self):
        """Test completely empty code."""
        empty_code = ""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]
        
        tester = CodeTester(
            code=empty_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python"
        )
        
        result = tester.run_tests()
        assert not result.was_successful()
        assert result.errors > 0 or result.failures > 0
    
    def test_whitespace_only_code(self):
        """Test code with only whitespace."""
        whitespace_code = "   \n\n\t  \n  "
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]
        
        tester = CodeTester(
            code=whitespace_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python"
        )
        
        result = tester.run_tests()
        assert not result.was_successful()
        assert result.errors > 0 or result.failures > 0
    
    def test_multiple_functions_wrong_name(self):
        """Test code with multiple functions but wrong target function name."""
        multiple_functions_code = """
def helper_function(x):
    return x * 2

def another_function(a, b):
    return a + b

def yet_another_function(x, y, z):
    return x + y + z
"""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]
        
        tester = CodeTester(
            code=multiple_functions_code,
            test_cases=test_cases,
            function_name="target_function",  # Function doesn't exist
            language="python"
        )
        
        result = tester.run_tests()
        assert not result.was_successful()
        assert result.errors > 0 or result.failures > 0
    
    def test_invalid_python_keywords(self):
        """Test code using invalid Python constructs."""
        invalid_code = """
def add_numbers(a, b):
    return a + b + nonexistent_variable
"""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]
        
        tester = CodeTester(
            code=invalid_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python"
        )
        
        result = tester.run_tests()
        assert not result.was_successful()
    
    def test_import_error_python(self):
        """Test code that tries to import non-existent modules."""
        import_error_code = """
import nonexistent_module

def add_numbers(a, b):
    return nonexistent_module.add(a, b)
"""
        test_cases = [{"parameters": {"a": 1, "b": 2}, "expected": 3}]
        
        tester = CodeTester(
            code=import_error_code,
            test_cases=test_cases,
            function_name="add_numbers",
            language="python"
        )
        
        result = tester.run_tests()
        assert not result.was_successful()
        assert result.errors > 0 or result.failures > 0
