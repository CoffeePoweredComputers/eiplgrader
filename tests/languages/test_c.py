"""Tests for C language adapter and executor."""

import pytest
from eiplgrader.languages.adapters.c_adapter import CAdapter
from eiplgrader.languages.executors.c_executor import CExecutor


class TestCAdapter:
    """Test C language adapter."""

    def setup_method(self):
        """Set up test fixtures."""
        self.adapter = CAdapter()

    def test_get_config(self):
        """Test language configuration."""
        config = self.adapter.get_config()
        assert config.name == "c"
        assert config.display_name == "C"
        assert ".c" in config.file_extensions
        assert config.compile_command == ["gcc"]
        assert config.run_command is None  # C is compiled

    def test_generate_prompt_cgbg(self):
        """Test prompt generation for code generation based grading."""
        prompt = self.adapter.generate_prompt(
            student_response="adds two numbers together",
            function_name="add",
            gen_type="cgbg",
        )
        
        assert "add" in prompt
        assert "adds two numbers together" in prompt
        assert "```c" in prompt
        assert "#include" in prompt
        assert "stdio.h" in prompt

    def test_generate_prompt_redef(self):
        """Test prompt generation for function redefinition."""
        prompt = self.adapter.generate_prompt(
            student_response="",
            function_name="multiply",
            gen_type="redef",
            params="int a, int b",
            assumptions="a and b are positive integers",
        )
        
        assert "multiply" in prompt
        assert "int a, int b" in prompt
        assert "positive integers" in prompt

    def test_extract_code_markdown(self):
        """Test extracting code from markdown blocks."""
        response = """Here's the function:
```c
#include <stdio.h>

int add(int a, int b) {
    return a + b;
}
```
"""
        code_blocks = self.adapter.extract_code(response)
        assert len(code_blocks) == 1
        assert "int add(int a, int b)" in code_blocks[0]
        assert "#include <stdio.h>" in code_blocks[0]

    def test_extract_code_multiple(self):
        """Test extracting multiple code blocks."""
        response = """Version 1:
```c
int add(int a, int b) {
    return a + b;
}
```

Version 2:
```c
int add(int x, int y) {
    return x + y;
}
```
"""
        code_blocks = self.adapter.extract_code(response)
        assert len(code_blocks) == 2

    def test_extract_code_no_markdown(self):
        """Test extracting code without markdown blocks."""
        response = """int add(int a, int b) {
    return a + b;
}"""
        code_blocks = self.adapter.extract_code(response)
        assert len(code_blocks) == 1
        assert "int add(int a, int b)" in code_blocks[0]

    def test_validate_syntax_valid(self):
        """Test syntax validation with valid code."""
        code = """#include <stdio.h>

int add(int a, int b) {
    return a + b;
}"""
        is_valid, error = self.adapter.validate_syntax(code)
        assert is_valid
        assert error is None

    def test_validate_syntax_invalid(self):
        """Test syntax validation with invalid code."""
        code = """int add(int a, int b {  // Missing closing parenthesis
    return a + b;
}"""
        is_valid, error = self.adapter.validate_syntax(code)
        assert not is_valid
        assert error is not None


class TestCExecutor:
    """Test C language executor."""

    def setup_method(self):
        """Set up test fixtures."""
        self.executor = CExecutor()

    def teardown_method(self):
        """Clean up after tests."""
        self.executor.cleanup()

    def test_simple_addition(self):
        """Test executing a simple addition function."""
        code = """#include <stdio.h>

int add(int a, int b) {
    return a + b;
}"""
        
        test_case = {
            "function_name": "add",
            "parameters": {"a": 5, "b": 3},
            "expected": 8,
            "inplace": "0",
        }
        
        result = self.executor.execute_test(code, test_case)
        assert result["passed"]
        assert result["actual"] == 8
        assert result["expected"] == 8

    def test_string_function(self):
        """Test function returning string."""
        code = """#include <stdio.h>
#include <string.h>

char* greet(char* name) {
    static char greeting[100];
    sprintf(greeting, "Hello, %s", name);
    return greeting;
}"""
        
        test_case = {
            "function_name": "greet",
            "parameters": {"name": "World"},
            "expected": "Hello, World",
            "inplace": "0",
        }
        
        result = self.executor.execute_test(code, test_case)
        assert result["passed"]
        assert result["actual"] == "Hello, World"

    def test_array_sum(self):
        """Test function with array parameter."""
        code = """#include <stdio.h>

int sum_array(int* arr, int size) {
    int sum = 0;
    for (int i = 0; i < size; i++) {
        sum += arr[i];
    }
    return sum;
}"""
        
        test_case = {
            "function_name": "sum_array",
            "parameters": {"arr": [1, 2, 3, 4, 5], "size": 5},
            "expected": 15,
            "inplace": "0",
        }
        
        result = self.executor.execute_test(code, test_case)
        assert result["passed"]
        assert result["actual"] == 15

    def test_inplace_modification(self):
        """Test in-place array modification."""
        code = """#include <stdio.h>

void double_array(int* arr, int size) {
    for (int i = 0; i < size; i++) {
        arr[i] *= 2;
    }
}"""
        
        test_case = {
            "function_name": "double_array",
            "parameters": {"arr": [1, 2, 3], "size": 3},
            "expected": [2, 4, 6],
            "inplace": "1",
        }
        
        result = self.executor.execute_test(code, test_case)
        assert result["passed"]
        assert result["actual"] == [2, 4, 6]

    def test_swap_values(self):
        """Test pointer-based value swapping."""
        code = """#include <stdio.h>

void swap(int* a, int* b) {
    int temp = *a;
    *a = *b;
    *b = temp;
}"""
        
        test_case = {
            "function_name": "swap",
            "parameters": {"a": 5, "b": 10},
            "expected": 10,  # We expect the first parameter to be modified to 10
            "inplace": "1",
        }
        
        result = self.executor.execute_test(code, test_case)
        assert result["passed"]
        assert result["actual"] == 10

    def test_compilation_error(self):
        """Test handling of compilation errors."""
        code = """int add(int a, int b {  // Syntax error
    return a + b;
}"""
        
        test_case = {
            "function_name": "add",
            "parameters": {"a": 5, "b": 3},
            "expected": 8,
            "inplace": "0",
        }
        
        result = self.executor.execute_test(code, test_case)
        assert not result["passed"]
        assert "Compilation failed" in result["error"]

    def test_runtime_error(self):
        """Test handling of runtime errors."""
        code = """#include <stdio.h>

int divide(int a, int b) {
    return a / b;  // Will cause division by zero
}"""
        
        test_case = {
            "function_name": "divide",
            "parameters": {"a": 10, "b": 0},
            "expected": None,
            "inplace": "0",
        }
        
        result = self.executor.execute_test(code, test_case)
        assert not result["passed"]
        # Runtime error might manifest as incorrect output or signal

    def test_prepare_code_headers(self):
        """Test that prepare_code adds necessary headers."""
        code = """int add(int a, int b) {
    return a + b;
}"""
        
        test_case = {
            "function_name": "add",
            "parameters": {"a": 1, "b": 2},
            "expected": 3,
            "inplace": "0",
        }
        
        prepared = self.executor.prepare_code(code, test_case)
        assert "#include <stdio.h>" in prepared
        assert "#include <stdlib.h>" in prepared
        assert "int main()" in prepared

    def test_float_operations(self):
        """Test floating point operations."""
        code = """#include <stdio.h>

double multiply(double a, double b) {
    return a * b;
}"""
        
        test_case = {
            "function_name": "multiply",
            "parameters": {"a": 2.5, "b": 4.0},
            "expected": 10.0,
            "inplace": "0",
        }
        
        result = self.executor.execute_test(code, test_case)
        assert result["passed"]
        assert result["actual"] == 10.0