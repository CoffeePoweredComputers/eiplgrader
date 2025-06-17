"""Tests for C++ language adapter and executor."""

import pytest
from eiplgrader.languages.adapters.cpp_adapter import CppAdapter
from eiplgrader.languages.executors.cpp_executor import CppExecutor


class TestCppAdapter:
    """Test C++ adapter functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.adapter = CppAdapter()

    def test_get_config(self):
        """Test language configuration."""
        config = self.adapter.get_config()
        assert config.name == "cpp"
        assert config.display_name == "C++"
        assert ".cpp" in config.file_extensions
        assert config.compile_command == ["g++", "-std=c++17"]

    def test_generate_prompt_cgbg(self):
        """Test CGBG prompt generation."""
        prompt = self.adapter.generate_prompt(
            student_response="adds two numbers together",
            function_name="add",
            gen_type="cgbg"
        )
        
        assert "add" in prompt
        assert "adds two numbers together" in prompt
        assert "```cpp" in prompt
        assert "C++17" in prompt
        assert "STL" in prompt

    def test_generate_prompt_redef(self):
        """Test function redefinition prompt generation."""
        prompt = self.adapter.generate_prompt(
            student_response="",
            function_name="multiply",
            gen_type="redef",
            params="int a, int b",
            assumptions="a and b are positive integers",
            return_type="int"
        )
        
        assert "multiply" in prompt
        assert "int a, int b" in prompt
        assert "positive integers" in prompt
        assert "```cpp" in prompt

    def test_generate_prompt_multiple_versions(self):
        """Test prompt with multiple versions requested."""
        prompt = self.adapter.generate_prompt(
            student_response="reverses a string",
            function_name="reverse",
            num_to_gen=3
        )
        
        assert "3 different versions" in prompt

    def test_extract_code_markdown_blocks(self):
        """Test extracting code from markdown blocks."""
        llm_response = """Here's the function:

```cpp
#include <iostream>

int add(int a, int b) {
    return a + b;
}
```

And here's another version:

```cpp
#include <iostream>

int add(int x, int y) {
    return x + y;
}
```
"""
        
        codes = self.adapter.extract_code(llm_response)
        assert len(codes) == 2
        assert "int add(int a, int b)" in codes[0]
        assert "int add(int x, int y)" in codes[1]

    def test_extract_code_cpp_lowercase(self):
        """Test extracting code from c++ (lowercase) blocks."""
        llm_response = """Here's the function:

```c++
int multiply(int a, int b) {
    return a * b;
}
```
"""
        
        codes = self.adapter.extract_code(llm_response)
        assert len(codes) == 1
        assert "multiply" in codes[0]

    def test_extract_code_no_markdown(self):
        """Test extracting code without markdown blocks."""
        llm_response = """
int divide(int a, int b) {
    if (b == 0) return 0;
    return a / b;
}
"""
        
        codes = self.adapter.extract_code(llm_response)
        assert len(codes) == 1
        assert "divide" in codes[0]

    def test_extract_code_with_templates(self):
        """Test extracting template functions."""
        llm_response = """```cpp
template<typename T>
T max(T a, T b) {
    return (a > b) ? a : b;
}
```"""
        
        codes = self.adapter.extract_code(llm_response)
        assert len(codes) == 1
        assert "template" in codes[0]
        assert "typename T" in codes[0]

    def test_validate_syntax_valid_code(self):
        """Test syntax validation with valid code."""
        code = """
#include <iostream>

int factorial(int n) {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}
"""
        
        is_valid, error = self.adapter.validate_syntax(code)
        # Note: This might fail if g++ is not available
        # In that case, it should return (True, None)
        assert is_valid or error is None

    def test_validate_syntax_invalid_code(self):
        """Test syntax validation with invalid code."""
        code = """
int broken(int a {  // Missing closing parenthesis
    return a + ;  // Incomplete expression
}
"""
        
        is_valid, error = self.adapter.validate_syntax(code)
        # If g++ is available, this should fail
        # If not available, it returns (True, None)
        if error is not None:
            assert not is_valid
            assert "error" in error.lower()


class TestCppExecutor:
    """Test C++ executor functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.executor = CppExecutor()

    def teardown_method(self):
        """Clean up after tests."""
        self.executor.cleanup()

    def test_simple_function(self):
        """Test executing a simple C++ function."""
        code = """
int add(int a, int b) {
    return a + b;
}
"""
        
        test_case = {
            "function_name": "add",
            "parameters": {"a": 5, "b": 3},
            "expected": 8,
            "inplace": "0"
        }
        
        result = self.executor.execute_test(code, test_case)
        assert result["passed"]
        assert result["actual"] == 8

    def test_string_function(self):
        """Test function with string parameters."""
        code = """
#include <string>

std::string concatenate(std::string a, std::string b) {
    return a + b;
}
"""
        
        test_case = {
            "function_name": "concatenate",
            "parameters": {"a": "Hello", "b": "World"},
            "expected": "HelloWorld",
            "inplace": "0"
        }
        
        result = self.executor.execute_test(code, test_case)
        print(f"Result: {result}")
        assert result["passed"]
        assert result["actual"] == "HelloWorld"

    def test_vector_function(self):
        """Test function with vector parameters."""
        code = """
#include <vector>

int sum_vector(std::vector<int> nums) {
    int sum = 0;
    for (int n : nums) {
        sum += n;
    }
    return sum;
}
"""
        
        test_case = {
            "function_name": "sum_vector",
            "parameters": {"nums": [1, 2, 3, 4, 5]},
            "expected": 15,
            "inplace": "0"
        }
        
        result = self.executor.execute_test(code, test_case)
        assert result["passed"]
        assert result["actual"] == 15

    def test_inplace_modification(self):
        """Test function that modifies parameter in-place."""
        code = """
#include <vector>
#include <algorithm>

void sort_vector(std::vector<int>& nums) {
    std::sort(nums.begin(), nums.end());
}
"""
        
        test_case = {
            "function_name": "sort_vector",
            "parameters": {"nums": [3, 1, 4, 1, 5]},
            "expected": [1, 1, 3, 4, 5],
            "inplace": "1"
        }
        
        result = self.executor.execute_test(code, test_case)
        assert result["passed"]
        assert result["actual"] == [1, 1, 3, 4, 5]

    def test_mixed_mode_function(self):
        """Test function that both modifies in-place and returns a value."""
        code = """
#include <vector>

int reverse_and_sum(std::vector<int>& nums) {
    int sum = 0;
    int n = nums.size();
    for (int i = 0; i < n / 2; i++) {
        std::swap(nums[i], nums[n - 1 - i]);
    }
    for (int num : nums) {
        sum += num;
    }
    return sum;
}
"""
        
        test_case = {
            "function_name": "reverse_and_sum",
            "parameters": {"nums": [1, 2, 3, 4]},
            "expected": 10,  # Sum of elements
            "inplace": "2"
        }
        
        result = self.executor.execute_test(code, test_case)
        assert result["passed"]
        assert result["actual"] == 10

    def test_template_function(self):
        """Test template function execution."""
        code = """
template<typename T>
T multiply(T a, T b) {
    return a * b;
}

// Explicit instantiation for int
template int multiply<int>(int, int);
"""
        
        test_case = {
            "function_name": "multiply",
            "parameters": {"a": 4, "b": 7},
            "expected": 28,
            "inplace": "0"
        }
        
        result = self.executor.execute_test(code, test_case)
        assert result["passed"]
        assert result["actual"] == 28

    def test_compilation_error(self):
        """Test handling of compilation errors."""
        code = """
int broken(int a) {
    return a +   // Syntax error
}
"""
        
        test_case = {
            "function_name": "broken",
            "parameters": {"a": 5},
            "expected": 5
        }
        
        result = self.executor.execute_test(code, test_case)
        assert not result["passed"]
        assert "error" in result
        assert "Compilation failed" in result["error"]

    def test_runtime_error(self):
        """Test handling of runtime errors."""
        code = """
#include <vector>

int access_element(std::vector<int> vec, int index) {
    return vec.at(index);  // Will throw if index out of bounds
}
"""
        
        test_case = {
            "function_name": "access_element",
            "parameters": {"vec": [1, 2, 3], "index": 10},
            "expected": 0
        }
        
        result = self.executor.execute_test(code, test_case)
        assert not result["passed"]
        # Runtime error or unexpected output

    def test_timeout_handling(self):
        """Test handling of infinite loops/timeouts."""
        code = """
int infinite_loop(int n) {
    while (true) {
        n++;
    }
    return n;
}
"""
        
        test_case = {
            "function_name": "infinite_loop",
            "parameters": {"n": 1},
            "expected": 1,
            "timeout": 2  # 2 second timeout
        }
        
        result = self.executor.execute_test(code, test_case)
        assert not result["passed"]
        assert "timeout" in result["error"].lower()

    def test_stl_algorithm_usage(self):
        """Test function using STL algorithms."""
        code = """
#include <vector>
#include <algorithm>
#include <numeric>

double average(std::vector<int> nums) {
    if (nums.empty()) return 0.0;
    int sum = std::accumulate(nums.begin(), nums.end(), 0);
    return static_cast<double>(sum) / nums.size();
}
"""
        
        test_case = {
            "function_name": "average",
            "parameters": {"nums": [10, 20, 30, 40, 50]},
            "expected": 30.0,
            "inplace": "0"
        }
        
        result = self.executor.execute_test(code, test_case)
        assert result["passed"]
        assert result["actual"] == 30.0

    def test_modern_cpp_features(self):
        """Test function using modern C++ features."""
        code = """
#include <vector>
#include <algorithm>

auto filter_even(const std::vector<int>& nums) {
    std::vector<int> result;
    std::copy_if(nums.begin(), nums.end(), 
                 std::back_inserter(result),
                 [](int n) { return n % 2 == 0; });
    return result;
}
"""
        
        test_case = {
            "function_name": "filter_even",
            "parameters": {"nums": [1, 2, 3, 4, 5, 6]},
            "expected": [2, 4, 6],
            "inplace": "0"
        }
        
        result = self.executor.execute_test(code, test_case)
        assert result["passed"]
        assert result["actual"] == [2, 4, 6]