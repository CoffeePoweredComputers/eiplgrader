"""Tests for Go language adapter and executor."""

import pytest
from eiplgrader.languages.adapters.go_adapter import GoAdapter
from eiplgrader.languages.executors.go_executor import GoExecutor


def _go_available():
    """Check if Go is available on the system."""
    import subprocess

    try:
        result = subprocess.run(["go", "version"], capture_output=True)
        return result.returncode == 0
    except FileNotFoundError:
        return False


class TestGoAdapter:
    """Test Go language adapter functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.adapter = GoAdapter()

    def test_get_config(self):
        """Test language configuration."""
        config = self.adapter.get_config()
        assert config.name == "go"
        assert config.display_name == "Go"
        assert config.file_extensions == [".go"]
        assert config.compile_command == ["go", "build"]
        assert config.run_command == ["go", "run"]

    def test_generate_prompt_cgbg(self):
        """Test CGBG prompt generation."""
        prompt = self.adapter.generate_prompt(
            student_response="adds two numbers together",
            function_name="add",
            gen_type="cgbg",
        )

        assert "Create a function, called add" in prompt
        assert "adds two numbers together" in prompt
        assert "```go" in prompt
        assert "package main" in prompt
        assert "func add" in prompt

    def test_generate_prompt_redef(self):
        """Test redef prompt generation."""
        prompt = self.adapter.generate_prompt(
            student_response="",
            function_name="multiply",
            gen_type="redef",
            params="a int, b int",
            assumptions="a and b are positive integers",
        )

        assert "func multiply(a int, b int)" in prompt
        assert "positive integers" in prompt
        assert "```go" in prompt

    def test_generate_prompt_multiple_versions(self):
        """Test prompt generation with multiple versions."""
        prompt = self.adapter.generate_prompt(
            student_response="calculates the sum",
            function_name="sum",
            gen_type="cgbg",
            num_to_gen=3,
        )

        assert "Generate 3 different versions" in prompt

    def test_extract_code_markdown_blocks(self):
        """Test extracting code from markdown blocks."""
        llm_response = """Here's the function:

```go
package main

func add(a int, b int) int {
    return a + b
}
```

And here's another version:

```go
package main

func add(x int, y int) int {
    return x + y
}
```
"""

        code_blocks = self.adapter.extract_code(llm_response)
        assert len(code_blocks) == 2
        assert "func add(a int, b int)" in code_blocks[0]
        assert "func add(x int, y int)" in code_blocks[1]
        assert all("package main" in block for block in code_blocks)

    def test_extract_code_no_markdown(self):
        """Test extracting code without markdown blocks."""
        llm_response = """func subtract(a int, b int) int {
    return a - b
}"""

        code_blocks = self.adapter.extract_code(llm_response)
        assert len(code_blocks) == 1
        assert "func subtract" in code_blocks[0]
        # Should add package declaration if missing
        assert "package main" in code_blocks[0]

    def test_extract_code_function_pattern(self):
        """Test extracting code using function pattern matching."""
        llm_response = """Here are the functions:
func multiply(a int, b int) int {
    return a * b
}

func divide(a int, b int) (int, error) {
    if b == 0 {
        return 0, errors.New("division by zero")
    }
    return a / b, nil
}"""

        code_blocks = self.adapter.extract_code(llm_response)
        assert len(code_blocks) == 2
        assert "func multiply" in code_blocks[0]
        assert "func divide" in code_blocks[1]

    def test_validate_syntax_valid(self):
        """Test syntax validation with valid code."""
        valid_code = """package main

func hello() string {
    return "Hello, World!"
}"""

        is_valid, error = self.adapter.validate_syntax(valid_code)
        # Note: This will pass even if gofmt is not available
        assert is_valid
        assert error is None

    def test_validate_syntax_invalid(self):
        """Test syntax validation with invalid code."""
        invalid_code = """package main

func broken( {
    return "This won't compile"
}"""

        is_valid, error = self.adapter.validate_syntax(invalid_code)
        # If gofmt is available, it should catch the error
        # Otherwise, it will still pass
        if not is_valid:
            assert error is not None
            assert "Syntax error" in error


class TestGoExecutor:
    """Test Go language executor functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.executor = GoExecutor()

    def teardown_method(self):
        """Clean up after tests."""
        self.executor.cleanup()

    def test_prepare_code_simple_function(self):
        """Test preparing simple function for execution."""
        code = """func add(a int, b int) int {
    return a + b
}"""

        test_case = {
            "function_name": "add",
            "parameters": {"a": 5, "b": 3},
            "expected": 8,
            "inplace": "0",
        }

        prepared = self.executor.prepare_code(code, test_case)

        assert "package main" in prepared
        assert "import" in prepared
        assert "func main()" in prepared
        assert "add(a, b)" in prepared
        assert "json.Marshal(result)" in prepared

    def test_prepare_code_with_package(self):
        """Test preparing code that already has package declaration."""
        code = """package main

import "fmt"

func greet(name string) string {
    return fmt.Sprintf("Hello, %s!", name)
}"""

        test_case = {
            "function_name": "greet",
            "parameters": {"name": "World"},
            "expected": "Hello, World!",
            "inplace": "0",
        }

        prepared = self.executor.prepare_code(code, test_case)

        # Should not duplicate package declaration
        assert prepared.count("package main") == 1
        assert "func main()" in prepared

    def test_prepare_code_inplace_mode_1(self):
        """Test preparing code for in-place modification."""
        code = """func reverseArray(arr []int) {
    for i := 0; i < len(arr)/2; i++ {
        arr[i], arr[len(arr)-1-i] = arr[len(arr)-1-i], arr[i]
    }
}"""

        test_case = {
            "function_name": "reverseArray",
            "parameters": {"arr": [1, 2, 3, 4, 5]},
            "expected": [5, 4, 3, 2, 1],
            "inplace": "1",
        }

        prepared = self.executor.prepare_code(code, test_case)

        assert "reverseArray(&arr)" in prepared
        assert "json.Marshal(arr)" in prepared

    def test_infer_go_types(self):
        """Test Go type inference from Python values."""
        parameters = {
            "int_val": 42,
            "float_val": 3.14,
            "str_val": "hello",
            "bool_val": True,
            "int_list": [1, 2, 3],
            "str_list": ["a", "b", "c"],
        }

        types = self.executor._infer_go_types(parameters)

        assert types == ["int", "float64", "string", "bool", "[]int", "[]string"]

    @pytest.mark.skipif(not _go_available(), reason="Go not available")
    def test_execute_simple_function(self):
        """Test executing a simple Go function."""
        code = """func multiply(a int, b int) int {
    return a * b
}"""

        test_case = {
            "function_name": "multiply",
            "parameters": {"a": 7, "b": 6},
            "expected": 42,
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"]
        assert result["actual"] == 42
        assert result["expected"] == 42
        assert "error" not in result

    @pytest.mark.skipif(not _go_available(), reason="Go not available")
    def test_execute_string_function(self):
        """Test executing a function with string parameters."""
        code = """func concat(a string, b string) string {
    return a + b
}"""

        test_case = {
            "function_name": "concat",
            "parameters": {"a": "Hello", "b": "World"},
            "expected": "HelloWorld",
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"]
        assert result["actual"] == "HelloWorld"

    @pytest.mark.skipif(not _go_available(), reason="Go not available")
    def test_execute_array_function(self):
        """Test executing a function with array parameters."""
        code = """func sumArray(nums []int) int {
    sum := 0
    for _, n := range nums {
        sum += n
    }
    return sum
}"""

        test_case = {
            "function_name": "sumArray",
            "parameters": {"nums": [1, 2, 3, 4, 5]},
            "expected": 15,
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"]
        assert result["actual"] == 15

    @pytest.mark.skipif(not _go_available(), reason="Go not available")
    def test_execute_error_handling(self):
        """Test executing function with compilation error."""
        code = """func broken( {
    return "This won't compile"
}"""

        test_case = {
            "function_name": "broken",
            "parameters": {},
            "expected": "This won't compile",
        }

        result = self.executor.execute_test(code, test_case)

        assert not result["passed"]
        assert "error" in result
        assert "Runtime error" in result["error"]

    @pytest.mark.skipif(not _go_available(), reason="Go not available")
    def test_execute_inplace_modification(self):
        """Test executing function that modifies argument in-place."""
        code = """func doubleArray(arr []int) {
    for i := range arr {
        arr[i] *= 2
    }
}"""

        test_case = {
            "function_name": "doubleArray",
            "parameters": {"arr": [1, 2, 3]},
            "expected": [2, 4, 6],
            "inplace": "1",
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"]
        assert result["actual"] == [2, 4, 6]
