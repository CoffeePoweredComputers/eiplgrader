"""Test JavaScript language adapter and executor."""

import pytest
from eiplgrader.languages.adapters.javascript_adapter import JavascriptAdapter
from eiplgrader.languages.executors.javascript_executor import JavascriptExecutor


class TestJavascriptAdapter:
    """Test JavaScript adapter functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.adapter = JavascriptAdapter()

    def test_get_config(self):
        """Test language configuration."""
        config = self.adapter.get_config()
        assert config.name == "javascript"
        assert config.display_name == "JavaScript"
        assert config.file_extensions == [".js"]
        assert config.run_command == ["node"]

    def test_generate_prompt_cgbg(self):
        """Test CGBG prompt generation."""
        prompt = self.adapter.generate_prompt(
            student_response="adds two numbers together",
            function_name="add",
            gen_type="cgbg",
        )

        assert "introductory CS student learning JavaScript" in prompt
        assert "function add that adds two numbers together" in prompt
        assert "```javascript" in prompt
        assert "ES6+ syntax" in prompt

    def test_generate_prompt_redef(self):
        """Test function redefinition prompt generation."""
        prompt = self.adapter.generate_prompt(
            student_response="",  # Not used in redef
            function_name="multiply",
            gen_type="redef",
            params="a, b",
            assumptions="a and b are numbers",
        )

        assert "function multiply(a, b)" in prompt
        assert "a and b are numbers" in prompt
        assert "arrow function syntax" in prompt

    def test_generate_prompt_multiple_versions(self):
        """Test prompt with multiple versions."""
        prompt = self.adapter.generate_prompt(
            student_response="returns the sum", function_name="sum", num_to_gen=3
        )

        assert "Generate 3 different versions" in prompt

    def test_extract_code_markdown_blocks(self):
        """Test extracting code from markdown blocks."""
        response = """Here's the function:
```javascript
function add(a, b) {
    return a + b;
}
```

And here's another version:
```javascript
const add = (a, b) => a + b;
```"""

        codes = self.adapter.extract_code(response)
        assert len(codes) == 2
        assert "function add(a, b)" in codes[0]
        assert "const add = (a, b) => a + b" in codes[1]

    def test_extract_code_no_markdown(self):
        """Test extracting code without markdown blocks."""
        response = """function multiply(x, y) {
    return x * y;
}

const divide = (a, b) => {
    return a / b;
};"""

        codes = self.adapter.extract_code(response)
        assert len(codes) >= 1
        assert any("multiply" in code for code in codes)

    def test_extract_code_async_functions(self):
        """Test extracting async functions."""
        response = """```javascript
async function fetchData(url) {
    const response = await fetch(url);
    return response.json();
}
```"""

        codes = self.adapter.extract_code(response)
        assert len(codes) == 1
        assert "async function fetchData" in codes[0]
        assert "await fetch" in codes[0]

    def test_validate_syntax_valid(self):
        """Test syntax validation with valid code."""
        valid_code = """
function isEven(n) {
    return n % 2 === 0;
}
"""
        is_valid, error = self.adapter.validate_syntax(valid_code)
        assert is_valid is True
        assert error is None

    def test_validate_syntax_invalid(self):
        """Test syntax validation with invalid code."""
        invalid_code = """
function broken(x {  // Missing closing parenthesis
    return x + 1;
}
"""
        is_valid, error = self.adapter.validate_syntax(invalid_code)
        assert is_valid is False
        assert error is not None
        assert "Syntax error" in error


class TestJavascriptExecutor:
    """Test JavaScript executor functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.executor = JavascriptExecutor()

    def teardown_method(self):
        """Clean up after tests."""
        self.executor.cleanup()

    def test_prepare_code_normal_mode(self):
        """Test code preparation for normal execution mode."""
        code = "function add(a, b) { return a + b; }"
        test_case = {
            "function_name": "add",
            "parameters": {"a": 2, "b": 3},
            "expected": 5,
            "inplace": "0",
        }

        prepared = self.executor.prepare_code(code, test_case)
        assert "function add(a, b) { return a + b; }" in prepared
        assert "await add(...args)" in prepared
        assert 'inplaceMode === "0"' in prepared

    def test_execute_simple_function(self):
        """Test executing a simple function."""
        code = """
function multiply(x, y) {
    return x * y;
}
"""
        test_case = {
            "function_name": "multiply",
            "parameters": {"x": 4, "y": 5},
            "expected": 20,
        }

        result = self.executor.execute_test(code, test_case)
        assert result["passed"] is True
        assert result["actual"] == 20
        assert result["expected"] == 20
        assert "multiply(4, 5)" in result.get("function_call", "")

    def test_execute_arrow_function(self):
        """Test executing an arrow function."""
        code = """
const square = (n) => n * n;
"""
        test_case = {"function_name": "square", "parameters": {"n": 7}, "expected": 49}

        result = self.executor.execute_test(code, test_case)
        assert result["passed"] is True
        assert result["actual"] == 49

    def test_execute_async_function(self):
        """Test executing an async function."""
        code = """
async function delayed(value) {
    return new Promise(resolve => {
        setTimeout(() => resolve(value * 2), 10);
    });
}
"""
        test_case = {
            "function_name": "delayed",
            "parameters": {"value": 5},
            "expected": 10,
        }

        result = self.executor.execute_test(code, test_case)
        assert result["passed"] is True
        assert result["actual"] == 10

    def test_execute_inplace_mode_1(self):
        """Test executing with in-place modification (mode 1)."""
        code = """
function sortArray(arr) {
    arr.sort((a, b) => a - b);
}
"""
        test_case = {
            "function_name": "sortArray",
            "parameters": {"arr": [3, 1, 4, 1, 5, 9]},
            "expected": [1, 1, 3, 4, 5, 9],
            "inplace": "1",
        }

        result = self.executor.execute_test(code, test_case)
        assert result["passed"] is True
        assert result["actual"] == [1, 1, 3, 4, 5, 9]

    def test_execute_inplace_mode_2(self):
        """Test executing with in-place modification and return (mode 2)."""
        code = """
function reverseAndCount(arr) {
    arr.reverse();
    return arr.length;
}
"""
        test_case = {
            "function_name": "reverseAndCount",
            "parameters": {"arr": [1, 2, 3, 4]},
            "expected": 4,
            "inplace": "2",
        }

        result = self.executor.execute_test(code, test_case)
        assert result["passed"] is True
        assert result["actual"] == 4

    def test_execute_missing_function(self):
        """Test executing when function is missing."""
        code = "// No function defined"
        test_case = {"function_name": "missing", "parameters": {}, "expected": None}

        result = self.executor.execute_test(code, test_case)
        assert result["passed"] is False
        assert "not defined" in result["error"]

    def test_execute_runtime_error(self):
        """Test executing with runtime error."""
        code = """
function divide(a, b) {
    if (b === 0) throw new Error("Division by zero");
    return a / b;
}
"""
        test_case = {
            "function_name": "divide",
            "parameters": {"a": 10, "b": 0},
            "expected": None,
        }

        result = self.executor.execute_test(code, test_case)
        assert result["passed"] is False
        assert "Division by zero" in result["error"]

    def test_execute_with_objects(self):
        """Test executing with object parameters and return values."""
        code = """
function mergeObjects(obj1, obj2) {
    return {...obj1, ...obj2};
}
"""
        test_case = {
            "function_name": "mergeObjects",
            "parameters": {"obj1": {"a": 1, "b": 2}, "obj2": {"b": 3, "c": 4}},
            "expected": {"a": 1, "b": 3, "c": 4},
        }

        result = self.executor.execute_test(code, test_case)
        assert result["passed"] is True
        assert result["actual"] == {"a": 1, "b": 3, "c": 4}

    def test_execute_with_arrays(self):
        """Test executing with array parameters."""
        code = """
function sum(numbers) {
    return numbers.reduce((a, b) => a + b, 0);
}
"""
        test_case = {
            "function_name": "sum",
            "parameters": {"numbers": [1, 2, 3, 4, 5]},
            "expected": 15,
        }

        result = self.executor.execute_test(code, test_case)
        assert result["passed"] is True
        assert result["actual"] == 15
