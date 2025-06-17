"""Tests for Kotlin language adapter and executor."""

import pytest
from eiplgrader.languages.adapters.kotlin_adapter import KotlinAdapter
from eiplgrader.languages.executors.kotlin_executor import KotlinExecutor


def _kotlin_available():
    """Check if Kotlin is available on the system."""
    import subprocess

    try:
        result = subprocess.run(["kotlinc", "-version"], capture_output=True)
        return result.returncode == 0
    except FileNotFoundError:
        return False


class TestKotlinAdapter:
    """Test Kotlin language adapter functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.adapter = KotlinAdapter()

    def test_get_config(self):
        """Test language configuration."""
        config = self.adapter.get_config()
        assert config.name == "kotlin"
        assert config.display_name == "Kotlin"
        assert config.file_extensions == [".kt", ".kts"]
        assert config.compile_command == ["kotlinc"]
        assert config.run_command == ["kotlin"]

    def test_generate_prompt_cgbg(self):
        """Test CGBG prompt generation."""
        prompt = self.adapter.generate_prompt(
            student_response="adds two numbers together",
            function_name="add",
            gen_type="cgbg",
        )

        assert "Create a function, called add" in prompt
        assert "adds two numbers together" in prompt
        assert "```kotlin" in prompt
        assert "null safety" in prompt

    def test_generate_prompt_redef(self):
        """Test redef prompt generation."""
        prompt = self.adapter.generate_prompt(
            student_response="",
            function_name="multiply",
            gen_type="redef",
            params="a: Int, b: Int",
            assumptions="a and b are positive integers",
        )

        assert "fun multiply(a: Int, b: Int)" in prompt
        assert "positive integers" in prompt
        assert "```kotlin" in prompt

    def test_generate_prompt_multiple_versions(self):
        """Test prompt generation with multiple versions."""
        prompt = self.adapter.generate_prompt(
            student_response="calculates the sum",
            function_name="sum",
            gen_type="cgbg",
            num_to_gen=3,
        )

        assert "Generate 3 different versions" in prompt
        assert "single expression functions" in prompt

    def test_extract_code_markdown_blocks(self):
        """Test extracting code from markdown blocks."""
        llm_response = """Here's the function:

```kotlin
fun add(a: Int, b: Int): Int {
    return a + b
}
```

And here's a single-expression version:

```kotlin
fun add(x: Int, y: Int) = x + y
```
"""

        code_blocks = self.adapter.extract_code(llm_response)
        assert len(code_blocks) == 2
        assert "fun add(a: Int, b: Int): Int" in code_blocks[0]
        assert "fun add(x: Int, y: Int) = x + y" in code_blocks[1]

    def test_extract_code_no_markdown(self):
        """Test extracting code without markdown blocks."""
        llm_response = """fun subtract(a: Int, b: Int): Int {
    return a - b
}"""

        code_blocks = self.adapter.extract_code(llm_response)
        assert len(code_blocks) == 1
        assert "fun subtract" in code_blocks[0]

    def test_extract_code_function_pattern(self):
        """Test extracting code using function pattern matching."""
        llm_response = """Here are the functions:
fun multiply(a: Int, b: Int) = a * b

fun divide(a: Double, b: Double): Double? {
    return if (b != 0.0) a / b else null
}"""

        code_blocks = self.adapter.extract_code(llm_response)
        assert len(code_blocks) == 2
        assert "fun multiply" in code_blocks[0]
        assert "fun divide" in code_blocks[1]
        assert "null" in code_blocks[1]

    def test_validate_syntax_valid(self):
        """Test syntax validation with valid code."""
        valid_code = """fun hello(): String {
    return "Hello, World!"
}"""

        is_valid, error = self.adapter.validate_syntax(valid_code)
        assert is_valid
        assert error is None

    def test_validate_syntax_invalid_braces(self):
        """Test syntax validation with unbalanced braces."""
        invalid_code = """fun broken() {
    return "This won't compile"
"""

        is_valid, error = self.adapter.validate_syntax(invalid_code)
        assert not is_valid
        assert "Unbalanced braces" in error

    def test_validate_syntax_invalid_parens(self):
        """Test syntax validation with unbalanced parentheses."""
        invalid_code = """fun broken( {
    return "This won't compile"
}"""

        is_valid, error = self.adapter.validate_syntax(invalid_code)
        assert not is_valid
        assert "Unbalanced parentheses" in error

    def test_validate_syntax_no_function(self):
        """Test syntax validation without function definition."""
        invalid_code = """val x = 5
val y = 10"""

        is_valid, error = self.adapter.validate_syntax(invalid_code)
        assert not is_valid
        assert "No function definition found" in error


class TestKotlinExecutor:
    """Test Kotlin language executor functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.executor = KotlinExecutor()

    def teardown_method(self):
        """Clean up after tests."""
        self.executor.cleanup()

    def test_prepare_code_simple_function(self):
        """Test preparing simple function for execution."""
        code = """fun add(a: Int, b: Int): Int {
    return a + b
}"""

        test_case = {
            "function_name": "add",
            "parameters": {"a": 5, "b": 3},
            "expected": 8,
            "inplace": "0",
        }

        prepared = self.executor.prepare_code(code, test_case)

        assert "fun main(args: Array<String>)" in prepared
        assert "add(a, b)" in prepared
        assert "toJson(result)" in prepared
        assert code in prepared

    def test_prepare_code_single_expression(self):
        """Test preparing single-expression function."""
        code = """fun multiply(a: Int, b: Int) = a * b"""

        test_case = {
            "function_name": "multiply",
            "parameters": {"a": 4, "b": 5},
            "expected": 20,
            "inplace": "0",
        }

        prepared = self.executor.prepare_code(code, test_case)

        assert "multiply(a, b)" in prepared
        assert code in prepared

    def test_prepare_code_inplace_mode_1(self):
        """Test preparing code for in-place modification."""
        code = """fun reverseList(list: MutableList<Int>) {
    list.reverse()
}"""

        test_case = {
            "function_name": "reverseList",
            "parameters": {"list": [1, 2, 3, 4, 5]},
            "expected": [5, 4, 3, 2, 1],
            "inplace": "1",
        }

        prepared = self.executor.prepare_code(code, test_case)

        assert "toMutableList()" in prepared
        assert "reverseList(mutableParam)" in prepared
        assert "toJson(mutableParam)" in prepared

    def test_prepare_code_inplace_mode_2(self):
        """Test preparing code for both modify and return."""
        code = """fun processAndReturn(list: MutableList<Int>): Int {
    list.sort()
    return list.sum()
}"""

        test_case = {
            "function_name": "processAndReturn",
            "parameters": {"list": [3, 1, 4, 1, 5]},
            "expected": 14,
            "inplace": "2",
        }

        prepared = self.executor.prepare_code(code, test_case)

        assert "toMutableList()" in prepared
        assert "processAndReturn(mutableParam)" in prepared
        assert "result ?: mutableParam" in prepared

    def test_infer_kotlin_type(self):
        """Test Kotlin type inference from Python values."""
        assert self.executor._infer_kotlin_type(42) == "Int"
        assert self.executor._infer_kotlin_type(3.14) == "Double"
        assert self.executor._infer_kotlin_type("hello") == "String"
        assert self.executor._infer_kotlin_type(True) == "Boolean"
        assert self.executor._infer_kotlin_type([1, 2, 3]) == "List<Int>"
        assert self.executor._infer_kotlin_type([1.0, 2.0]) == "List<Double>"
        assert self.executor._infer_kotlin_type(["a", "b"]) == "List<String>"

    def test_get_parse_method(self):
        """Test getting parse methods for Kotlin types."""
        assert self.executor._get_parse_method("Int") == "toInt()"
        assert self.executor._get_parse_method("Double") == "toDouble()"
        assert self.executor._get_parse_method("Boolean") == "toBoolean()"
        assert self.executor._get_parse_method("String") == ""
        assert (
            "split(',').map { it.trim().toInt() }"
            in self.executor._get_parse_method("List<Int>")
        )

    @pytest.mark.skipif(not _kotlin_available(), reason="Kotlin not available")
    def test_execute_simple_function(self):
        """Test executing a simple Kotlin function."""
        code = """fun multiply(a: Int, b: Int): Int {
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
        assert "error" not in result or not result["error"]

    @pytest.mark.skipif(not _kotlin_available(), reason="Kotlin not available")
    def test_execute_single_expression_function(self):
        """Test executing a single-expression function."""
        code = """fun add(a: Int, b: Int) = a + b"""

        test_case = {
            "function_name": "add",
            "parameters": {"a": 10, "b": 20},
            "expected": 30,
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"]
        assert result["actual"] == 30

    @pytest.mark.skipif(not _kotlin_available(), reason="Kotlin not available")
    def test_execute_string_function(self):
        """Test executing a function with string parameters."""
        code = """fun greet(name: String): String {
    return "Hello, $name!"
}"""

        test_case = {
            "function_name": "greet",
            "parameters": {"name": "World"},
            "expected": "Hello, World!",
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"]
        assert result["actual"] == "Hello, World!"

    @pytest.mark.skipif(not _kotlin_available(), reason="Kotlin not available")
    def test_execute_list_function(self):
        """Test executing a function with list parameters."""
        code = """fun sumList(numbers: List<Int>): Int {
    return numbers.sum()
}"""

        test_case = {
            "function_name": "sumList",
            "parameters": {"numbers": [1, 2, 3, 4, 5]},
            "expected": 15,
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"]
        assert result["actual"] == 15

    @pytest.mark.skipif(not _kotlin_available(), reason="Kotlin not available")
    def test_execute_null_safe_function(self):
        """Test executing a function with null safety."""
        code = """fun safeDivide(a: Double, b: Double): Double? {
    return if (b != 0.0) a / b else null
}"""

        test_case = {
            "function_name": "safeDivide",
            "parameters": {"a": 10.0, "b": 2.0},
            "expected": 5.0,
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"]
        assert result["actual"] == 5.0

    @pytest.mark.skipif(not _kotlin_available(), reason="Kotlin not available")
    def test_execute_error_handling(self):
        """Test executing function with compilation error."""
        code = """fun broken( {
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
        assert "Compilation failed" in result["error"]

    @pytest.mark.skipif(not _kotlin_available(), reason="Kotlin not available")
    def test_execute_inplace_modification(self):
        """Test executing function that modifies list in-place."""
        code = """fun doubleList(list: MutableList<Int>) {
    for (i in list.indices) {
        list[i] = list[i] * 2
    }
}"""

        test_case = {
            "function_name": "doubleList",
            "parameters": {"list": [1, 2, 3]},
            "expected": [2, 4, 6],
            "inplace": "1",
        }

        result = self.executor.execute_test(code, test_case)

        assert result["passed"]
        assert result["actual"] == [2, 4, 6]

    @pytest.mark.skipif(not _kotlin_available(), reason="Kotlin not available")
    def test_convert_to_simple_json(self):
        """Test conversion to simple JSON when Gson is not available."""
        code_with_gson = """import com.google.gson.Gson

fun add(a: Int, b: Int) = a + b

fun toJson(obj: Any?): String {
    return Gson().toJson(obj)
}

fun main(args: Array<String>) {
    println(toJson(add(1, 2)))
}"""

        converted = self.executor._convert_to_simple_json(code_with_gson)

        assert "import com.google.gson.Gson" not in converted
        assert "Gson().toJson" not in converted
        assert "when (obj)" in converted
        assert "is List<*>" in converted
