"""Tests for PHP language implementation."""
import pytest
from .test_base_language import BaseLanguageTest


class TestPHPLanguage(BaseLanguageTest):
    """Test suite for PHP language support."""
    
    language_name = "php"
    
    def get_sample_response(self) -> str:
        """Return sample LLM response for PHP."""
        return """Here's a PHP function to add two numbers:

```php
<?php
function add($a, $b) {
    return $a + $b;
}
```

This function takes two parameters and returns their sum."""
    
    def get_sample_code(self) -> str:
        """Return sample working PHP code."""
        return """<?php
function add($a, $b) {
    return $a + $b;
}"""
    
    def test_php_tag_handling(self, adapter):
        """Test that PHP tag is properly handled."""
        # Test extraction without PHP tag in response
        response = """```php
function add($a, $b) {
    return $a + $b;
}
```"""
        code_blocks = adapter.extract_code(response)
        assert len(code_blocks) > 0
        assert all(block.startswith("<?php") for block in code_blocks)
    
    def test_multiple_functions(self, adapter):
        """Test extraction of multiple PHP functions."""
        response = """Here are two different implementations:

```php
<?php
function add($a, $b) {
    return $a + $b;
}
```

And another version:

```php
<?php
function add($a, $b) {
    $sum = $a + $b;
    return $sum;
}
```"""
        code_blocks = adapter.extract_code(response)
        assert len(code_blocks) == 2
        assert all("function add" in block for block in code_blocks)
    
    def test_string_handling(self, executor):
        """Test string concatenation in PHP."""
        code = """<?php
function concat($str1, $str2) {
    return $str1 . $str2;
}"""
        test_case = {
            "function_name": "concat",
            "parameters": {"str1": "Hello", "str2": "World"},
            "expected": "HelloWorld"
        }
        result = executor.execute_test(code, test_case)
        assert result["passed"], f"Test failed: {result.get('error', 'Unknown error')}"
        assert result["actual"] == "HelloWorld"
    
    def test_array_operations(self, executor):
        """Test array operations in PHP."""
        code = """<?php
function sum_array($arr) {
    return array_sum($arr);
}"""
        test_case = {
            "function_name": "sum_array",
            "parameters": {"arr": [1, 2, 3, 4, 5]},
            "expected": 15
        }
        result = executor.execute_test(code, test_case)
        assert result["passed"], f"Test failed: {result.get('error', 'Unknown error')}"
        assert result["actual"] == 15
    
    def test_inplace_modification(self, executor):
        """Test in-place array modification."""
        code = """<?php
function append_element(&$arr, $element) {
    $arr[] = $element;
}"""
        test_case = {
            "function_name": "append_element",
            "parameters": {"arr": [1, 2, 3], "element": 4},
            "expected": [1, 2, 3, 4],
            "inplace": "1"
        }
        result = executor.execute_test(code, test_case)
        assert result["passed"], f"Test failed: {result.get('error', 'Unknown error')}"
        assert result["actual"] == [1, 2, 3, 4]
    
    def test_syntax_error_detection(self, adapter):
        """Test PHP syntax error detection."""
        # Missing semicolon
        invalid_code = """<?php
function add($a, $b) {
    return $a + $b
}"""
        is_valid, error = adapter.validate_syntax(invalid_code)
        assert not is_valid
        assert "error" in error.lower()
    
    def test_runtime_error_handling(self, executor):
        """Test runtime error handling."""
        code = """<?php
function divide($a, $b) {
    if ($b == 0) {
        throw new Exception("Division by zero");
    }
    return $a / $b;
}"""
        test_case = {
            "function_name": "divide",
            "parameters": {"a": 10, "b": 0},
            "expected": None
        }
        result = executor.execute_test(code, test_case)
        assert not result["passed"]
        assert "error" in result
        assert "Division by zero" in result["error"]
    
    def test_type_coercion(self, executor):
        """Test PHP's type coercion."""
        code = """<?php
function add_mixed($a, $b) {
    return $a + $b;
}"""
        test_cases = [
            {"function_name": "add_mixed", "parameters": {"a": "5", "b": 3}, "expected": 8},
            {"function_name": "add_mixed", "parameters": {"a": 2.5, "b": 1.5}, "expected": 4.0},
            {"function_name": "add_mixed", "parameters": {"a": "10", "b": "20"}, "expected": 30},
        ]
        
        for test_case in test_cases:
            result = executor.execute_test(code, test_case)
            assert result["passed"], f"Test failed for {test_case}: {result.get('error')}"
    
    def test_associative_array(self, executor):
        """Test associative array handling."""
        code = """<?php
function get_value($data, $key) {
    return isset($data[$key]) ? $data[$key] : null;
}"""
        test_case = {
            "function_name": "get_value",
            "parameters": {
                "data": {"name": "John", "age": 30},
                "key": "name"
            },
            "expected": "John"
        }
        result = executor.execute_test(code, test_case)
        assert result["passed"], f"Test failed: {result.get('error', 'Unknown error')}"
        assert result["actual"] == "John"
    
    def test_prompt_generation_types(self, adapter):
        """Test different prompt generation types."""
        # Test CGBG prompt
        cgbg_prompt = adapter.generate_prompt(
            "multiply two numbers and return the result",
            "multiply",
            gen_type="cgbg"
        )
        assert "multiply" in cgbg_prompt
        assert "<?php" in cgbg_prompt
        assert "echo" not in cgbg_prompt or "DO NOT use echo" in cgbg_prompt
        
        # Test redef prompt
        redef_prompt = adapter.generate_prompt(
            "",
            "calculate",
            gen_type="redef",
            params="$x, $y",
            assumptions="$x and $y are numbers"
        )
        assert "calculate" in redef_prompt
        assert "$x, $y" in redef_prompt
        assert "$x and $y are numbers" in redef_prompt
    
    def test_no_echo_enforcement(self, adapter):
        """Test that prompts enforce no echo/print usage."""
        prompt = adapter.generate_prompt(
            "calculate the sum",
            "sum",
            gen_type="cgbg"
        )
        assert "echo" in prompt.lower() or "print" in prompt.lower()
        assert "return" in prompt.lower()
        assert "DO NOT" in prompt or "not" in prompt.lower()


# Additional specific PHP tests
class TestPHPSpecificFeatures:
    """Test PHP-specific language features."""
    
    @pytest.fixture
    def executor(self):
        """Get PHP executor."""
        from eiplgrader.languages.registry import language_registry
        return language_registry.get_executor("php")
    
    def test_global_variable_access(self, executor):
        """Test global variable access."""
        code = """<?php
$global_counter = 10;

function increment_global() {
    global $global_counter;
    $global_counter++;
    return $global_counter;
}"""
        test_case = {
            "function_name": "increment_global",
            "parameters": {},
            "expected": 11
        }
        result = executor.execute_test(code, test_case)
        assert result["passed"], f"Test failed: {result.get('error', 'Unknown error')}"
    
    def test_variable_references(self, executor):
        """Test PHP reference handling."""
        code = """<?php
function swap(&$a, &$b) {
    $temp = $a;
    $a = $b;
    $b = $temp;
    return true;
}"""
        # This would require special handling for multiple reference parameters
        # For now, we test that the function at least executes
        test_case = {
            "function_name": "swap",
            "parameters": {"a": 5, "b": 10},
            "expected": True,
            "inplace": "2"
        }
        result = executor.execute_test(code, test_case)
        # The test harness doesn't fully support swapping two parameters,
        # but it should at least execute without error
        assert "error" not in result or result["error"] is None