"""Tests for base adapter functionality and all language adapters."""

import pytest
from unittest.mock import Mock, patch
from typing import List, Dict, Any

from eiplgrader.languages.base import LanguageAdapter, LanguageConfig
from eiplgrader.languages.adapters.python_adapter import PythonAdapter
from eiplgrader.languages.adapters.javascript_adapter import JavaScriptAdapter
from eiplgrader.languages.adapters.java_adapter import JavaAdapter
from eiplgrader.languages.adapters.c_adapter import CAdapter
from eiplgrader.languages.adapters.cpp_adapter import CppAdapter
from eiplgrader.languages.adapters.go_adapter import GoAdapter
from eiplgrader.languages.adapters.haskell_adapter import HaskellAdapter


class TestLanguageAdapterBase:
    """Test base adapter functionality."""

    def test_abstract_methods_required(self):
        """Test that abstract methods are required."""
        with pytest.raises(TypeError):
            LanguageAdapter()  # pylint: disable=abstract-class-instantiated

    def test_language_config_creation(self):
        """Test LanguageConfig dataclass creation."""
        config = LanguageConfig(
            name="test",
            display_name="Test Language",
            file_extensions=[".test"],
            run_command=["test"],
            compile_command=["compile"],
            test_timeout=45,
        )

        assert config.name == "test"
        assert config.display_name == "Test Language"
        assert config.file_extensions == [".test"]
        assert config.run_command == ["test"]
        assert config.compile_command == ["compile"]
        assert config.test_timeout == 45

    def test_language_config_defaults(self):
        """Test LanguageConfig with default values."""
        config = LanguageConfig(
            name="test",
            display_name="Test Language",
            file_extensions=[".test"],
            run_command=["test"],
        )

        assert config.compile_command is None
        assert config.test_timeout == 30


class TestPythonAdapter:
    """Test Python language adapter."""

    def setup_method(self):
        """Set up test fixtures."""
        self.adapter = PythonAdapter()

    def test_get_config(self):
        """Test Python configuration."""
        config = self.adapter.get_config()

        assert config.name == "python"
        assert config.display_name == "Python"
        assert config.file_extensions == [".py"]
        assert config.run_command == ["python3"]
        assert config.compile_command is None
        assert config.test_timeout == 30

    def test_generate_prompt_cgbg(self):
        """Test CGBG prompt generation."""
        prompt = self.adapter.generate_prompt(
            student_response="calculates the sum of two numbers",
            function_name="add_numbers",
            gen_type="cgbg",
        )

        assert "add_numbers" in prompt
        assert "calculates the sum of two numbers" in prompt
        assert "introductory CS student" in prompt
        assert "type annotations" in prompt
        assert "list comprehension" in prompt
        assert "```python" in prompt

    def test_generate_prompt_redef(self):
        """Test redef prompt generation."""
        prompt = self.adapter.generate_prompt(
            student_response="unused",
            function_name="test_func",
            gen_type="redef",
            params="x, y",
            assumptions="x and y are integers",
        )

        assert "test_func" in prompt
        assert "def test_func(x, y):" in prompt
        assert "x and y are integers" in prompt
        assert "introductory CS student" in prompt
        assert "```python" in prompt

    def test_generate_prompt_redef_defaults(self):
        """Test redef prompt with default values."""
        prompt = self.adapter.generate_prompt(
            student_response="unused", function_name="test_func", gen_type="redef"
        )

        assert "test_func" in prompt
        assert "def test_func():" in prompt

    def test_generate_prompt_unknown_type(self):
        """Test prompt generation with unknown type."""
        prompt = self.adapter.generate_prompt(
            student_response="unused", function_name="test_func", gen_type="unknown"
        )

        assert "Generate a Python function named test_func" in prompt

    def test_extract_code_python_block(self):
        """Test extracting code from Python markdown block."""
        response = """Here's the solution:

```python
def add(a, b):
    return a + b
```

This function adds two numbers."""

        codes = self.adapter.extract_code(response)
        assert len(codes) == 1
        assert "def add(a, b):" in codes[0]
        assert "return a + b" in codes[0]

    def test_extract_code_py_block(self):
        """Test extracting code from py markdown block."""
        response = """```py
def subtract(a, b):
    return a - b
```"""

        codes = self.adapter.extract_code(response)
        assert len(codes) == 1
        assert "def subtract(a, b):" in codes[0]

    def test_extract_code_generic_block(self):
        """Test extracting code from generic markdown block."""
        response = """```
def multiply(a, b):
    return a * b
```"""

        codes = self.adapter.extract_code(response)
        assert len(codes) == 1
        assert "def multiply(a, b):" in codes[0]

    def test_extract_code_multiple_blocks(self):
        """Test extracting multiple code blocks."""
        response = """```python
def func1():
    pass
```

And another:

```python
def func2():
    pass
```"""

        codes = self.adapter.extract_code(response)
        assert len(codes) == 2
        assert "func1" in codes[0]
        assert "func2" in codes[1]

    def test_extract_code_no_blocks(self):
        """Test extracting code when no blocks found."""
        response = "def func(): pass"

        codes = self.adapter.extract_code(response)
        assert len(codes) == 1
        assert codes[0] == "def func(): pass"

    def test_extract_code_empty_response(self):
        """Test extracting code from empty response."""
        codes = self.adapter.extract_code("")
        assert len(codes) == 0

        codes = self.adapter.extract_code("   ")
        assert len(codes) == 0

    def test_normalize_code_basic(self):
        """Test basic code normalization."""
        code = """def add(a, b):
    # This adds two numbers
    return a + b"""

        normalized = self.adapter.normalize_code(code)
        assert normalized == "def add(a, b): return a + b"

    def test_normalize_code_complex(self):
        """Test complex code normalization."""
        code = """def complex_func(x):
    # Initialize variable
    result = 0
    
    # Loop through values
    for i in range(x):
        result += i  # Add current value
    
    return result  # Return final result"""

        normalized = self.adapter.normalize_code(code)
        expected = "def complex_func(x): result = 0 for i in range(x): result += i return result"
        assert normalized == expected

    def test_normalize_code_empty(self):
        """Test normalizing empty code."""
        assert self.adapter.normalize_code("") == ""
        assert self.adapter.normalize_code("   ") == ""
        assert self.adapter.normalize_code("# Only comments") == ""


class TestJavaScriptAdapter:
    """Test JavaScript language adapter."""

    def setup_method(self):
        """Set up test fixtures."""
        self.adapter = JavaScriptAdapter()

    def test_get_config(self):
        """Test JavaScript configuration."""
        config = self.adapter.get_config()

        assert config.name == "javascript"
        assert config.display_name == "JavaScript"
        assert config.file_extensions == [".js"]
        assert config.run_command == ["node"]
        assert config.compile_command is None
        assert config.test_timeout == 30

    def test_generate_prompt_cgbg(self):
        """Test CGBG prompt generation."""
        prompt = self.adapter.generate_prompt(
            student_response="calculates the factorial",
            function_name="factorial",
            gen_type="cgbg",
        )

        assert "factorial" in prompt
        assert "calculates the factorial" in prompt
        assert "introductory CS student" in prompt
        assert "JavaScript" in prompt
        assert "```javascript" in prompt

    def test_extract_code_patterns(self):
        """Test various code extraction patterns."""
        # Test javascript block
        response1 = "```javascript\nfunction test() {}\n```"
        codes1 = self.adapter.extract_code(response1)
        assert len(codes1) == 1
        assert "function test()" in codes1[0]

        # Test js block
        response2 = "```js\nfunction test() {}\n```"
        codes2 = self.adapter.extract_code(response2)
        assert len(codes2) == 1
        assert "function test()" in codes2[0]


class TestJavaAdapter:
    """Test Java language adapter."""

    def setup_method(self):
        """Set up test fixtures."""
        self.adapter = JavaAdapter()

    def test_get_config(self):
        """Test Java configuration."""
        config = self.adapter.get_config()

        assert config.name == "java"
        assert config.display_name == "Java"
        assert config.file_extensions == [".java"]
        assert config.run_command == ["java"]
        assert config.compile_command == ["javac"]
        assert config.test_timeout == 30

    def test_generate_prompt_cgbg(self):
        """Test CGBG prompt generation."""
        prompt = self.adapter.generate_prompt(
            student_response="finds the maximum value",
            function_name="findMax",
            gen_type="cgbg",
        )

        assert "findMax" in prompt
        assert "finds the maximum value" in prompt
        assert "introductory CS student" in prompt
        assert "Java" in prompt
        assert "```java" in prompt
        assert "public static" in prompt

    def test_normalize_code_java_comments(self):
        """Test Java comment normalization."""
        code = """public static int add(int a, int b) {
    // Single line comment
    int result = a + b;
    /* Multi-line
       comment */
    return result;
}"""

        normalized = self.adapter.normalize_code(code)
        expected = (
            "public static int add(int a, int b) { int result = a + b; return result; }"
        )
        assert normalized == expected


class TestCompiledLanguageAdapters:
    """Test compiled language adapters (C, C++, Go, Haskell)."""

    @pytest.mark.parametrize(
        "adapter_class,expected_name,expected_compile_cmd",
        [
            (CAdapter, "c", ["gcc", "-o", "a.out"]),
            (CppAdapter, "cpp", ["g++", "-o", "a.out"]),
            (GoAdapter, "go", None),
            (HaskellAdapter, "haskell", ["ghc"]),
        ],
    )
    def test_compiled_adapter_configs(
        self, adapter_class, expected_name, expected_compile_cmd
    ):
        """Test compiled language adapter configurations."""
        adapter = adapter_class()
        config = adapter.get_config()

        assert config.name == expected_name
        assert config.compile_command == expected_compile_cmd
        assert config.test_timeout == 30

    def test_c_adapter_specifics(self):
        """Test C adapter specifics."""
        adapter = CAdapter()
        config = adapter.get_config()

        assert config.file_extensions == [".c"]
        assert config.run_command == ["./a.out"]

        # Test prompt generation
        prompt = adapter.generate_prompt(
            student_response="calculates area",
            function_name="calculate_area",
            gen_type="cgbg",
        )
        assert "calculate_area" in prompt
        assert "learning C" in prompt

    def test_cpp_adapter_specifics(self):
        """Test C++ adapter specifics."""
        adapter = CppAdapter()
        config = adapter.get_config()

        assert config.file_extensions == [".cpp", ".cc", ".cxx"]
        assert config.run_command == ["./a.out"]

        # Test prompt generation
        prompt = adapter.generate_prompt(
            student_response="sorts an array",
            function_name="sort_array",
            gen_type="cgbg",
        )
        assert "sort_array" in prompt
        assert "C++" in prompt

    def test_go_adapter_specifics(self):
        """Test Go adapter specifics."""
        adapter = GoAdapter()
        config = adapter.get_config()

        assert config.file_extensions == [".go"]
        assert config.run_command == ["go", "run"]

        # Test prompt generation
        prompt = adapter.generate_prompt(
            student_response="reverses a string",
            function_name="reverseString",
            gen_type="cgbg",
        )
        assert "reverseString" in prompt
        assert "Go" in prompt

    def test_haskell_adapter_specifics(self):
        """Test Haskell adapter specifics."""
        adapter = HaskellAdapter()
        config = adapter.get_config()

        assert config.file_extensions == [".hs"]
        assert not config.run_command

        # Test prompt generation
        prompt = adapter.generate_prompt(
            student_response="calculates fibonacci",
            function_name="fibonacci",
            gen_type="cgbg",
        )
        assert "fibonacci" in prompt
        assert "Haskell" in prompt


class TestAdapterCodeExtractionPatterns:
    """Test code extraction patterns across different adapters."""

    @pytest.mark.parametrize(
        "adapter_class,language_name",
        [
            (PythonAdapter, "python"),
            (JavaScriptAdapter, "javascript"),
            (JavaAdapter, "java"),
            (CAdapter, "c"),
            (CppAdapter, "cpp"),
            (GoAdapter, "go"),
            (HaskellAdapter, "haskell"),
        ],
    )
    def test_extract_code_empty_handling(
        self, adapter_class, language_name
    ):  # pylint: disable=unused-argument
        """Test that all adapters handle empty responses consistently."""
        adapter = adapter_class()

        # Test empty string
        codes = adapter.extract_code("")
        assert len(codes) == 0

        # Test whitespace only
        codes = adapter.extract_code("   \n\t  ")
        assert len(codes) == 0

    @pytest.mark.parametrize(
        "adapter_class",
        [
            PythonAdapter,
            JavaScriptAdapter,
            JavaAdapter,
            CAdapter,
            CppAdapter,
            GoAdapter,
            HaskellAdapter,
        ],
    )
    def test_extract_code_fallback(self, adapter_class):
        """Test that all adapters fall back to returning the full response when no blocks found."""
        adapter = adapter_class()

        response = "Some code without markdown blocks"
        codes = adapter.extract_code(response)

        assert len(codes) == 1
        assert codes[0] == response

    @pytest.mark.parametrize(
        "adapter_class",
        [
            PythonAdapter,
            JavaScriptAdapter,
            JavaAdapter,
            CAdapter,
            CppAdapter,
            GoAdapter,
            HaskellAdapter,
        ],
    )
    def test_normalize_code_consistency(self, adapter_class):
        """Test that all adapters normalize code consistently."""
        adapter = adapter_class()

        # Test with language-appropriate comments and whitespace
        if adapter_class == PythonAdapter:
            code = "def test():\n    # comment\n    return 42"
            comment_text = "# comment"
        elif adapter_class == HaskellAdapter:
            code = "test = do\n    -- comment\n    return 42"
            comment_text = "-- comment"
        else:
            # For other languages (JavaScript, Java, C, C++, Go) use // comments
            code = "function test() {\n    // comment\n    return 42;\n}"
            comment_text = "// comment"

        normalized = adapter.normalize_code(code)

        # All adapters should remove comments and normalize whitespace
        assert comment_text not in normalized
        assert len(normalized.split()) > 0  # Should have some content
        assert normalized.strip() == normalized  # Should be trimmed


class TestAdapterErrorHandling:
    """Test adapter error handling scenarios."""

    def test_python_adapter_malformed_code_blocks(self):
        """Test Python adapter with malformed code blocks."""
        adapter = PythonAdapter()

        # Test unclosed code block
        response = "```python\ndef test():\n    pass"
        codes = adapter.extract_code(response)
        # Should fall back to full response
        assert len(codes) == 1
        assert codes[0] == response

    def test_adapter_normalize_code_edge_cases(self):
        """Test code normalization edge cases."""
        adapter = PythonAdapter()

        # Test code with only whitespace and comments
        code = "   \n  # Just a comment\n   "
        normalized = adapter.normalize_code(code)
        assert normalized == ""

        # Test code with multiple consecutive spaces
        code = "def    func(  x  ,  y  ):\n    return    x   +   y"
        normalized = adapter.normalize_code(code)
        assert "    " not in normalized  # Multiple spaces should be normalized

    def test_generate_prompt_with_special_characters(self):
        """Test prompt generation with special characters."""
        adapter = PythonAdapter()

        # Test with quotes and newlines in student response
        student_response = 'calculates "area" and\nprints result'
        prompt = adapter.generate_prompt(
            student_response=student_response,
            function_name="calculate_area",
            gen_type="cgbg",
        )

        assert student_response in prompt
        assert "calculate_area" in prompt

    def test_generate_prompt_with_empty_kwargs(self):
        """Test prompt generation with missing kwargs."""
        adapter = PythonAdapter()

        # Test redef without function_signature
        prompt = adapter.generate_prompt(
            student_response="unused", function_name="test_func", gen_type="redef"
        )

        # Should use default function signature
        assert "def test_func():" in prompt
