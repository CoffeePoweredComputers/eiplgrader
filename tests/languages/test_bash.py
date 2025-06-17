"""Test suite for Bash language adapter and executor."""
import pytest
from eiplgrader.languages.adapters.bash_adapter import BashAdapter
from eiplgrader.languages.executors.bash_executor import BashExecutor


class TestBashAdapter:
    """Test the Bash language adapter."""
    
    def setup_method(self):
        """Set up test fixtures."""
        self.adapter = BashAdapter()
    
    def test_get_config(self):
        """Test language configuration."""
        config = self.adapter.get_config()
        assert config.name == "bash"
        assert config.display_name == "Bash"
        assert ".sh" in config.file_extensions
        assert config.run_command == ["bash"]
    
    def test_generate_prompt_cgbg(self):
        """Test CGBG prompt generation."""
        prompt = self.adapter.generate_prompt(
            student_response="adds two numbers together",
            function_name="add",
            gen_type="cgbg"
        )
        
        assert "add" in prompt
        assert "adds two numbers together" in prompt
        assert "```bash" in prompt
        assert "echo" in prompt
        assert "$1" in prompt
    
    def test_generate_prompt_redef(self):
        """Test redef prompt generation."""
        prompt = self.adapter.generate_prompt(
            student_response="",
            function_name="multiply",
            gen_type="redef",
            params="a, b",
            assumptions="a and b are integers"
        )
        
        assert "multiply" in prompt
        assert "$1 is a" in prompt
        assert "$2 is b" in prompt
        assert "a and b are integers" in prompt
        assert "```bash" in prompt
    
    def test_extract_code_markdown(self):
        """Test extracting code from markdown blocks."""
        response = """Here's the function:
```bash
add() {
    echo $(($1 + $2))
}
```
"""
        codes = self.adapter.extract_code(response)
        assert len(codes) == 1
        assert "add()" in codes[0]
        assert "echo" in codes[0]
    
    def test_extract_code_multiple(self):
        """Test extracting multiple code blocks."""
        response = """Version 1:
```bash
add() {
    echo $(($1 + $2))
}
```

Version 2:
```bash
add() {
    local sum=$(($1 + $2))
    echo $sum
}
```
"""
        codes = self.adapter.extract_code(response)
        assert len(codes) == 2
        assert "add()" in codes[0]
        assert "local sum" in codes[1]
    
    def test_extract_code_no_markdown(self):
        """Test extracting code without markdown."""
        response = """add() {
    echo $(($1 + $2))
}"""
        codes = self.adapter.extract_code(response)
        assert len(codes) == 1
        assert "add()" in codes[0]
    
    def test_validate_syntax_valid(self):
        """Test syntax validation with valid code."""
        code = """add() {
    echo $(($1 + $2))
}"""
        is_valid, error = self.adapter.validate_syntax(code)
        assert is_valid
        assert error is None
    
    def test_validate_syntax_invalid(self):
        """Test syntax validation with invalid code."""
        code = """add() {
    echo $(($1 + $2)
}"""  # Missing closing parenthesis
        is_valid, error = self.adapter.validate_syntax(code)
        assert not is_valid
        assert "Syntax error" in error


class TestBashExecutor:
    """Test the Bash language executor."""
    
    def setup_method(self):
        """Set up test fixtures."""
        self.executor = BashExecutor()
    
    def teardown_method(self):
        """Clean up after tests."""
        self.executor.cleanup()
    
    def test_simple_addition(self):
        """Test executing a simple addition function."""
        code = """add() {
    echo $(($1 + $2))
}"""
        test_case = {
            "function_name": "add",
            "parameters": {"a": 5, "b": 3},
            "expected": 8
        }
        
        result = self.executor.execute_test(code, test_case)
        assert result["passed"]
        assert result["actual"] == 8
        assert result["expected"] == 8
    
    def test_string_concatenation(self):
        """Test string operations."""
        code = """concat() {
    echo "$1$2"
}"""
        test_case = {
            "function_name": "concat",
            "parameters": {"a": "Hello", "b": "World"},
            "expected": "HelloWorld"
        }
        
        result = self.executor.execute_test(code, test_case)
        assert result["passed"]
        assert result["actual"] == "HelloWorld"
    
    def test_arithmetic_division(self):
        """Test division with integer result."""
        code = """divide() {
    echo $(($1 / $2))
}"""
        test_case = {
            "function_name": "divide",
            "parameters": {"a": 10, "b": 2},
            "expected": 5
        }
        
        result = self.executor.execute_test(code, test_case)
        assert result["passed"]
        assert result["actual"] == 5
    
    def test_conditional_logic(self):
        """Test function with conditional logic."""
        code = """max() {
    if [ $1 -gt $2 ]; then
        echo $1
    else
        echo $2
    fi
}"""
        test_case = {
            "function_name": "max",
            "parameters": {"a": 10, "b": 20},
            "expected": 20
        }
        
        result = self.executor.execute_test(code, test_case)
        assert result["passed"]
        assert result["actual"] == 20
    
    def test_syntax_error(self):
        """Test handling of syntax errors."""
        code = """add() {
    echo $(($1 + $2)
}"""  # Missing closing parenthesis
        test_case = {
            "function_name": "add",
            "parameters": {"a": 5, "b": 3},
            "expected": 8
        }
        
        result = self.executor.execute_test(code, test_case)
        assert not result["passed"]
        assert "error" in result
        assert result["actual"] is None
    
    def test_runtime_error(self):
        """Test handling of runtime errors."""
        # Use a function that explicitly exits with error
        code = """divide() {
    if [ $2 -eq 0 ]; then
        echo "Division by zero error" >&2
        exit 1
    fi
    echo $(($1 / $2))
}"""
        test_case = {
            "function_name": "divide",
            "parameters": {"a": 10, "b": 0},
            "expected": 5  # Expected value doesn't matter as it should error
        }
        
        # This will cause an error due to our explicit check
        result = self.executor.execute_test(code, test_case)
        assert not result["passed"]
        assert "error" in result
    
    def test_missing_function(self):
        """Test handling when function is not defined."""
        code = """add() {
    echo $(($1 + $2))
}"""
        test_case = {
            "function_name": "subtract",  # Wrong function name
            "parameters": {"a": 5, "b": 3},
            "expected": 2
        }
        
        result = self.executor.execute_test(code, test_case)
        assert not result["passed"]
        assert result["actual"] is None
    
    def test_prepare_code_structure(self):
        """Test the structure of prepared code."""
        code = """add() {
    echo $(($1 + $2))
}"""
        test_case = {
            "function_name": "add",
            "parameters": {"a": 5, "b": 3},
            "expected": 8
        }
        
        prepared = self.executor.prepare_code(code, test_case)
        assert "#!/bin/bash" in prepared
        assert code in prepared
        assert "add 5 3" in prepared
    
    def test_cleanup(self):
        """Test that cleanup removes temporary files."""
        import os
        temp_dir = self.executor.temp_dir
        
        # Create a test file
        test_file = os.path.join(temp_dir, "test.txt")
        with open(test_file, 'w') as f:
            f.write("test")
        
        assert os.path.exists(test_file)
        
        # Clean up
        self.executor.cleanup()
        
        # Directory should be removed
        assert not os.path.exists(temp_dir)