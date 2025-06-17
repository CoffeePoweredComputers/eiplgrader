"""Tests for Ruby language implementation."""
import pytest
from tests.languages.test_base_language import BaseLanguageTest
from eiplgrader.languages.adapters.ruby_adapter import RubyAdapter
from eiplgrader.languages.executors.ruby_executor import RubyExecutor


class TestRuby(BaseLanguageTest):
    """Test Ruby language implementation."""
    
    language_name = "ruby"
    
    def get_sample_response(self) -> str:
        """Return sample LLM response for Ruby."""
        return '''Here's a Ruby method that adds two numbers:

```ruby
def add(a, b)
  a + b
end
```

This method takes two parameters and returns their sum.'''
    
    def get_sample_code(self) -> str:
        """Return sample working Ruby code."""
        return """def add(a, b)
  a + b
end"""
    
    def test_ruby_specific_prompt_generation(self):
        """Test Ruby-specific prompt features."""
        adapter = RubyAdapter()
        prompt = adapter.generate_prompt(
            "reverse a string", 
            "reverse_string",
            gen_type="cgbg"
        )
        assert "Ruby" in prompt
        assert "method" in prompt  # Ruby uses 'method' not 'function'
        assert "reverse_string" in prompt
    
    def test_extract_ruby_blocks(self):
        """Test extraction of Ruby code blocks."""
        adapter = RubyAdapter()
        
        # Test markdown blocks
        response = """
Here's the code:

```ruby
def multiply(x, y)
  x * y
end
```

And another one:

```ruby
def divide(x, y)
  x / y
end
```
"""
        blocks = adapter.extract_code(response)
        assert len(blocks) == 2
        assert "multiply" in blocks[0]
        assert "divide" in blocks[1]
    
    def test_extract_method_definitions(self):
        """Test extraction of method definitions without markdown."""
        adapter = RubyAdapter()
        response = """
def factorial(n)
  return 1 if n <= 1
  n * factorial(n - 1)
end

def fibonacci(n)
  return n if n <= 1
  fibonacci(n - 1) + fibonacci(n - 2)
end
"""
        blocks = adapter.extract_code(response)
        assert len(blocks) == 2
        assert "factorial" in blocks[0]
        assert "fibonacci" in blocks[1]
    
    def test_ruby_syntax_validation(self):
        """Test Ruby-specific syntax validation."""
        adapter = RubyAdapter()
        
        # Valid Ruby code
        valid_code = """
def greet(name)
  "Hello, #{name}!"
end
"""
        is_valid, error = adapter.validate_syntax(valid_code)
        assert is_valid, f"Valid Ruby code failed: {error}"
        
        # Invalid Ruby code
        invalid_code = """
def broken
  puts "missing end
"""
        is_valid, error = adapter.validate_syntax(invalid_code)
        assert not is_valid
        assert "error" in error.lower()
    
    def test_ruby_blocks_and_symbols(self):
        """Test Ruby code with blocks and symbols."""
        executor = RubyExecutor()
        code = """
def process_array(arr)
  arr.map { |x| x * 2 }
end
"""
        test_case = {
            "function_name": "process_array",
            "parameters": {"arr": [1, 2, 3, 4]},
            "expected": [2, 4, 6, 8]
        }
        result = executor.execute_test(code, test_case)
        assert result["passed"], f"Test failed: {result.get('error')}"
        assert result["actual"] == [2, 4, 6, 8]
    
    def test_ruby_string_interpolation(self):
        """Test Ruby string interpolation."""
        executor = RubyExecutor()
        code = """
def format_greeting(name, age)
  "Hello, #{name}! You are #{age} years old."
end
"""
        test_case = {
            "function_name": "format_greeting",
            "parameters": {"name": "Alice", "age": 25},
            "expected": "Hello, Alice! You are 25 years old."
        }
        result = executor.execute_test(code, test_case)
        assert result["passed"], f"Test failed: {result.get('error')}"
    
    def test_ruby_hash_operations(self):
        """Test Ruby hash operations."""
        executor = RubyExecutor()
        code = """
def merge_hashes(hash1, hash2)
  hash1.merge(hash2)
end
"""
        test_case = {
            "function_name": "merge_hashes",
            "parameters": {
                "hash1": {"a": 1, "b": 2},
                "hash2": {"b": 3, "c": 4}
            },
            "expected": {"a": 1, "b": 3, "c": 4}
        }
        result = executor.execute_test(code, test_case)
        assert result["passed"], f"Test failed: {result.get('error')}"
    
    def test_ruby_inplace_modification(self):
        """Test Ruby in-place modification (mode 1)."""
        executor = RubyExecutor()
        code = """
def sort_array!(arr)
  arr.sort!
end
"""
        test_case = {
            "function_name": "sort_array!",
            "parameters": {"arr": [3, 1, 4, 1, 5, 9]},
            "expected": [1, 1, 3, 4, 5, 9],
            "inplace": "1"
        }
        result = executor.execute_test(code, test_case)
        assert result["passed"], f"Test failed: {result.get('error')}"
        assert result["actual"] == [1, 1, 3, 4, 5, 9]
    
    def test_ruby_modify_and_return(self):
        """Test Ruby function that modifies and returns (mode 2)."""
        executor = RubyExecutor()
        code = """
def reverse_and_upcase!(str)
  str.reverse!
  str.upcase!
  str.length
end
"""
        test_case = {
            "function_name": "reverse_and_upcase!",
            "parameters": {"str": "hello"},
            "expected": 5,  # Returns length, not the modified string
            "inplace": "2"
        }
        result = executor.execute_test(code, test_case)
        assert result["passed"], f"Test failed: {result.get('error')}"
    
    def test_ruby_error_handling(self):
        """Test Ruby error handling."""
        executor = RubyExecutor()
        
        # Test with syntax error
        invalid_code = "def broken; puts 'no end'"
        test_case = {
            "function_name": "broken",
            "parameters": {},
            "expected": None
        }
        result = executor.execute_test(invalid_code, test_case)
        assert not result["passed"]
        assert "error" in result
        
        # Test with runtime error
        code = """
def divide(a, b)
  a / b
end
"""
        test_case = {
            "function_name": "divide",
            "parameters": {"a": 10, "b": 0},
            "expected": 5
        }
        result = executor.execute_test(code, test_case)
        assert not result["passed"]
        assert "error" in result
    
    def test_ruby_class_methods(self):
        """Test Ruby class methods."""
        executor = RubyExecutor()
        code = """
class Calculator
  def self.multiply(a, b)
    a * b
  end
end

def multiply(a, b)
  Calculator.multiply(a, b)
end
"""
        test_case = {
            "function_name": "multiply",
            "parameters": {"a": 6, "b": 7},
            "expected": 42
        }
        result = executor.execute_test(code, test_case)
        assert result["passed"], f"Test failed: {result.get('error')}"
        assert result["actual"] == 42
    
    def test_redef_generation_type(self):
        """Test redef generation type for Ruby."""
        adapter = RubyAdapter()
        prompt = adapter.generate_prompt(
            "",  # Not used in redef
            "calculate_area",
            gen_type="redef",
            params="length, width",
            assumptions="length and width are positive numbers representing rectangle dimensions"
        )
        assert "calculate_area" in prompt
        assert "length, width" in prompt
        assert "positive numbers" in prompt
        assert "```ruby" in prompt
    
    def test_multiple_generation_versions(self):
        """Test generating multiple versions."""
        adapter = RubyAdapter()
        prompt = adapter.generate_prompt(
            "calculate the sum of an array",
            "array_sum",
            gen_type="cgbg",
            num_to_gen=3
        )
        assert "3 different versions" in prompt