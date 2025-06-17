"""Base test class for language implementations."""
import pytest
from abc import ABC, abstractmethod
from typing import List
from eiplgrader.languages.registry import language_registry


class BaseLanguageTest(ABC):
    """Base test class for language implementations"""
    
    language_name: str = None  # Override in subclasses
    
    @pytest.fixture
    def adapter(self):
        """Get the language adapter"""
        return language_registry.get_adapter(self.language_name)
    
    @pytest.fixture
    def executor(self):
        """Get the language executor"""
        return language_registry.get_executor(self.language_name)
    
    def test_adapter_registered(self):
        """Test that the adapter is registered"""
        assert self.language_name in language_registry.list_languages()
    
    def test_basic_function_generation(self, adapter):
        """Test basic function prompt generation"""
        prompt = adapter.generate_prompt(
            "add two numbers",
            "add",
            gen_type="cgbg"
        )
        assert "add" in prompt
        assert self.language_name in prompt.lower() or "code" in prompt.lower()
    
    def test_code_extraction(self, adapter):
        """Test code extraction from LLM response"""
        response = self.get_sample_response()
        code_blocks = adapter.extract_code(response)
        assert len(code_blocks) > 0
        assert all(isinstance(block, str) for block in code_blocks)
    
    def test_syntax_validation(self, adapter):
        """Test syntax validation"""
        valid_code = self.get_sample_code()
        is_valid, error = adapter.validate_syntax(valid_code)
        assert is_valid, f"Valid code failed validation: {error}"
        
        invalid_code = "this is not valid code!"
        is_valid, error = adapter.validate_syntax(invalid_code)
        assert not is_valid
    
    def test_basic_execution(self, executor):
        """Test basic code execution"""
        code = self.get_sample_code()
        test_case = {
            "function_name": "add",
            "parameters": {"a": 5, "b": 3},
            "expected": 8
        }
        result = executor.execute_test(code, test_case)
        assert result["passed"], f"Test failed: {result.get('error', 'Unknown error')}"
        assert result["actual"] == 8
    
    def test_multiple_test_cases(self, executor):
        """Test multiple test cases"""
        code = self.get_sample_code()
        test_cases = [
            {"function_name": "add", "parameters": {"a": 1, "b": 1}, "expected": 2},
            {"function_name": "add", "parameters": {"a": 0, "b": 0}, "expected": 0},
            {"function_name": "add", "parameters": {"a": -1, "b": 1}, "expected": 0},
            {"function_name": "add", "parameters": {"a": 10, "b": 20}, "expected": 30},
        ]
        
        for test_case in test_cases:
            result = executor.execute_test(code, test_case)
            assert result["passed"], f"Test failed for {test_case}: {result.get('error')}"
    
    def test_error_handling(self, executor):
        """Test error handling for invalid code"""
        invalid_code = "this is not valid code"
        test_case = {
            "function_name": "add",
            "parameters": {"a": 1, "b": 1},
            "expected": 2
        }
        result = executor.execute_test(invalid_code, test_case)
        assert not result["passed"]
        assert "error" in result
    
    @abstractmethod
    def get_sample_response(self) -> str:
        """Return sample LLM response for this language"""
        pass
    
    @abstractmethod
    def get_sample_code(self) -> str:
        """Return sample working code for this language"""
        pass