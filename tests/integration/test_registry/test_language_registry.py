"""Tests for the language registry system."""

import pytest
from unittest.mock import Mock, patch, MagicMock
import tempfile
import os

from eiplgrader.languages.registry import LanguageRegistry, language_registry
from eiplgrader.languages.base import LanguageAdapter, LanguageConfig, LanguageExecutor
from eiplgrader.languages.adapters.python_adapter import PythonAdapter
from eiplgrader.languages.adapters.java_adapter import JavaAdapter


class MockAdapter(LanguageAdapter):
    """Mock adapter for testing."""
    
    def get_config(self) -> LanguageConfig:
        return LanguageConfig(
            name="mock",
            display_name="Mock Language",
            file_extensions=[".mock"],
            run_command=["mock"]
        )
    
    def generate_prompt(self, student_response: str, function_name: str, 
                       gen_type: str = "cgbg", num_to_gen: int = 1, **kwargs) -> str:
        return f"Mock prompt for {function_name}"
    
    def extract_code(self, llm_response: str) -> list:
        return [llm_response.strip()] if llm_response.strip() else []
    
    def normalize_code(self, code: str) -> str:
        return code.strip()


class MockExecutor(LanguageExecutor):
    """Mock executor for testing."""
    
    def prepare_code(self, code: str, test_case: dict) -> str:
        return f"prepared_{code}"
    
    def execute_test(self, code: str, test_case: dict) -> dict:
        return {"passed": True, "actual": "mock_result", "expected": "mock_result"}
    
    def cleanup(self) -> None:
        pass


class TestLanguageRegistry:
    """Test core registry functionality."""
    
    def setup_method(self):
        """Set up fresh registry for each test."""
        self.registry = LanguageRegistry()
    
    def test_registry_initialization(self):
        """Test registry initializes empty."""
        assert len(self.registry._adapters) == 0
        assert self.registry.list_languages() == []
    
    def test_register_adapter(self):
        """Test registering a language adapter."""
        self.registry.register("mock", MockAdapter)
        
        assert "mock" in self.registry._adapters
        assert self.registry.is_supported("mock")
        assert "mock" in self.registry.list_languages()
    
    def test_register_adapter_case_insensitive(self):
        """Test that adapter registration is case-insensitive."""
        self.registry.register("MOCK", MockAdapter)
        
        assert self.registry.is_supported("mock")
        assert self.registry.is_supported("Mock")
        assert self.registry.is_supported("MOCK")
        
        # List should return lowercase
        assert "mock" in self.registry.list_languages()
    
    def test_get_adapter_success(self):
        """Test successfully getting an adapter."""
        self.registry.register("mock", MockAdapter)
        
        adapter = self.registry.get_adapter("mock")
        assert adapter is not None
        assert isinstance(adapter, MockAdapter)
        
        # Test case insensitive access
        adapter2 = self.registry.get_adapter("MOCK")
        assert adapter2 is not None
        assert isinstance(adapter2, MockAdapter)
    
    def test_get_adapter_not_found(self):
        """Test getting adapter for unsupported language."""
        adapter = self.registry.get_adapter("nonexistent")
        assert adapter is None
    
    def test_get_adapter_creates_new_instance(self):
        """Test that get_adapter creates new instances each time."""
        self.registry.register("mock", MockAdapter)
        
        adapter1 = self.registry.get_adapter("mock")
        adapter2 = self.registry.get_adapter("mock")
        
        assert adapter1 is not adapter2
        assert isinstance(adapter1, MockAdapter)
        assert isinstance(adapter2, MockAdapter)
    
    def test_list_languages_sorted(self):
        """Test that list_languages returns sorted results."""
        self.registry.register("zebra", MockAdapter)
        self.registry.register("alpha", MockAdapter)
        self.registry.register("beta", MockAdapter)
        
        languages = self.registry.list_languages()
        assert languages == ["alpha", "beta", "zebra"]
    
    def test_is_supported(self):
        """Test is_supported method."""
        assert not self.registry.is_supported("mock")
        
        self.registry.register("mock", MockAdapter)
        assert self.registry.is_supported("mock")
        assert self.registry.is_supported("MOCK")
        assert not self.registry.is_supported("nonexistent")


class TestRegistryExecutorLoading:
    """Test registry executor loading functionality."""
    
    def setup_method(self):
        """Set up fresh registry for each test."""
        self.registry = LanguageRegistry()
    
    def test_get_executor_supported_languages(self):
        """Test getting executors for known languages."""
        # These should work with real modules
        supported_languages = ["python", "javascript", "java", "c", "cpp", "go", "haskell"]
        
        for lang in supported_languages:
            executor = self.registry.get_executor(lang)
            # We can't guarantee the modules exist in test environment,
            # so we just test that the method doesn't crash
            # and returns either an executor or None
            assert executor is None or hasattr(executor, 'execute_test')
    
    def test_get_executor_case_insensitive(self):
        """Test that executor loading is case-insensitive."""
        # Try different cases
        executor1 = self.registry.get_executor("python")
        executor2 = self.registry.get_executor("PYTHON")
        executor3 = self.registry.get_executor("Python")
        
        # All should return the same type (None or actual executor)
        assert type(executor1) == type(executor2) == type(executor3)
    
    def test_get_executor_unsupported_language(self):
        """Test getting executor for unsupported language."""
        executor = self.registry.get_executor("nonexistent")
        assert executor is None
    
    @patch('eiplgrader.languages.registry.import_module')
    def test_get_executor_import_error(self, mock_import):
        """Test executor loading when import fails."""
        mock_import.side_effect = ImportError("Module not found")
        
        executor = self.registry.get_executor("python")
        assert executor is None
    
    @patch('eiplgrader.languages.registry.import_module')
    def test_get_executor_attribute_error(self, mock_import):
        """Test executor loading when class not found in module."""
        mock_module = Mock()
        mock_import.return_value = mock_module
        del mock_module.PythonExecutor  # Simulate missing class
        
        executor = self.registry.get_executor("python")
        assert executor is None
    
    @patch('eiplgrader.languages.registry.import_module')
    def test_get_executor_success(self, mock_import):
        """Test successful executor loading."""
        mock_module = Mock()
        mock_executor_class = Mock()
        mock_executor_instance = Mock()
        
        mock_module.PythonExecutor = mock_executor_class
        mock_executor_class.return_value = mock_executor_instance
        mock_import.return_value = mock_module
        
        executor = self.registry.get_executor("python")
        
        assert executor == mock_executor_instance
        mock_import.assert_called_once_with(".executors.python_executor", package="eiplgrader.languages")
        mock_executor_class.assert_called_once()
    
    def test_executor_mapping_completeness(self):
        """Test that all expected languages have executor mappings."""
        # These are the languages that should be mapped
        expected_languages = {
            "python": "python_executor.PythonExecutor",
            "javascript": "javascript_executor.JavaScriptExecutor", 
            "java": "java_executor.JavaExecutor",
            "c": "c_executor.CExecutor",
            "cpp": "cpp_executor.CppExecutor",
            "go": "go_executor.GoExecutor",
            "haskell": "haskell_executor.HaskellExecutor"
        }
        
        # Access the internal mapping to test completeness
        # Note: This is testing implementation details, but ensures consistency
        for lang in expected_languages:
            # This should not return None due to mapping missing
            # (it might return None due to import errors, but that's different)
            result = self.registry.get_executor(lang)
            # We can't assert result is not None because modules might not exist
            # But we can check the mapping exists by checking it doesn't immediately return None
            # for unsupported language (which would be a quick return)
            
            # Instead, let's verify by checking the method gets to the import stage
            with patch('eiplgrader.languages.registry.import_module') as mock_import:
                mock_import.side_effect = ImportError()
                result = self.registry.get_executor(lang)
                # If import_module was called, then the mapping exists
                mock_import.assert_called_once()


class TestGlobalRegistry:
    """Test global registry instance."""
    
    def test_global_registry_exists(self):
        """Test that global registry exists and is LanguageRegistry instance."""
        from eiplgrader.languages.registry import language_registry
        assert isinstance(language_registry, LanguageRegistry)
    
    def test_global_registry_singleton_behavior(self):
        """Test that global registry behaves like singleton."""
        from eiplgrader.languages.registry import language_registry as reg1
        from eiplgrader.languages.registry import language_registry as reg2
        
        assert reg1 is reg2
    
    def test_global_registry_modifications_persist(self):
        """Test that modifications to global registry persist."""
        from eiplgrader.languages.registry import language_registry
        
        # Remember initial state
        initial_languages = language_registry.list_languages()
        
        # Add a test adapter
        language_registry.register("test_global", MockAdapter)
        
        # Check it's there
        assert "test_global" in language_registry.list_languages()
        assert language_registry.is_supported("test_global")
        
        # Clean up
        if "test_global" in language_registry._adapters:
            del language_registry._adapters["test_global"]


class TestRegistryIntegrationWithRealAdapters:
    """Test registry integration with real adapter classes."""
    
    def setup_method(self):
        """Set up registry with real adapters."""
        self.registry = LanguageRegistry()
        self.registry.register("python", PythonAdapter)
        self.registry.register("java", JavaAdapter)
    
    def test_real_adapter_registration(self):
        """Test registering and using real adapters."""
        # Test Python adapter
        python_adapter = self.registry.get_adapter("python")
        assert isinstance(python_adapter, PythonAdapter)
        
        config = python_adapter.get_config()
        assert config.name == "python"
        assert config.display_name == "Python"
        
        # Test Java adapter
        java_adapter = self.registry.get_adapter("java")
        assert isinstance(java_adapter, JavaAdapter)
        
        config = java_adapter.get_config()
        assert config.name == "java"
        assert config.display_name == "Java"
    
    def test_real_adapter_methods(self):
        """Test that real adapters work through registry."""
        python_adapter = self.registry.get_adapter("python")
        
        # Test prompt generation
        prompt = python_adapter.generate_prompt(
            student_response="adds two numbers",
            function_name="add",
            gen_type="cgbg"
        )
        assert "add" in prompt
        assert "adds two numbers" in prompt
        
        # Test code extraction
        response = "```python\ndef add(a, b):\n    return a + b\n```"
        codes = python_adapter.extract_code(response)
        assert len(codes) == 1
        assert "def add(a, b):" in codes[0]
        
        # Test code normalization
        code = "def add(a, b):\n    # Add two numbers\n    return a + b"
        normalized = python_adapter.normalize_code(code)
        assert "# Add two numbers" not in normalized
        assert "def add(a, b):" in normalized
    
    def test_registry_language_listing(self):
        """Test listing languages with real adapters."""
        languages = self.registry.list_languages()
        assert "python" in languages
        assert "java" in languages
        assert languages == sorted(languages)  # Should be sorted


class TestRegistryErrorHandling:
    """Test registry error handling scenarios."""
    
    def setup_method(self):
        """Set up registry for error testing."""
        self.registry = LanguageRegistry()
    
    def test_register_invalid_adapter(self):
        """Test registering invalid adapter class."""
        # This should not crash, but might cause issues later
        class NotAnAdapter:
            pass
        
        # Registry doesn't validate at registration time
        self.registry.register("invalid", NotAnAdapter)
        
        # But getting adapter should handle the error gracefully
        adapter = self.registry.get_adapter("invalid")
        # Will create instance but it won't be a valid adapter
        assert adapter is not None
        assert not isinstance(adapter, LanguageAdapter)
    
    def test_get_adapter_instantiation_error(self):
        """Test adapter instantiation error handling."""
        
        class ErrorAdapter(LanguageAdapter):
            def __init__(self):
                raise ValueError("Adapter initialization failed")
            
            def get_config(self):
                pass
            def generate_prompt(self, *args, **kwargs):
                pass
            def extract_code(self, response):
                pass
            def normalize_code(self, code):
                pass
        
        self.registry.register("error", ErrorAdapter)
        
        # This should raise the error from adapter initialization
        with pytest.raises(ValueError, match="Adapter initialization failed"):
            self.registry.get_adapter("error")
    
    def test_concurrent_access(self):
        """Test concurrent access to registry."""
        import threading
        
        results = []
        errors = []
        
        def register_and_get(name):
            try:
                self.registry.register(f"mock_{name}", MockAdapter)
                adapter = self.registry.get_adapter(f"mock_{name}")
                results.append((name, adapter is not None))
            except Exception as e:
                errors.append((name, str(e)))
        
        threads = []
        for i in range(10):
            thread = threading.Thread(target=register_and_get, args=(i,))
            threads.append(thread)
            thread.start()
        
        for thread in threads:
            thread.join()
        
        # All operations should succeed
        assert len(errors) == 0
        assert len(results) == 10
        assert all(success for name, success in results)
    
    def test_registry_with_none_values(self):
        """Test registry behavior with None values."""
        # Test registering None (should not crash but will cause issues later)
        self.registry.register("none_adapter", None)
        
        # Getting None adapter should return None
        adapter = self.registry.get_adapter("none_adapter")
        assert adapter is None
    
    def test_registry_memory_behavior(self):
        """Test registry memory usage and cleanup."""
        # Register many adapters
        for i in range(100):
            self.registry.register(f"test_{i}", MockAdapter)
        
        # Verify they're all there
        assert len(self.registry.list_languages()) == 100
        
        # Get adapters multiple times
        adapters = []
        for i in range(10):
            for j in range(10):
                adapter = self.registry.get_adapter(f"test_{j}")
                adapters.append(adapter)
        
        # Should have created 100 adapter instances
        assert len(adapters) == 100
        
        # All should be different instances
        assert len(set(id(adapter) for adapter in adapters)) == 100
