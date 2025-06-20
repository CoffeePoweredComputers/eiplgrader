"""Simple registry for language adapters."""

from typing import Dict, Type, List, Optional, Any
from .base import LanguageAdapter


class LanguageRegistry:
    """Simple registry for language implementations."""

    def __init__(self) -> None:
        self._adapters: Dict[str, Type[LanguageAdapter]] = {}

    def register(self, name: str, adapter_class: Type[LanguageAdapter]) -> None:
        """Register a language adapter."""
        self._adapters[name.lower()] = adapter_class

    def get_adapter(self, name: str) -> Optional[LanguageAdapter]:
        """Get adapter instance for language."""
        adapter_class = self._adapters.get(name.lower())
        return adapter_class() if adapter_class else None

    def get_executor(self, name: str) -> Optional[Any]:
        """Get executor instance for language."""
        # Import executors dynamically to avoid circular imports
        executor_mapping = {
            "python": "python_executor.PythonExecutor",
            "javascript": "javascript_executor.JavaScriptExecutor",
            "java": "java_executor.JavaExecutor",
            "c": "c_executor.CExecutor",
            "cpp": "cpp_executor.CppExecutor",
            "go": "go_executor.GoExecutor",
            "haskell": "haskell_executor.HaskellExecutor",
        }
        
        executor_path = executor_mapping.get(name.lower())
        if not executor_path:
            return None
            
        module_name, class_name = executor_path.rsplit(".", 1)
        try:
            # Import the executor module dynamically
            from importlib import import_module
            module = import_module(f".executors.{module_name}", package="eiplgrader.languages")
            executor_class = getattr(module, class_name)
            return executor_class()
        except (ImportError, AttributeError):
            return None

    def list_languages(self) -> List[str]:
        """List all registered languages."""
        return sorted(self._adapters.keys())

    def is_supported(self, name: str) -> bool:
        """Check if language is supported."""
        return name.lower() in self._adapters


# Global registry instance
language_registry = LanguageRegistry()
