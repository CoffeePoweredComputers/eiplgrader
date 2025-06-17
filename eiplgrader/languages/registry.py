"""Central registry for language implementations."""

from typing import Dict, Type, Optional, List
from .base import LanguageAdapter, LanguageExecutor


class LanguageRegistry:
    """Central registry for language implementations"""

    def __init__(self):
        self._adapters: Dict[str, Type[LanguageAdapter]] = {}
        self._executors: Dict[str, Type[LanguageExecutor]] = {}

    def register(
        self,
        language: str,
        adapter: Type[LanguageAdapter],
        executor: Type[LanguageExecutor],
    ) -> None:
        """Register a language implementation"""
        self._adapters[language.lower()] = adapter
        self._executors[language.lower()] = executor

    def get_adapter(self, language: str) -> Optional[LanguageAdapter]:
        """Get adapter instance for language"""
        adapter_class = self._adapters.get(language.lower())
        return adapter_class() if adapter_class else None

    def get_executor(self, language: str) -> Optional[LanguageExecutor]:
        """Get executor instance for language"""
        executor_class = self._executors.get(language.lower())
        return executor_class() if executor_class else None

    def list_languages(self) -> List[str]:
        """List all registered languages"""
        return sorted(self._adapters.keys())


# Global registry instance
language_registry = LanguageRegistry()
