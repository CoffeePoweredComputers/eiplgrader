"""Language support for eiplgrader."""

from .registry import language_registry
from .base import LanguageAdapter, LanguageConfig

# Import new simplified adapters
from .adapters.python_adapter import PythonAdapter
from .adapters.javascript_adapter import JavaScriptAdapter
from .adapters.java_adapter import JavaAdapter
from .adapters.c_adapter import CAdapter
from .adapters.cpp_adapter import CppAdapter
from .adapters.go_adapter import GoAdapter
from .adapters.haskell_adapter import HaskellAdapter

# Register languages with simplified system
language_registry.register("python", PythonAdapter)
language_registry.register("javascript", JavaScriptAdapter)
language_registry.register("java", JavaAdapter)
language_registry.register("c", CAdapter)
language_registry.register("cpp", CppAdapter)
language_registry.register("go", GoAdapter)
language_registry.register("haskell", HaskellAdapter)

__all__ = ["language_registry", "LanguageAdapter", "LanguageConfig"]
