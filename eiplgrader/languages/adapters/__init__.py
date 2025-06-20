"""Simple language adapters."""

from .python_adapter import PythonAdapter
from .javascript_adapter import JavaScriptAdapter
from .java_adapter import JavaAdapter
from .c_adapter import CAdapter
from .cpp_adapter import CppAdapter
from .go_adapter import GoAdapter
from .haskell_adapter import HaskellAdapter

__all__ = [
    "PythonAdapter",
    "JavaScriptAdapter",
    "JavaAdapter",
    "CAdapter",
    "CppAdapter",
    "GoAdapter",
    "HaskellAdapter",
]
