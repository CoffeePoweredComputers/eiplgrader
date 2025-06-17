"""Language support for eiplgrader."""

from .registry import language_registry
from .base import LanguageAdapter, LanguageExecutor, LanguageConfig

# Import adapters
from .adapters.python_adapter import PythonAdapter
from .adapters.bash_adapter import BashAdapter
from .adapters.ruby_adapter import RubyAdapter
from .adapters.php_adapter import PhpAdapter
from .adapters.javascript_adapter import JavascriptAdapter
from .adapters.c_adapter import CAdapter
from .adapters.go_adapter import GoAdapter
from .adapters.typescript_adapter import TypescriptAdapter
from .adapters.cpp_adapter import CppAdapter
from .adapters.rust_adapter import RustAdapter
from .adapters.kotlin_adapter import KotlinAdapter
from .adapters.java_adapter import JavaAdapter
from .adapters.sql_adapter import SqlAdapter
from .adapters.ocaml_adapter import OcamlAdapter
from .adapters.haskell_adapter import HaskellAdapter

# Import executors
from .executors.python_executor import PythonExecutor
from .executors.bash_executor import BashExecutor
from .executors.ruby_executor import RubyExecutor
from .executors.php_executor import PhpExecutor
from .executors.javascript_executor import JavascriptExecutor
from .executors.c_executor import CExecutor
from .executors.go_executor import GoExecutor
from .executors.typescript_executor import TypescriptExecutor
from .executors.cpp_executor import CppExecutor
from .executors.rust_executor import RustExecutor
from .executors.kotlin_executor import KotlinExecutor
from .executors.java_executor import JavaExecutor
from .executors.sql_executor import SqlExecutor
from .executors.ocaml_executor import OcamlExecutor
from .executors.haskell_executor import HaskellExecutor

# Register languages
language_registry.register("python", PythonAdapter, PythonExecutor)
language_registry.register("bash", BashAdapter, BashExecutor)
language_registry.register("ruby", RubyAdapter, RubyExecutor)
language_registry.register("php", PhpAdapter, PhpExecutor)
language_registry.register("javascript", JavascriptAdapter, JavascriptExecutor)
language_registry.register("c", CAdapter, CExecutor)
language_registry.register("go", GoAdapter, GoExecutor)
language_registry.register("typescript", TypescriptAdapter, TypescriptExecutor)
language_registry.register("cpp", CppAdapter, CppExecutor)
language_registry.register("rust", RustAdapter, RustExecutor)
language_registry.register("kotlin", KotlinAdapter, KotlinExecutor)
language_registry.register("java", JavaAdapter, JavaExecutor)
language_registry.register("sql", SqlAdapter, SqlExecutor)
language_registry.register("ocaml", OcamlAdapter, OcamlExecutor)
language_registry.register("haskell", HaskellAdapter, HaskellExecutor)

__all__ = ["language_registry", "LanguageAdapter", "LanguageExecutor", "LanguageConfig"]
