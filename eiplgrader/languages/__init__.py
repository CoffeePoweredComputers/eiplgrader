"""Language support for eiplgrader."""

from .registry import language_registry
from .base import LanguageAdapter, LanguageExecutor, LanguageConfig

# Import and register Python adapter/executor
from .adapters.python_adapter import PythonAdapter
from .executors.python_executor import PythonExecutor

# Import and register Bash adapter/executor
from .adapters.bash_adapter import BashAdapter
from .executors.bash_executor import BashExecutor

# Import and register Ruby adapter/executor
from .adapters.ruby_adapter import RubyAdapter
from .executors.ruby_executor import RubyExecutor

# Import and register PHP adapter/executor
from .adapters.php_adapter import PhpAdapter
from .executors.php_executor import PhpExecutor

# Import and register JavaScript adapter/executor
from .adapters.javascript_adapter import JavascriptAdapter
from .executors.javascript_executor import JavascriptExecutor

# Import and register C adapter/executor
from .adapters.c_adapter import CAdapter
from .executors.c_executor import CExecutor

# Register Python
language_registry.register("python", PythonAdapter, PythonExecutor)

# Import and register Go adapter/executor
from .adapters.go_adapter import GoAdapter
from .executors.go_executor import GoExecutor

# Register Go
language_registry.register("go", GoAdapter, GoExecutor)

# Register Bash
language_registry.register("bash", BashAdapter, BashExecutor)

# Register Ruby
language_registry.register("ruby", RubyAdapter, RubyExecutor)

# Register PHP
language_registry.register("php", PhpAdapter, PhpExecutor)

# Register JavaScript
language_registry.register("javascript", JavascriptAdapter, JavascriptExecutor)

# Import and register TypeScript adapter/executor
from .adapters.typescript_adapter import TypescriptAdapter
from .executors.typescript_executor import TypescriptExecutor

# Import and register C adapter/executor
from .adapters.c_adapter import CAdapter
from .executors.c_executor import CExecutor

# Register TypeScript
language_registry.register("typescript", TypescriptAdapter, TypescriptExecutor)

# Register C
language_registry.register("c", CAdapter, CExecutor)

# Import and register C++ adapter/executor
from .adapters.cpp_adapter import CppAdapter
from .executors.cpp_executor import CppExecutor

# Register C++
language_registry.register("cpp", CppAdapter, CppExecutor)

# Import and register Rust adapter/executor
from .adapters.rust_adapter import RustAdapter
from .executors.rust_executor import RustExecutor

# Register Rust
language_registry.register("rust", RustAdapter, RustExecutor)

# Import and register Kotlin adapter/executor
from .adapters.kotlin_adapter import KotlinAdapter
from .executors.kotlin_executor import KotlinExecutor

# Register Kotlin
language_registry.register("kotlin", KotlinAdapter, KotlinExecutor)

# Import and register Java adapter/executor
from .adapters.java_adapter import JavaAdapter
from .executors.java_executor import JavaExecutor

# Register Java
language_registry.register("java", JavaAdapter, JavaExecutor)

# Import and register SQL adapter/executor
from .adapters.sql_adapter import SqlAdapter
from .executors.sql_executor import SqlExecutor

# Register SQL
language_registry.register("sql", SqlAdapter, SqlExecutor)

# Import and register OCaml adapter/executor
from .adapters.ocaml_adapter import OcamlAdapter
from .executors.ocaml_executor import OcamlExecutor

# Register OCaml
language_registry.register("ocaml", OcamlAdapter, OcamlExecutor)

# Import and register Haskell adapter/executor
from .adapters.haskell_adapter import HaskellAdapter
from .executors.haskell_executor import HaskellExecutor

# Register Haskell
language_registry.register("haskell", HaskellAdapter, HaskellExecutor)

__all__ = ["language_registry", "LanguageAdapter", "LanguageExecutor", "LanguageConfig"]
