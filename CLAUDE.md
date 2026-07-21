# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Development Commands

### Testing
- `python -m pytest tests/` - Run all tests
- `python -m pytest --cov=eiplgrader/ tests/` - Run tests with coverage

### Code Quality
- `./lint.sh` - Run all linting, formatting, and type checks (creates venv if needed)
  - Runs pylint for code quality (target: 10.00/10)
  - Runs black for code formatting
  - Runs mypy for type checking
  - Runs pytest with coverage
- `black eiplgrader/ tests/` - Auto-format code
- `pylint eiplgrader/ tests/` - Lint code
- `mypy eiplgrader/` - Type check

### Documentation Validation
- `python examples/enhanced_documentation_validator.py` - Validate all documentation examples work correctly
  - Tests API examples from documentation
  - Validates return types and data structures
  - Checks error handling examples
  - Uses .env file for OPENAI_API_KEY (same as other examples)

### Documentation Site
- Mermaid diagrams are supported via client-side rendering (GitHub Pages compatible)
- 13+ architecture diagrams throughout the documentation
- Test page available at `/test-mermaid.html` when site is built

### Package Management
- Uses Poetry for dependency management (`poetry install --with dev`)
- Falls back to pip with `requirements.txt` if Poetry unavailable

## Architecture

### Core Components

**eiplgrader.codegen.CodeGenerator**: The main interface for generating code from student responses. Currently supports OpenAI and Ollama providers through a pluggable architecture using `ModelRequest` subclasses. Anthropic and Meta support are planned for future releases.

**eiplgrader.tester.CodeTester**: Executes generated code against test cases and returns structured results. Handles three test modes:
- `inplace="0"`: Normal function calls with return values
- `inplace="1"`: In-place modifications (function modifies arguments)  
- `inplace="2"`: Functions that both modify in-place and return values

### Generation Types
- `"cgbg"`: Code Generation Based Grading - generates functions from natural language descriptions
- `"redef"`: Function redefinition - generates functions from signatures and assumptions
- Segmentation: Optional feature that maps explanation text to generated code sections

### Model Provider Architecture
Each model provider implements the `ModelRequest` interface with methods:
- `request_function_generation()`: Generate code from prompts
- `request_segmentation()`: Map explanations to code sections

Currently supported: OpenAI (full), Ollama (full), Anthropic/Meta (placeholders).

### Test Case Format
Test cases use dictionary format:
```python
{
    "parameters": {"param_name": value, ...},
    "expected": expected_output
}
```

The CodeTester dynamically loads generated code, executes it with test parameters, and compares results.

### Language Support Architecture

**eiplgrader.languages.base.LanguageAdapter**: Abstract base class for language-specific code generation with methods:
- `get_config()`: Return language configuration (name, file extensions, commands, etc.)
- `generate_prompt(student_response, function_name, gen_type, num_to_gen, **kwargs)`: Generate language-specific LLM prompts
- `extract_code(llm_response)`: Extract code blocks from LLM responses
- `normalize_code(code)`: Remove comments and standardize formatting

**eiplgrader.languages.base.LanguageExecutor**: Abstract base class for language-specific code execution with methods:
- `prepare_code(code, test_case)`: Prepare code for execution with test harness
- `execute_test(code, test_case)`: Execute code and return results
- `cleanup()`: Clean up temporary resources
- `validate_types_provided()`: Validate required type information (static languages)
- `infer_type()`: Infer type from value (dynamic languages)

**eiplgrader.languages.executors.base_executors**: Specialized base classes:
- `InterpretedLanguageExecutor`: For Python, JavaScript - supports type inference
- `CompiledLanguageExecutor`: For C, C++, Java, Go, Haskell - handles compilation

Currently supported languages with testing status:
- ✅ **Python** (`python_adapter.py`, `python_executor.py`) - Fully functional, type inference supported
- ✅ **JavaScript** (`javascript_adapter.py`, `javascript_executor.py`) - Fully functional, type inference supported
- ✅ **Java** (`java_adapter.py`, `java_executor.py`) - Fully functional, types required
- ✅ **Go** (`go_adapter.py`, `go_executor.py`) - Fully functional, types required
- ✅ **C++** (`cpp_adapter.py`, `cpp_executor.py`) - Fully functional, types required
- ✅ **C** (`c_adapter.py`, `c_executor.py`) - Fully functional, types required
- ✅ **Haskell** (`haskell_adapter.py`, `haskell_executor.py`) - Fully functional, types required

All language adapters follow the same interface for consistent multi-language support.

### Language Executor Development Notes
- **Python**: Uses InterpretedLanguageExecutor, JSON input/output, type inference from values
- **JavaScript**: Uses InterpretedLanguageExecutor with Node.js, JSON input/output, type inference from values
- **Go**: Uses CompiledLanguageExecutor with embedded values, requires explicit Go types
- **Java**: Uses CompiledLanguageExecutor with embedded values, requires explicit Java types
- **C++**: Uses CompiledLanguageExecutor with embedded values, requires explicit C++ types
- **C**: Uses CompiledLanguageExecutor with embedded values, requires explicit C types
- **Haskell**: Uses CompiledLanguageExecutor with embedded values, requires explicit Haskell types

### Type System Summary
- **Languages with native JSON support (type inference available)**: 
  - Python, JavaScript - test cases can omit type annotations
  - These languages parse JSON input and output natively
- **Languages without native JSON support (exact types required)**:
  - C, C++, Java, Go, Haskell - must specify parameter_types and expected_type
  - C: Use exact C types (e.g., `"int"`, `"char*"`, `"int*"`)
  - C++: Use exact C++ types (e.g., `"int"`, `"std::string"`, `"std::vector<int>"`)
  - Java: Use exact Java types (e.g., `"int"`, `"String"`, `"boolean"`, `"int[]"`)
  - Go: Use exact Go types (e.g., `"int"`, `"string"`, `"bool"`, `"[]int"`, `"[]string"`, `"[]float64"`)
  - Haskell: Use exact Haskell types (e.g., `"Int"`, `"String"`, `"[Int]"`)
  - These languages use embedded test values instead of JSON parsing