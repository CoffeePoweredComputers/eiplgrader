---
layout: default
title: Language System API
parent: API Reference
grand_parent: Developer Documentation
nav_order: 3
permalink: /developer/api/language-api
---

# Language System API Reference

Complete API documentation for the language system components.

## LanguageRegistry

Central registry for language support.

```python
from eiplgrader.languages.registry import LanguageRegistry
```

### Instance Methods

#### register

```python
def register(
    self,
    name: str,
    adapter_class: Type[LanguageAdapter]
) -> None
```

Register a language adapter.

python
registry = LanguageRegistry()
registry.register("python", PythonAdapter)
```

#### get_adapter

```python
def get_adapter(
    self,
    name: str
) -> Optional[LanguageAdapter]
```

Get adapter instance for a language.

python
registry = LanguageRegistry()
adapter = registry.get_adapter("python")
if adapter:
    config = adapter.get_config()
```

#### get_executor

```python
def get_executor(
    self,
    name: str
) -> Optional[Any]
```

Get executor class for a language.

python
ExecutorClass = LanguageRegistry.get_executor("java")
executor = ExecutorClass()
```

#### list_languages

```python
def list_languages(self) -> List[str]
```

Get list of supported languages.

python
registry = LanguageRegistry()
languages = registry.list_languages()
# Returns sorted list of registered language names
# e.g., ['c', 'cpp', 'go', 'haskell', 'java', 'javascript', 'python']
```

#### is_supported

```python
def is_supported(
    self,
    name: str
) -> bool
```

Check if a language is supported.

python
registry = LanguageRegistry()
if registry.is_supported("rust"):
    print("Rust is supported!")
else:
    print("Rust is not yet supported")
```

#### clear

```python
def clear(self) -> None
```

Clear all registered adapters.

python
registry = LanguageRegistry()
registry.clear()  # Remove all registered languages
```

## LanguageAdapter

Base class for language-specific code generation.

```python
from eiplgrader.languages.base import LanguageAdapter, LanguageConfig
```

### Abstract Methods

#### get_config

```python
@abstractmethod
def get_config(self) -> LanguageConfig
```

Return language configuration.

##### Returns

`LanguageConfig` object:

```python
@dataclass
class LanguageConfig:
    name: str                    # Canonical name
    display_name: str            # Display name
    file_extension: str          # File extension (e.g., ".py")
    compile_command: Optional[str]  # Compilation command template
    run_command: str             # Execution command template

    supports_types: List[str]    # Supported type names
    requires_types: bool         # Whether explicit types required
    timeout_multiplier: float    # Timeout adjustment factor
```

##### Example Implementation

```python
def get_config(self) -> LanguageConfig:
    return LanguageConfig(
        name="python",
        display_name="Python",
        file_extension=".py",
        compile_command=None,
        run_command="python {source}",

        supports_types=["int", "float", "str", "bool", "list", "dict"],
        requires_types=False,
        timeout_multiplier=1.0
    )
```

#### generate_prompt

```python
@abstractmethod
def generate_prompt(
    self,
    student_response: str,
    function_name: str,
    gen_type: str,
    num_to_gen: int,
    **kwargs
) -> str
```

Generate language-specific prompt for LLM.

python
def generate_prompt(self, student_response: str, function_name: str,
                   gen_type: str, num_to_gen: int, **kwargs) -> str:
    if gen_type == "cgbg":
        prompt = f"""Generate {num_to_gen} Python implementations for:
Function name: {function_name}
Description: {student_response}

Requirements:
- Use Python 3.9+ syntax
- Include proper error handling
- Each implementation should be different

Generate exactly {num_to_gen} implementations:
"""
        if "example_inputs" in kwargs:
            prompt += self._format_examples(kwargs["example_inputs"], 
                                          kwargs["example_outputs"])
    
    return prompt
```

#### extract_code

```python
@abstractmethod
def extract_code(
    self,
    llm_response: str
) -> List[str]
```

Extract code blocks from LLM response.

python
def extract_code(self, llm_response: str) -> List[str]:
    import re
    
    # Extract markdown code blocks
    pattern = r'```(?:python)?\s*\n(.*?)```'
    matches = re.findall(pattern, llm_response, re.DOTALL)
    
    if not matches:
        # Fallback: extract function definitions
        pattern = r'def\s+\w+.*?(?=def\s+\w+|$)'
        matches = re.findall(pattern, llm_response, re.DOTALL)
    
    return [self.normalize_code(code) for code in matches]
```

#### normalize_code

```python
@abstractmethod
def normalize_code(
    self,
    code: str
) -> str
```

Normalize code format (remove comments, fix indentation).

python
def normalize_code(self, code: str) -> str:
    # Remove comments for Python
    lines = []
    for line in code.split('\n'):
        # Remove inline comments
        if '#' in line:
            line = line[:line.index('#')]
        line = line.rstrip()
        if line:  # Keep non-empty lines
            lines.append(line)
    
    return '\n'.join(lines)
```

### Helper Methods

#### format_examples

```python
def format_examples(
    self,
    inputs: List[Any],
    outputs: List[Any]
) -> str
```

Format input/output examples for prompt.

python
examples = adapter.format_examples(
    inputs=[[5], [0], [3]],
    outputs=[120, 1, 6]
)
# Returns:
# Examples:
# - Input: 5 → Output: 120
# - Input: 0 → Output: 1
# - Input: 3 → Output: 6
```

## LanguageExecutor

Base class for language-specific code execution.

```python
from eiplgrader.languages.base import LanguageExecutor
```

### Abstract Methods

#### prepare_code

```python
@abstractmethod
def prepare_code(
    self,
    code: str,
    test_case: dict
) -> str
```

Prepare code with test harness.

python
def prepare_code(self, code: str, test_case: dict) -> str:
    import json
    
    params = test_case["parameters"]
    func_name = test_case.get("function_name", "solution")
    
    harness = f"""
import json

{{{code}}}

# Execute test
params = {json.dumps(params)}
result = {func_name}(**params)
print(json.dumps({{"result": result}}))
"""
    return harness
```

#### execute_test

```python
@abstractmethod
def execute_test(
    self,
    code: str,
    test_case: dict,
    timeout: int = 5
) -> dict
```

Execute code and return results.

python
{
    "passed": bool,           # Test pass/fail
    "expected": Any,          # Expected value
    "actual": Any,            # Actual value (if available)
    "error": Optional[str],   # Error message
    "error_type": Optional[str],  # Error category
    "stdout": Optional[str],  # Captured stdout
    "stderr": Optional[str],  # Captured stderr
    "execution_time": float   # Time in seconds
}
```

### Common Methods

#### cleanup

```python
def cleanup(self) -> None
```

Clean up temporary resources.

##### Example

```python
def cleanup(self):
    if hasattr(self, 'temp_dir') and self.temp_dir:
        import shutil
        shutil.rmtree(self.temp_dir, ignore_errors=True)
```

## InterpretedLanguageExecutor

Base class for interpreted languages with type inference.

```python
from eiplgrader.languages.executors.base_executors import InterpretedLanguageExecutor
```

### Additional Methods

#### validate_or_infer_types

```python
def validate_or_infer_types(
    self,
    test_case: dict
) -> Tuple[dict, Any]
```

Validate provided types or infer from values.

python
# Input without types
test_case = {
    "parameters": {"x": 5, "y": "hello"},
    "expected": "hello5"
}

params, expected = executor.validate_or_infer_types(test_case)

# test_case now contains:
# {
#     "parameters": {"x": 5, "y": "hello"},
#     "parameter_types": {"x": "integer", "y": "string"},
#     "expected": "hello5",
#     "expected_type": "string"
# }
```

#### infer_type

```python
def infer_type(
    self,
    value: Any
) -> str
```

Infer type name from Python value.

python
from eiplgrader.languages.executors.base_executors import CompiledLanguageExecutor
```

### Abstract Methods

#### get_type_mapping

```python
@abstractmethod
def get_type_mapping(self) -> dict
```

Map generic types to language-specific types.

##### Returns

Dictionary mapping generic to specific types.

##### Example

```python
def get_type_mapping(self) -> dict:
    return {
        "integer": "int",
        "float": "double",
        "string": "String",
        "boolean": "boolean",
        "list": "ArrayList",
        "dict": "HashMap"
    }
```

#### format_value

```python
@abstractmethod
def format_value(
    self,
    value: Any,
    type_str: str
) -> str
```

Format Python value as language-specific literal.

python
def format_value(self, value: Any, type_str: str) -> str:
    if type_str == "String":
        return f'"{value}"'
    elif type_str == "boolean":
        return "true" if value else "false"
    elif type_str.startswith("int[]"):
        elements = ", ".join(str(v) for v in value)
        return f"new int[]{{{elements}}}"
    else:
        return str(value)
```

### Required Methods

#### validate_types_provided

```python
def validate_types_provided(
    self,
    test_case: dict
) -> None
```

Ensure all required type information is provided.

python
# This will raise ValueError
test_case = {
    "parameters": {"x": 5},
    "expected": 10
}
executor.validate_types_provided(test_case)

# This is valid
test_case = {
    "parameters": {"x": 5},
    "parameter_types": {"x": "int"},
    "expected": 10,
    "expected_type": "int"
}
executor.validate_types_provided(test_case)
```

## Utility Functions

### validate_language_name

```python
def validate_language_name(
    name: str
) -> str
```

Validate and normalize language name.

python
# All return "python"
validate_language_name("python")
validate_language_name("Python")
validate_language_name("py")
validate_language_name("python3")
```

## Best Practices

### Language Registration

```python
# Register during module import
def register_all_languages():
    """Register all supported languages."""
    languages = [
        ("python", PythonAdapter),
        ("javascript", JSAdapter),
        # ... more languages
    ]
    
    for name, adapter in languages:
        try:
            registry.register(name, adapter)
        except Exception as e:
            logging.error(f"Failed to register {name}: {e}")
```

### Type Safety

```python
def ensure_type_compatibility(test_case: dict, language: str):
    """Ensure test case types are compatible with language."""
    executor_class = LanguageRegistry.get_executor(language)
    
    if issubclass(executor_class, CompiledLanguageExecutor):
        # Requires explicit types
        if "parameter_types" not in test_case:
            raise ValueError(
                f"{language} requires explicit parameter_types"
            )
    else:
        # Can infer types
        executor = executor_class()
        executor.validate_or_infer_types(test_case)
```

### Error Handling

```python
def safe_execute(executor: LanguageExecutor, code: str, 
                test_case: dict) -> dict:
    """Execute with comprehensive error handling."""
    try:
        return executor.execute_test(code, test_case)
    except subprocess.TimeoutExpired:
        return {
            "passed": False,
            "error": "Execution timeout",
            "error_type": "timeout"
        }
    except MemoryError:
        return {
            "passed": False,
            "error": "Memory limit exceeded",
            "error_type": "memory"
        }
    except Exception as e:
        return {
            "passed": False,
            "error": str(e),
            "error_type": "system"
        }
    finally:
        executor.cleanup()
```

## See Also

- [Adding Languages](../languages/adding-languages.html) - Guide to adding language support
- [Executors](../languages/executors.html) - Detailed executor documentation
- [Architecture](../languages/architecture.html) - Language system design
