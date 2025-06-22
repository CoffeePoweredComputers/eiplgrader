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

### Class Methods

#### register_language

```python
@classmethod
def register_language(
    cls,
    name: str,
    adapter_class: Type[LanguageAdapter],
    executor_class: Type[LanguageExecutor],
    aliases: Optional[List[str]] = None
) -> None
```

Register a new language with the system.

##### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `name` | `str` | Canonical language name |
| `adapter_class` | `Type[LanguageAdapter]` | Adapter class |
| `executor_class` | `Type[LanguageExecutor]` | Executor class |
| `aliases` | `List[str]` | Alternative names |

##### Example

```python
LanguageRegistry.register_language(
    "python",
    PythonAdapter,
    PythonExecutor,
    aliases=["py", "python3"]
)
```

#### get_adapter

```python
@classmethod
def get_adapter(
    cls,
    language: str
) -> LanguageAdapter
```

Get adapter instance for a language.

##### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `language` | `str` | Language name or alias |

##### Returns

Instantiated `LanguageAdapter`.

##### Raises

- `LanguageNotSupportedError`: If language not registered

##### Example

```python
adapter = LanguageRegistry.get_adapter("python")
config = adapter.get_config()
```

#### get_executor

```python
@classmethod
def get_executor(
    cls,
    language: str
) -> Type[LanguageExecutor]
```

Get executor class for a language.

##### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `language` | `str` | Language name or alias |

##### Returns

`LanguageExecutor` class (not instance).

##### Example

```python
ExecutorClass = LanguageRegistry.get_executor("java")
executor = ExecutorClass()
```

#### get_supported_languages

```python
@classmethod
def get_supported_languages(
    cls,
    include_aliases: bool = False
) -> List[str]
```

Get list of supported languages.

##### Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `include_aliases` | `bool` | `False` | Include language aliases |

##### Returns

List of language names.

##### Example

```python
# Canonical names only
languages = LanguageRegistry.get_supported_languages()
# ['python', 'javascript', 'java', 'cpp', 'c', 'go', 'haskell']

# With aliases
all_names = LanguageRegistry.get_supported_languages(include_aliases=True)
# ['python', 'py', 'python3', 'javascript', 'js', 'node', ...]
```

#### is_supported

```python
@classmethod
def is_supported(
    cls,
    language: str
) -> bool
```

Check if a language is supported.

##### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `language` | `str` | Language name to check |

##### Returns

`True` if supported, `False` otherwise.

##### Example

```python
if LanguageRegistry.is_supported("rust"):
    print("Rust is supported!")
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
    docker_image: Optional[str]  # Docker image name
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
        docker_image="python:3.9-slim",
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

##### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `student_response` | `str` | Natural language description |
| `function_name` | `str` | Target function name |
| `gen_type` | `str` | Generation type ("cgbg" or "redef") |
| `num_to_gen` | `int` | Number of variants to generate |
| `**kwargs` | `dict` | Additional parameters |

##### Common kwargs

- `example_inputs`: List of example inputs
- `example_outputs`: List of example outputs
- `assumptions`: Additional assumptions (redef)
- `function_signature`: Function signature (redef)
- `language_version`: Language version requirement
- `use_type_hints`: Whether to include type hints

##### Returns

Formatted prompt string for LLM.

##### Example Implementation

```python
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

##### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `llm_response` | `str` | Raw LLM response |

##### Returns

List of extracted code strings.

##### Example Implementation

```python
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

##### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `code` | `str` | Raw code string |

##### Returns

Normalized code string.

##### Example Implementation

```python
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

##### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `inputs` | `List[Any]` | Example input values |
| `outputs` | `List[Any]` | Example output values |

##### Returns

Formatted example string.

##### Example

```python
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

##### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `code` | `str` | Function code to test |
| `test_case` | `dict` | Test case dictionary |

##### Returns

Complete code with test harness.

##### Example Implementation

```python
def prepare_code(self, code: str, test_case: dict) -> str:
    import json
    
    params = test_case["parameters"]
    func_name = test_case.get("function_name", "solution")
    
    harness = f"""
import json

{code}

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

##### Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `code` | `str` | required | Code to execute |
| `test_case` | `dict` | required | Test case |
| `timeout` | `int` | `5` | Timeout in seconds |

##### Returns

Result dictionary:

```python
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

##### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `test_case` | `dict` | Test case dictionary |

##### Returns

Tuple of (parameters, expected_value).

##### Side Effects

Updates test_case with inferred types if not provided.

##### Example

```python
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

##### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `value` | `Any` | Value to analyze |

##### Returns

Generic type name string.

##### Type Mapping

| Python Type | Inferred Type |
|-------------|---------------|
| `bool` | `"boolean"` |
| `int` | `"integer"` |
| `float` | `"float"` |
| `str` | `"string"` |
| `list` | `"list"` |
| `dict` | `"dict"` |
| `None` | `"null"` |
| others | `"any"` |

## CompiledLanguageExecutor

Base class for compiled languages requiring explicit types.

```python
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

##### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `value` | `Any` | Python value |
| `type_str` | `str` | Language-specific type |

##### Returns

Formatted literal string.

##### Example

```python
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

##### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `test_case` | `dict` | Test case to validate |

##### Raises

- `ValueError`: If required types are missing

##### Example

```python
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

### create_language_config

```python
def create_language_config(
    name: str,
    **kwargs
) -> LanguageConfig
```

Helper to create language configuration.

##### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `name` | `str` | Language name |
| `**kwargs` | `dict` | Configuration options |

##### Example

```python
config = create_language_config(
    "rust",
    display_name="Rust",
    file_extension=".rs",
    compile_command="rustc {source} -o {output}",
    run_command="./{output}",
    requires_types=True
)
```

### validate_language_name

```python
def validate_language_name(
    name: str
) -> str
```

Validate and normalize language name.

##### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `name` | `str` | Language name to validate |

##### Returns

Canonical language name.

##### Raises

- `LanguageNotSupportedError`: If invalid

##### Example

```python
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
        ("python", PythonAdapter, PythonExecutor, ["py", "python3"]),
        ("javascript", JSAdapter, JSExecutor, ["js", "node"]),
        # ... more languages
    ]
    
    for name, adapter, executor, aliases in languages:
        try:
            LanguageRegistry.register_language(
                name, adapter, executor, aliases
            )
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