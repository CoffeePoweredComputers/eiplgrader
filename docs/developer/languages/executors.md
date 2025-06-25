---
layout: default
title: Executors
parent: Language System
grand_parent: Developer Documentation
nav_order: 3
permalink: /developer/languages/executors
---

# Language Executors

Deep dive into the executor system that powers code execution in EiplGrader.

## Overview

Language executors are responsible for:
- Preparing code with test harnesses
- Executing code in a safe environment
- Handling language-specific type systems
- Normalizing and returning results

## Executor Hierarchy

![Language Executor Hierarchy](/assets/diagrams/executors_diagram_1_corrected.svg)

## Base Executor Classes

### LanguageExecutor (Abstract Base)

```python
from abc import ABC, abstractmethod

class LanguageExecutor(ABC):
    """Base class for all language executors."""
    
    @abstractmethod
    def prepare_code(self, code: str, test_case: dict) -> str:
        """Prepare code with test harness for execution."""
        pass
    
    @abstractmethod
    def execute_test(self, code: str, test_case: dict, 
                    timeout: int = 5) -> dict:
        """Execute code and return results."""
        pass
    
    def cleanup(self):
        """Clean up temporary resources."""
        if hasattr(self, 'temp_dir') and self.temp_dir:
            shutil.rmtree(self.temp_dir, ignore_errors=True)
```

### InterpretedLanguageExecutor

For languages with:
- Runtime type inspection (Python, JavaScript)
- Native JSON support
- Direct script execution

```python
class InterpretedLanguageExecutor(LanguageExecutor):
    """Base for interpreted languages with type inference."""
    
    def validate_or_infer_types(self, test_case: dict) -> Tuple[dict, Any]:
        """Validate provided types or infer from values."""
        parameters = test_case["parameters"]
        expected = test_case["expected"]
        
        # Check if types are provided
        if "parameter_types" in test_case:
            # Validate types match values
            self._validate_types_match(parameters, test_case["parameter_types"])
        else:
            # Infer types from values
            test_case["parameter_types"] = {
                name: self.infer_type(value)
                for name, value in parameters.items()
            }
        
        if "expected_type" not in test_case:
            test_case["expected_type"] = self.infer_type(expected)
        
        return parameters, expected
    
    def infer_type(self, value: Any) -> str:
        """Infer type from Python value."""
        if isinstance(value, bool):
            return "boolean"
        elif isinstance(value, int):
            return "integer"
        elif isinstance(value, float):
            return "float"
        elif isinstance(value, str):
            return "string"
        elif isinstance(value, list):
            return "list"
        elif isinstance(value, dict):
            return "dict"
        else:
            return "any"
```

### CompiledLanguageExecutor

For languages with:
- Static type systems (Java, C++, Go)
- Compilation step required
- No native JSON support

```python
class CompiledLanguageExecutor(LanguageExecutor):
    """Base for compiled languages requiring explicit types."""
    
    def validate_types_provided(self, test_case: dict):
        """Ensure all required type information is provided."""
        if "parameter_types" not in test_case:
            raise ValueError(
                f"parameter_types required for {self.__class__.__name__}"
            )
        
        if "expected_type" not in test_case:
            raise ValueError(
                f"expected_type required for {self.__class__.__name__}"
            )
        
        # Validate all parameters have types
        for param in test_case["parameters"]:
            if param not in test_case["parameter_types"]:
                raise ValueError(f"Missing type for parameter: {param}")
    
    @abstractmethod
    def get_type_mapping(self) -> dict:
        """Map generic types to language-specific types."""
        pass
    
    @abstractmethod
    def format_value(self, value: Any, type_str: str) -> str:
        """Format Python value as language-specific literal."""
        pass
```

## Executor Implementations

### Python Executor Example

```python
class PythonExecutor(InterpretedLanguageExecutor):
    def prepare_code(self, code: str, test_case: dict) -> str:
        """Prepare Python code with JSON test harness."""
        params, expected = self.validate_or_infer_types(test_case)
        
        harness = f"""
import json
import sys

# Generated function
{% raw %}{{{code}}}{% endraw %}

# Test execution
try:
    params = {json.dumps(params)}
    result = {test_case.get('function_name', 'solution')}(**params)
    print(json.dumps({% raw %}{{"result": result, "success": True}}{% endraw %}))
except Exception as e:
    print(json.dumps({% raw %}{{"error": str(e), "success": False}}{% endraw %}))
    sys.exit(1)
"""
        return harness
    
    def execute_test(self, code: str, test_case: dict, timeout: int = 5) -> dict:
        """Execute Python code and return results."""
        full_code = self.prepare_code(code, test_case)
        
        try:
            result = subprocess.run(
                [sys.executable, '-c', full_code],
                capture_output=True,
                text=True,
                timeout=timeout
            )
            
            if result.returncode != 0:
                return {
                    'passed': False,
                    'error': result.stderr,
                    'error_type': 'runtime'
                }
            
            output = json.loads(result.stdout)
            if not output['success']:
                return {
                    'passed': False,
                    'error': output['error'],
                    'error_type': 'runtime'
                }
            
            actual = output['result']
            expected = test_case['expected']
            
            return {
                'passed': actual == expected,
                'expected': expected,
                'actual': actual
            }
            
        except subprocess.TimeoutExpired:
            return {
                'passed': False,
                'error': 'Execution timeout',
                'error_type': 'timeout'
            }
        except json.JSONDecodeError:
            return {
                'passed': False,
                'error': f'Invalid output: {result.stdout}',
                'error_type': 'output_format'
            }
```

### Java Executor Example

```python
class JavaExecutor(CompiledLanguageExecutor):
    def get_type_mapping(self) -> dict:
        return {
            "integer": "int",
            "float": "double",
            "string": "String",
            "boolean": "boolean",
            "list": "List",
            "dict": "Map"
        }
    
    def format_value(self, value: Any, type_str: str) -> str:
        """Format value as Java literal."""
        if type_str == "String":
            return f'"{value}"'
        elif type_str == "boolean":
            return "true" if value else "false"
        elif type_str.startswith("int[]"):
            return f"new int[]{% raw %}{{{', '.join(map(str, value))}}}{% endraw %}"
        elif type_str.startswith("List"):
            return f"Arrays.asList({', '.join(self.format_value(v, 'Object') for v in value)})"
        else:
            return str(value)
    
    def prepare_code(self, code: str, test_case: dict) -> str:
        """Prepare Java code with embedded test values."""
        self.validate_types_provided(test_case)
        
        # Build parameter declarations
        param_decls = []
        for name, value in test_case["parameters"].items():
            java_type = test_case["parameter_types"][name]
            formatted = self.format_value(value, java_type)
            param_decls.append(f"        {java_type} {name} = {formatted};")
        
        # Build method call
        func_name = test_case.get("function_name", "solution")
        param_names = ", ".join(test_case["parameters"].keys())
        
        harness = f"""
import java.util.*;

public class TestHarness {
    {% raw %}{{{code}}}{% endraw %}
    
    public static void main(String[] args) {
{% raw %}{{{chr(10).join(param_decls)}}}{% endraw %}
        
        Solution sol = new Solution();
        Object result = sol.{func_name}({param_names});
        
        System.out.println({% raw %}"{\"result\": " + formatResult(result) + "}"{% endraw %});
    }
    
    private static String formatResult(Object obj) {
        if (obj instanceof String) {
            return "\"" + obj + "\"";
        } else if (obj instanceof int[]) {
            return Arrays.toString((int[])obj);
        } else {
            return String.valueOf(obj);
        }
    }
}
"""
        return harness
    
    def compile_code(self, source_file: str) -> tuple[bool, str]:
        """Compile Java source file."""
        result = subprocess.run(
            ['javac', source_file],
            capture_output=True,
            text=True
        )
        return result.returncode == 0, result.stderr
    
    def execute_test(self, code: str, test_case: dict, timeout: int = 5) -> dict:
        """Compile and execute Java code."""
        # Create temporary directory
        self.temp_dir = tempfile.mkdtemp()
        source_file = os.path.join(self.temp_dir, 'TestHarness.java')
        
        try:
            # Write source code
            full_code = self.prepare_code(code, test_case)
            with open(source_file, 'w') as f:
                f.write(full_code)
            
            # Compile
            success, error = self.compile_code(source_file)
            if not success:
                return {
                    'passed': False,
                    'error': error,
                    'error_type': 'compilation'
                }
            
            # Execute
            result = subprocess.run(
                ['java', '-cp', self.temp_dir, 'TestHarness'],
                capture_output=True,
                text=True,
                timeout=timeout
            )
            
            if result.returncode != 0:
                return {
                    'passed': False,
                    'error': result.stderr,
                    'error_type': 'runtime'
                }
            
            # Parse output and compare
            # Implementation continues...
            
        finally:
            self.cleanup()
```

## In-Place Modification Support

Executors must handle three test modes:

### Mode 0: Return Value Testing (Default)
```python
def prepare_code_mode_0(self, code: str, test_case: dict) -> str:
    """Standard function call with return value check."""
    return f"""
{code}
result = {func_name}({params})
assert result == {expected}
"""
```

### Mode 1: In-Place Modification
```python
def prepare_code_mode_1(self, code: str, test_case: dict) -> str:
    """Test in-place modification of arguments."""
    return f"""
{code}
# Create mutable copy
test_data = {params}
{func_name}(test_data)
assert test_data == {expected}
"""
```

### Mode 2: Both Modification and Return
```python
def prepare_code_mode_2(self, code: str, test_case: dict) -> str:
    """Test both in-place modification and return value."""
    return f"""
{code}
test_data = {params}
result = {func_name}(test_data)
assert test_data == {expected_state}
assert result == {expected_return}
"""
```

## Error Handling

### Error Categories

```python
class ExecutorError:
    COMPILATION = "compilation"      # Static language compilation failed
    RUNTIME = "runtime"             # Code executed but crashed
    TIMEOUT = "timeout"             # Execution exceeded time limit
    OUTPUT_FORMAT = "output_format" # Could not parse output
    SYSTEM = "system"              # Executor system error
```

### Error Response Format

```python
def create_error_response(error_type: str, message: str, 
                         details: dict = None) -> dict:
    """Create standardized error response."""
    response = {
        'passed': False,
        'error_type': error_type,
        'error': message
    }
    
    if details:
        response.update(details)
    
    return response
```

## Performance Optimization

### Executor Pooling

```python
class ExecutorPool:
    """Reuse executor instances for better performance."""
    
    def __init__(self, max_size: int = 10):
        self.pool = defaultdict(list)
        self.max_size = max_size
    
    def acquire(self, language: str) -> LanguageExecutor:
        if self.pool[language]:
            return self.pool[language].pop()
        return LanguageRegistry.get_executor(language)()
    
    def release(self, language: str, executor: LanguageExecutor):
        if len(self.pool[language]) < self.max_size:
            executor.cleanup()  # Reset state
            self.pool[language].append(executor)
```

### Compilation Caching

```python
class CompilationCache:
    """Cache compiled binaries for static languages."""
    
    def __init__(self, cache_dir: str = ".compilation_cache"):
        self.cache_dir = cache_dir
        os.makedirs(cache_dir, exist_ok=True)
    
    def get_cache_key(self, code: str, language: str) -> str:
        """Generate cache key from code content."""
        content = f"{language}:{code}"
        return hashlib.sha256(content.encode()).hexdigest()
    
    def get(self, code: str, language: str) -> Optional[str]:
        """Retrieve cached binary path."""
        key = self.get_cache_key(code, language)
        binary_path = os.path.join(self.cache_dir, key)
        
        if os.path.exists(binary_path):
            return binary_path
        return None
    
    def put(self, code: str, language: str, binary_path: str):
        """Store compiled binary in cache."""
        key = self.get_cache_key(code, language)
        cache_path = os.path.join(self.cache_dir, key)
        shutil.copy2(binary_path, cache_path)
```

## Security Considerations

### Process Isolation

```python
def execute_with_limits(command: list, timeout: int = 5, 
                       memory_mb: int = 256) -> subprocess.CompletedProcess:
    """Execute with resource limits."""
    if sys.platform == "linux":
        # Use ulimit for resource constraints
        wrapped_command = [
            "bash", "-c",
            f"ulimit -v {memory_mb * 1024}; exec {' '.join(command)}"
        ]
    else:
        wrapped_command = command
    
    return subprocess.run(
        wrapped_command,
        capture_output=True,
        text=True,
        timeout=timeout,
        env={**os.environ, "PYTHONPATH": ""}  # Clean environment
    )
```

### Input Validation

```python
def validate_test_case(test_case: dict):
    """Validate test case structure and content."""
    required = ["parameters", "expected"]
    for field in required:
        if field not in test_case:
            raise ValueError(f"Missing required field: {field}")
    
    # Validate parameter names (prevent injection)
    for param_name in test_case["parameters"]:
        if not re.match(r'^[a-zA-Z_][a-zA-Z0-9_]*$', param_name):
            raise ValueError(f"Invalid parameter name: {param_name}")
    
    # Size limits
    if len(str(test_case)) > 10000:
        raise ValueError("Test case too large")
```

## Testing Executors

### Unit Test Template

```python
class TestLanguageExecutor:
    """Template for executor unit tests."""
    
    def test_type_system(self):
        """Test type validation/inference."""
        pass
    
    def test_value_formatting(self):
        """Test language-specific value formatting."""
        pass
    
    def test_code_preparation(self):
        """Test harness generation."""
        pass
    
    def test_execution(self):
        """Test code execution."""
        pass
    
    def test_error_handling(self):
        """Test various error conditions."""
        pass
    
    def test_cleanup(self):
        """Test resource cleanup."""
        pass
```

### Integration Test Template

```python
def test_executor_integration():
    """Test executor with real code generation."""
    # Generate code
    code = generate_sample_code()
    
    # Create test cases
    test_cases = create_test_cases()
    
    # Execute tests
    executor = LanguageExecutor()
    results = []
    
    for test_case in test_cases:
        result = executor.execute_test(code, test_case)
        results.append(result)
    
    # Verify results
    assert all(r['passed'] for r in results)
```

## Debugging Executors

### Enable Debug Logging

```python
import logging

# Configure executor logging
logging.basicConfig(
    level=logging.DEBUG,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)

class DebuggableExecutor(LanguageExecutor):
    def __init__(self):
        self.logger = logging.getLogger(self.__class__.__name__)
    
    def execute_test(self, code: str, test_case: dict, timeout: int = 5) -> dict:
        self.logger.debug(f"Executing test: {test_case}")
        self.logger.debug(f"Code:\n{code}")
        
        result = super().execute_test(code, test_case, timeout)
        
        self.logger.debug(f"Result: {result}")
        return result
```

### Common Issues and Solutions

| Issue | Symptoms | Solution |
|-------|----------|----------|
| Type mismatch | "Invalid type" errors | Verify type mapping implementation |
| Compilation fails | Syntax errors in harness | Check language-specific formatting |
| Output parsing | "Invalid output format" | Verify JSON/output formatting |
| Timeout | Tests fail with timeout | Increase timeout or optimize code |
| Resource cleanup | Temp files accumulate | Ensure cleanup() is called |

## Next Steps

- Review [Architecture](architecture.html) for system overview
- See [Adding Languages](adding-languages.html) for implementation guide
- Explore existing executors in the source code
