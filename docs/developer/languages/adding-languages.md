---
layout: default
title: Adding Languages
parent: Language System
grand_parent: Developer Documentation
nav_order: 2
permalink: /developer/languages/adding-languages
---

# Adding Language Support

This guide walks you through adding support for a new programming language to EiplGrader.

## Overview

Adding a new language requires implementing two components:
1. **Language Adapter** - Handles code generation specifics
2. **Language Executor** - Handles code execution and testing

Both components must be registered with the language registry.

## Step 1: Choose Base Classes

### For Interpreted Languages
Use these if your language:
- Has an interpreter (Python, Ruby, JavaScript)
- Supports runtime type inspection
- Can parse JSON natively

```python
from eiplgrader.languages.base import LanguageAdapter
from eiplgrader.languages.executors.base_executors import InterpretedLanguageExecutor
```

### For Compiled Languages
Use these if your language:
- Requires compilation (C, C++, Java, Go)
- Has static typing
- Needs explicit type declarations

```python
from eiplgrader.languages.base import LanguageAdapter
from eiplgrader.languages.executors.base_executors import CompiledLanguageExecutor
```

## Step 2: Create Language Adapter

### Basic Structure

```python
# eiplgrader/languages/adapters/newlang_adapter.py
from eiplgrader.languages.base import LanguageAdapter, LanguageConfig

class NewLangAdapter(LanguageAdapter):
    def get_config(self) -> LanguageConfig:
        return LanguageConfig(
            name="newlang",
            display_name="NewLang",
            file_extension=".nl",
            compile_command="newlangc {source} -o {output}",  # If compiled
            run_command="newlang {source}",  # If interpreted
            docker_image="newlang:latest",
            supports_types=["int", "float", "string", "bool", "list", "dict"]
        )
```

### Implement Prompt Generation

```python
    def generate_prompt(self, student_response: str, function_name: str,
                       gen_type: str, num_to_gen: int, **kwargs) -> str:
        if gen_type == "cgbg":
            return self._generate_cgbg_prompt(
                student_response, function_name, num_to_gen, **kwargs
            )
        elif gen_type == "redef":
            return self._generate_redef_prompt(
                student_response, function_name, num_to_gen, **kwargs
            )
        else:
            raise ValueError(f"Unknown generation type: {gen_type}")
    
    def _generate_cgbg_prompt(self, description: str, function_name: str,
                             num_to_gen: int, **kwargs) -> str:
        base_prompt = f"""Generate {num_to_gen} NewLang implementations for a function that:
{description}

Requirements:
- Function name: {function_name}
- Each implementation should be different but correct
- Use idiomatic NewLang patterns
- Include proper error handling
"""
        
        # Add examples if provided
        if "example_inputs" in kwargs and "example_outputs" in kwargs:
            base_prompt += self._format_examples(
                kwargs["example_inputs"], 
                kwargs["example_outputs"]
            )
        
        base_prompt += f"""
Generate exactly {num_to_gen} implementations in this format:
Implementation 1:
```newlang
// code here
```

Implementation 2:
```newlang
// code here
```
"""
        return base_prompt
```

### Implement Code Extraction

```python
    def extract_code(self, llm_response: str) -> List[str]:
        """Extract code blocks from LLM response."""
        import re
        
        # Pattern for markdown code blocks
        pattern = r'```(?:newlang)?\s*\n(.*?)```'
        matches = re.findall(pattern, llm_response, re.DOTALL)
        
        if not matches:
            # Fallback: try to find function definitions
            func_pattern = r'func\s+\w+\s*\([^)]*\).*?(?=func\s+\w+|$)'
            matches = re.findall(func_pattern, llm_response, re.DOTALL)
        
        return [self.normalize_code(code) for code in matches]
```

### Implement Code Normalization

```python
    def normalize_code(self, code: str) -> str:
        """Remove comments and normalize formatting."""
        lines = []
        for line in code.split('\n'):
            # Remove single-line comments
            if '//' in line:
                line = line[:line.index('//')]
            # Skip empty lines
            line = line.rstrip()
            if line:
                lines.append(line)
        
        return '\n'.join(lines)
```

## Step 3: Create Language Executor

### For Interpreted Languages

```python
# eiplgrader/languages/executors/newlang_executor.py
from eiplgrader.languages.executors.base_executors import InterpretedLanguageExecutor
import subprocess
import json

class NewLangExecutor(InterpretedLanguageExecutor):
    def prepare_code(self, code: str, test_case: dict) -> str:
        """Prepare code with test harness."""
        # Validate or infer types
        params, expected = self.validate_or_infer_types(test_case)
        
        # Create test harness
        harness = f"""
import json

<code>

# Test harness
test_input = json.parse('{json.dumps(params)}')
result = {test_case.get('function_name', 'solution')}(**test_input)
print(json.stringify({% raw %}{{"result": result}}{% endraw %}))
"""
        return harness
    
    def execute_test(self, code: str, test_case: dict, timeout: int = 5) -> dict:
        """Execute the test and return results."""
        try:
            # Prepare code with harness
            full_code = self.prepare_code(code, test_case)
            
            # Write to temporary file
            with tempfile.NamedTemporaryFile(mode='w', suffix='.nl', delete=False) as f:
                f.write(full_code)
                temp_file = f.name
            
            # Execute
            result = subprocess.run(
                ['newlang', temp_file],
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
            
            # Parse output
            output = json.loads(result.stdout)
            actual = output['result']
            expected = test_case['expected']
            
            return {
                'passed': actual == expected,
                'expected': expected,
                'actual': actual,
                'output': result.stdout
            }
            
        except subprocess.TimeoutExpired:
            return {
                'passed': False,
                'error': 'Execution timeout',
                'error_type': 'timeout'
            }
        except Exception as e:
            return {
                'passed': False,
                'error': str(e),
                'error_type': 'system'
            }
        finally:
            self.cleanup()
```

### For Compiled Languages

```python
class NewLangExecutor(CompiledLanguageExecutor):
    def get_type_mapping(self) -> dict:
        """Map generic types to NewLang types."""
        return {
            "integer": "int",
            "float": "float64",
            "string": "string",
            "boolean": "bool",
            "list": "[]",
            "dict": "map"
        }
    
    def format_value(self, value: Any, type_str: str) -> str:
        """Format Python value as NewLang literal."""
        if isinstance(value, bool):
            return "true" if value else "false"
        elif isinstance(value, str):
            return f'"{value}"'
        elif isinstance(value, list):
            # Handle list formatting based on type
            elem_type = type_str[2:] if type_str.startswith("[]") else "interface{}"
            elements = [self.format_value(v, elem_type) for v in value]
            return f'{type_str}{% raw %}{{{", ".join(elements)}}}{% endraw %}'
        else:
            return str(value)
    
    def prepare_code(self, code: str, test_case: dict) -> str:
        """Prepare code with embedded test values."""
        # Validate types are provided
        self.validate_types_provided(test_case)
        
        # Build parameter assignments
        param_setup = []
        for name, value in test_case["parameters"].items():
            param_type = test_case["parameter_types"][name]
            formatted = self.format_value(value, param_type)
            param_setup.append(f"    {name} := {formatted}")
        
        # Build function call
        func_name = test_case.get("function_name", "solution")
        param_names = ", ".join(test_case["parameters"].keys())
        
        harness = f"""
        <Define your test harness here>
"""
        return harness
```

## Step 4: Register the Language

```python
# eiplgrader/languages/registry.py
from eiplgrader.languages.registry import LanguageRegistry
from .adapters.newlang_adapter import NewLangAdapter
from .executors.newlang_executor import NewLangExecutor

# Register during module initialization
LanguageRegistry.register_language(
    "newlang",
    NewLangAdapter,
    NewLangExecutor
)
```

## Step 5: Add Tests

### Unit Tests for Adapter

```python
# tests/unit/test_adapters/test_newlang_adapter.py
import pytest
from eiplgrader.languages.adapters.newlang_adapter import NewLangAdapter

class TestNewLangAdapter:
    def setup_method(self):
        self.adapter = NewLangAdapter()
    
    def test_config(self):
        config = self.adapter.get_config()
        assert config.name == "newlang"
        assert config.file_extension == ".nl"
    
    def test_cgbg_prompt_generation(self):
        prompt = self.adapter.generate_prompt(
            "calculates the sum of two numbers",
            "add",
            "cgbg",
            1
        )
        assert "calculates the sum of two numbers" in prompt
        assert "add" in prompt
    
    def test_code_extraction(self):
        llm_response = """
Implementation 1:
```newlang
func add(a int, b int) int {
    return a + b
}
```
"""
        codes = self.adapter.extract_code(llm_response)
        assert len(codes) == 1
        assert "func add" in codes[0]
```

### Unit Tests for Executor

```python
# tests/unit/test_executors/test_newlang_executor.py
class TestNewLangExecutor:
    def setup_method(self):
        self.executor = NewLangExecutor()
    
    def test_type_validation(self):
        # For compiled languages
        test_case = {
            "parameters": {"x": 5},
            "expected": 10
        }
        with pytest.raises(ValueError, match="parameter_types"):
            self.executor.validate_types_provided(test_case)
    
    def test_value_formatting(self):
        # For compiled languages
        assert self.executor.format_value(True, "bool") == "true"
        assert self.executor.format_value("hello", "string") == '"hello"'
        assert self.executor.format_value([1, 2, 3], "[]int") == "[]int{1, 2, 3}"
```

### Integration Tests

```python
# tests/integration/test_newlang_integration.py
def test_newlang_end_to_end():
    # Generate code
    generator = CodeGenerator(api_key, language="newlang")
    result = generator.generate_code(
        "returns the factorial of a number",
        "factorial",
        num_to_gen=1
    )
    
    # Test generated code
    test_cases = [
        {"parameters": {"n": 5}, "expected": 120},
        {"parameters": {"n": 0}, "expected": 1}
    ]
    
    tester = CodeTester(
        code=result.codes[0],
        test_cases=test_cases,
        function_name="factorial",
        language="newlang"
    )
    
    results = tester.run_tests()
    assert results.was_successful()
```

## Step 6: Update Documentation

### Add to Language Feature Matrix

Update `docs/guide/languages.md` to include your language:

```markdown
| NewLang | ✅ | ❌ | Required | `.nl` | 1.0+ |
```

### Create Quickstart Guide

Create `docs/quickstart/newlang.md`:

```markdown
---
layout: default
title: NewLang Quickstart
parent: Quickstart Guides
nav_order: 7
---

# NewLang Quickstart

Get started with EiplGrader for NewLang...
```

## Best Practices

### Error Handling
- Provide clear error messages
- Distinguish between compilation and runtime errors
- Handle timeouts gracefully

### Performance
- Cache compiled binaries when possible
- Reuse executor instances
- Clean up temporary files

### Security
- Always use subprocess with timeout
- Validate all inputs
- Run in isolated environment

### Testing
- Test with various code patterns
- Include edge cases
- Test error conditions

## Common Pitfalls

### Type System Issues
- Ensure type mappings are complete
- Handle nested types correctly
- Test with complex data structures

### Code Extraction
- Handle various LLM response formats
- Account for markdown variations
- Validate extracted code

### Execution Environment
- Ensure language runtime is available
- Handle version differences
- Test in Docker environment

## Debugging Tips

### Enable Verbose Logging
```python
import logging
logging.basicConfig(level=logging.DEBUG)
```

### Test Components Separately
```python
# Test adapter
adapter = NewLangAdapter()
prompt = adapter.generate_prompt(...)
print(prompt)

# Test executor
executor = NewLangExecutor()
result = executor.execute_test(...)
print(result)
```

### Use Test Utilities
```python
from tests.utils import create_test_case
test = create_test_case("newlang", {"x": 5}, 10)
```

## Next Steps

After implementing your language:

1. Run the test suite: `python -m pytest tests/ -k newlang`
2. Test with real examples
3. Update the README
4. Submit a pull request

For more details on the executor implementation, see [Executors](executors.html).
