---
layout: default
title: API Reference
parent: Developer Documentation
nav_order: 4
has_children: true
permalink: /developer/api/
---

# API Reference

Complete API documentation for EiplGrader's public interfaces.

## Core APIs

### 📝 [CodeGenerator API](codegen-api.html)
Complete reference for the code generation interface.
- Class methods and parameters
- Configuration options
- Return value formats
- Error handling

### 🧪 [CodeTester API](tester-api.html)
Full documentation of the testing interface.
- Test execution methods
- Result structures
- Error types
- Performance options

### 🌐 [Language System API](language-api.html)
Reference for language adapters and executors.
- Base class interfaces
- Registry methods
- Type system APIs
- Extension points

## Quick Reference

### CodeGenerator

```python
from eiplgrader.codegen import CodeGenerator

# Initialize
generator = CodeGenerator(
    api_key="your-api-key",
    client_type="openai",
    language="python"
)

# Generate code
result = generator.generate_code(
    student_response="calculates factorial",
    function_name="factorial",
    gen_type="cgbg",
    num_to_gen=3,
    temperature=0.7
)

# Access results
for i, code in enumerate(result["code"]):
    print(f"Implementation {i+1}:\n{code}")
```

### CodeTester

```python
from eiplgrader.tester import CodeTester

# Initialize
tester = CodeTester(
    code=generated_code,
    test_cases=test_cases,
    function_name="factorial",
    language="python"
)

# Run tests
results = tester.run_tests()

# Check results
if results.was_successful():
    print("All tests passed!")
else:
    for result in results.test_results:
        if not result["pass"]:
            print(f"Failed: {result['error']}")
```

### Language Registry

```python
from eiplgrader.languages.registry import LanguageRegistry

# Create registry instance
registry = LanguageRegistry()

# Get available languages
languages = registry.list_languages()

# Get language components
adapter = registry.get_adapter("python")
executor_class = registry.get_executor("python")

# Register new language
registry.register("newlang", NewLangAdapter)
```

## Response Formats

### Generation Response

```python
class GenerationResult:
    codes: List[str]          # Generated code implementations
    raw_response: str         # Raw LLM response
    prompt_used: str         # Actual prompt sent to LLM
    metadata: dict           # Additional metadata
    
    # Segmentation results (if requested)
    segments: Optional[List[CodeSegment]]
```

### Test Results

```python
class TestResults:
    all_passed: bool         # Whether all tests passed
    passed_count: int        # Number of passed tests
    failed_count: int        # Number of failed tests
    results: List[TestResult] # Individual test results
    
class TestResult:
    passed: bool             # Test pass/fail status
    test_case: dict         # Original test case
    expected: Any           # Expected value
    actual: Any             # Actual value
    error: Optional[str]    # Error message if failed
    error_type: Optional[str] # Error category
    execution_time: float   # Time taken in seconds
```

## Error Handling

### Exception Hierarchy

```python
class EiplGraderError(Exception):
    """Base exception for all EiplGrader errors."""
    
class GenerationError(EiplGraderError):
    """Errors during code generation."""
    
class ExecutionError(EiplGraderError):
    """Errors during code execution."""
    
class LanguageNotSupportedError(EiplGraderError):
    """Requested language is not supported."""
    
class InvalidTestCaseError(EiplGraderError):
    """Test case format is invalid."""
```

### Error Response Format

```python
{
    "success": False,
    "error": {
        "type": "GenerationError",
        "message": "Failed to generate code",
        "details": {
            "model": "openai",
            "language": "python",
            "prompt_tokens": 150
        }
    }
}
```

## Configuration

### Environment Variables

Currently, environment variables are only used in the example scripts for API keys (e.g., `OPENAI_API_KEY`). The library itself does not use environment variables for configuration.

## Next Steps

- Explore detailed [CodeGenerator API](codegen-api.html)
- Review [CodeTester API](tester-api.html)
- Understand [Language System API](language-api.html)
- See [Examples](examples.html) for common patterns