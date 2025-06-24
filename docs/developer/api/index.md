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
from eiplgrader.languages import LanguageRegistry

# Get available languages
languages = LanguageRegistry.get_supported_languages()

# Get language components
adapter = LanguageRegistry.get_adapter("python")
executor = LanguageRegistry.get_executor("python")

# Register new language
LanguageRegistry.register_language(
    "newlang",
    adapter_class=NewLangAdapter,
    executor_class=NewLangExecutor
)
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

```bash
# API Keys
OPENAI_API_KEY=sk-...
ANTHROPIC_API_KEY=sk-ant-...

# Model Selection
EIPLGRADER_CLIENT_TYPE=openai  # or ollama (anthropic and meta planned)

# Execution Settings
EIPLGRADER_TIMEOUT=10
EIPLGRADER_MAX_PARALLEL=5

# Language Settings
EIPLGRADER_DEFAULT_LANGUAGE=python
```

### Configuration File

```yaml
# .eiplgrader.yml
models:
  openai:
    api_key: ${OPENAI_API_KEY}
    default_model: gpt-4
    temperature: 0.7
    
execution:
  timeout: 10
  max_parallel: 5
  temp_dir: /tmp/eiplgrader
  
languages:
  python:
    version: "3.9"
    docker_image: "python:3.9-slim"
    
logging:
  level: INFO
  format: "%(asctime)s - %(name)s - %(levelname)s - %(message)s"
```

## Rate Limiting

### Built-in Rate Limiter

```python
from eiplgrader.utils import RateLimiter

# Configure rate limits
rate_limiter = RateLimiter(
    max_requests_per_minute=60,
    max_tokens_per_minute=90000
)

# Use with generator
generator = CodeGenerator(
    api_key=api_key,
    rate_limiter=rate_limiter
)
```

### Custom Rate Limiting

```python
class CustomRateLimiter:
    def check_request(self, tokens: int) -> bool:
        """Return True if request should proceed."""
        pass
    
    def record_usage(self, tokens: int):
        """Record token usage."""
        pass
```

## Async Support

### Async Code Generation

```python
import asyncio
from eiplgrader import AsyncCodeGenerator

async def generate_async():
    generator = AsyncCodeGenerator(api_key)
    
    # Generate multiple variants concurrently
    tasks = []
    for i in range(5):
        task = generator.generate_code_async(
            student_response=f"variant {i}",
            function_name="solution"
        )
        tasks.append(task)
    
    results = await asyncio.gather(*tasks)
    return results
```

### Async Test Execution

```python
from eiplgrader import AsyncCodeTester

async def test_async():
    tester = AsyncCodeTester(code, test_cases)
    
    # Run tests concurrently
    results = await tester.run_tests_async(
        max_concurrent=10
    )
    
    return results
```

## Utilities

### Test Case Builders

```python
from eiplgrader.utils import TestCaseBuilder

# Build test case with type inference
test = TestCaseBuilder() \
    .with_parameters(x=5, y=10) \
    .expects(15) \
    .build()

# Build with explicit types
test = TestCaseBuilder() \
    .with_parameters(x=5, y=10) \
    .with_parameter_types(x="int", y="int") \
    .expects(15) \
    .with_expected_type("int") \
    .build()
```

### Code Validators

```python
from eiplgrader.utils import CodeValidator

# Validate Python code
validator = CodeValidator("python")
is_valid, errors = validator.validate(code)

if not is_valid:
    for error in errors:
        print(f"Line {error.line}: {error.message}")
```

## Next Steps

- Explore detailed [CodeGenerator API](codegen-api.html)
- Review [CodeTester API](tester-api.html)
- Understand [Language System API](language-api.html)
- See [Examples](examples.html) for common patterns