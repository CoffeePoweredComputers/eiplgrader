---
layout: default
title: Testing
parent: Developer Documentation
nav_order: 5
permalink: /developer/testing
---

# Testing Documentation

Comprehensive guide to testing EiplGrader components and features.

## Overview

EiplGrader uses a multi-layered testing approach:
- **Unit Tests**: Test individual components in isolation
- **Integration Tests**: Test component interactions
- **Language Tests**: Test language-specific features
- **End-to-End Tests**: Test complete workflows
- **Performance Tests**: Test execution speed and resource usage

## Test Structure

```
tests/
├── unit/                    # Unit tests
│   ├── test_adapters/      # Language adapter tests
│   ├── test_executors/     # Language executor tests
│   ├── test_codegen.py     # CodeGenerator tests
│   └── test_tester.py      # CodeTester tests
├── integration/            # Integration tests
│   ├── test_generation/    # Code generation tests
│   ├── test_execution/     # Code execution tests
│   └── test_languages/     # Language-specific tests
├── edge_cases/            # Edge case testing
│   ├── test_error_scenarios/
│   └── test_limits/
├── performance/           # Performance benchmarks
└── conftest.py           # Pytest configuration
```

## Running Tests

### Basic Commands

```bash
# Run all tests
python -m pytest

# Run with coverage
python -m pytest --cov=eiplgrader tests/

# Run specific test file
python -m pytest tests/unit/test_codegen.py

# Run specific test
python -m pytest tests/unit/test_codegen.py::test_generate_code

# Run tests for specific language
python -m pytest -k "python"

# Run with verbose output
python -m pytest -v

# Run in parallel
python -m pytest -n auto
```

### Test Categories

```bash
# Unit tests only
python -m pytest tests/unit/

# Integration tests only  
python -m pytest tests/integration/

# Edge cases
python -m pytest tests/edge_cases/

# Performance tests
python -m pytest tests/performance/ --benchmark-only
```

## Writing Tests

### Unit Test Template

```python
import pytest
from unittest.mock import Mock, patch
from eiplgrader.codegen import CodeGenerator

class TestCodeGenerator:
    """Unit tests for CodeGenerator."""
    
    def setup_method(self):
        """Set up test fixtures."""
        self.api_key = "test-key"
        self.generator = CodeGenerator(self.api_key)
    
    def test_initialization(self):
        """Test proper initialization."""
        assert self.generator.api_key == self.api_key
        assert self.generator.language == "python"
        assert self.generator.temperature == 0.7
    
    @patch('eiplgrader.codegen.OpenAIRequest')
    def test_generate_code(self, mock_openai):
        """Test code generation."""
        # Mock LLM response
        mock_openai.return_value.request_function_generation.return_value = {
            "choices": [{
                "message": {
                    "content": "def factorial(n):\n    return 1"
                }
            }]
        }
        
        # Generate code
        result = self.generator.generate_code(
            student_response="calculate factorial",
            function_name="factorial"
        )
        
        # Assertions
        assert len(result.codes) == 1
        assert "factorial" in result.codes[0]
        mock_openai.return_value.request_function_generation.assert_called_once()
    
    def test_invalid_language(self):
        """Test error handling for invalid language."""
        with pytest.raises(LanguageNotSupportedError):
            self.generator.set_language("invalid_lang")
    
    @pytest.mark.parametrize("gen_type,expected", [
        ("cgbg", True),
        ("redef", True),
        ("invalid", False)
    ])
    def test_generation_types(self, gen_type, expected):
        """Test different generation types."""
        if expected:
            # Should not raise
            self.generator._validate_gen_type(gen_type)
        else:
            with pytest.raises(ValueError):
                self.generator._validate_gen_type(gen_type)
```

### Integration Test Template

```python
class TestCodeGenerationIntegration:
    """Integration tests for code generation and testing."""
    
    @pytest.fixture
    def generator(self):
        """Create generator fixture."""
        return CodeGenerator(os.getenv("TEST_API_KEY", "dummy"))
    
    @pytest.fixture
    def test_cases(self):
        """Create test case fixture."""
        return [
            {"parameters": {"n": 5}, "expected": 120},
            {"parameters": {"n": 0}, "expected": 1}
        ]
    
    def test_generate_and_test_python(self, generator, test_cases):
        """Test Python code generation and execution."""
        # Generate code
        result = generator.generate_code(
            student_response="calculate factorial recursively",
            function_name="factorial",
            language="python"
        )
        
        assert result.success
        assert len(result.codes) > 0
        
        # Test generated code
        for code in result.codes:
            tester = CodeTester(
                code=code,
                test_cases=test_cases,
                function_name="factorial",
                language="python"
            )
            
            results = tester.run_tests()
            assert results.all_passed, f"Tests failed for code:\n{code}"
```

### Language-Specific Tests

```python
class TestJavaLanguageSupport:
    """Test Java language support."""
    
    def test_java_type_requirements(self):
        """Test that Java requires explicit types."""
        executor = JavaExecutor()
        
        # Missing types should raise error
        test_case = {
            "parameters": {"x": 5},
            "expected": 10
        }
        
        with pytest.raises(ValueError, match="parameter_types"):
            executor.validate_types_provided(test_case)
    
    def test_java_value_formatting(self):
        """Test Java value formatting."""
        executor = JavaExecutor()
        
        # Test various type conversions
        assert executor.format_value(True, "boolean") == "true"
        assert executor.format_value("hello", "String") == '"hello"'
        assert executor.format_value([1, 2, 3], "int[]") == "new int[]{1, 2, 3}"
    
    def test_java_compilation(self):
        """Test Java code compilation."""
        code = """
        public class Solution {
            public int add(int a, int b) {
                return a + b;
            }
        }
        """
        
        test_case = {
            "parameters": {"a": 5, "b": 3},
            "parameter_types": {"a": "int", "b": "int"},
            "expected": 8,
            "expected_type": "int"
        }
        
        executor = JavaExecutor()
        result = executor.execute_test(code, test_case)
        
        assert result["passed"]
        assert result["actual"] == 8
```

### Edge Case Tests

```python
class TestEdgeCases:
    """Test edge cases and error conditions."""
    
    def test_empty_code(self):
        """Test handling of empty code."""
        tester = CodeTester(
            code="",
            test_cases=[{"parameters": {"x": 1}, "expected": 1}],
            function_name="test"
        )
        
        results = tester.run_tests()
        assert not results.all_passed
        assert results.failures[0].error_type == "runtime"
    
    def test_infinite_loop(self):
        """Test timeout handling."""
        code = """
def infinite():
    while True:
        pass
"""
        
        tester = CodeTester(
            code=code,
            test_cases=[{"parameters": {}, "expected": None}],
            function_name="infinite",
            timeout=1
        )
        
        results = tester.run_tests()
        assert not results.all_passed
        assert results.failures[0].error_type == "timeout"
    
    def test_large_output(self):
        """Test output size limits."""
        code = """
def large_output():
    return "x" * 10000000  # 10MB string
"""
        
        tester = CodeTester(
            code=code,
            test_cases=[{"parameters": {}, "expected": "x"}],
            function_name="large_output",
            max_output_size=1024 * 1024  # 1MB limit
        )
        
        results = tester.run_tests()
        assert not results.all_passed
        assert "output size" in results.failures[0].error.lower()
```

## Test Fixtures

### Common Fixtures

```python
# conftest.py
import pytest
import tempfile
import shutil

@pytest.fixture
def temp_dir():
    """Create temporary directory for tests."""
    temp = tempfile.mkdtemp()
    yield temp
    shutil.rmtree(temp, ignore_errors=True)

@pytest.fixture
def sample_code():
    """Provide sample code for testing."""
    return {
        "python": """
def factorial(n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)
""",
        "java": """
public class Solution {
    public int factorial(int n) {
        if (n <= 1) return 1;
        return n * factorial(n - 1);
    }
}
""",
        "javascript": """
function factorial(n) {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}
"""
    }

@pytest.fixture
def api_key():
    """Get API key for testing."""
    key = os.getenv("OPENAI_API_KEY")
    if not key:
        pytest.skip("API key not available")
    return key
```

### Language-Specific Fixtures

```python
@pytest.fixture(params=["python", "javascript"])
def dynamic_language(request):
    """Parametrized fixture for dynamic languages."""
    return request.param

@pytest.fixture(params=["java", "cpp", "go"])
def static_language(request):
    """Parametrized fixture for static languages."""
    return request.param

@pytest.fixture
def language_test_cases():
    """Test cases for each language."""
    return {
        "python": {
            "simple": {
                "parameters": {"x": 5},
                "expected": 10
            }
        },
        "java": {
            "simple": {
                "parameters": {"x": 5},
                "parameter_types": {"x": "int"},
                "expected": 10,
                "expected_type": "int"
            }
        }
    }
```

## Test Utilities

### Test Case Builders

```python
def create_test_case(language: str, params: dict, expected: Any) -> dict:
    """Create test case with appropriate format for language."""
    test_case = {
        "parameters": params,
        "expected": expected
    }
    
    # Add types for static languages
    if language in ["java", "cpp", "c", "go", "haskell"]:
        test_case["parameter_types"] = infer_types(params, language)
        test_case["expected_type"] = infer_type(expected, language)
    
    return test_case

def infer_types(params: dict, language: str) -> dict:
    """Infer parameter types for a language."""
    type_map = get_type_mapping(language)
    return {
        name: type_map[type(value).__name__]
        for name, value in params.items()
    }
```

### Mock Helpers

```python
class MockLLMResponse:
    """Mock LLM response for testing."""
    
    def __init__(self, codes: List[str]):
        self.codes = codes
        self.call_count = 0
    
    def __call__(self, *args, **kwargs):
        """Return next code in sequence."""
        if self.call_count < len(self.codes):
            code = self.codes[self.call_count]
            self.call_count += 1
            return {"choices": [{"message": {"content": code}}]}
        raise ValueError("No more mock responses")

def mock_llm_response(monkeypatch, codes: List[str]):
    """Patch LLM calls with mock responses."""
    mock = MockLLMResponse(codes)
    monkeypatch.setattr(
        "eiplgrader.codegen.OpenAIRequest.request_function_generation",
        mock
    )
    return mock
```

## Performance Testing

### Benchmark Tests

```python
import pytest

@pytest.mark.benchmark
def test_code_generation_performance(benchmark):
    """Benchmark code generation."""
    generator = CodeGenerator("key")
    
    result = benchmark(
        generator.generate_code,
        student_response="calculate factorial",
        function_name="factorial",
        num_to_gen=1
    )
    
    assert result.success

@pytest.mark.benchmark
def test_execution_performance(benchmark):
    """Benchmark code execution."""
    code = "def add(a, b): return a + b"
    test_case = {"parameters": {"a": 1, "b": 2}, "expected": 3}
    
    tester = CodeTester(
        code=code,
        test_cases=[test_case],
        function_name="add"
    )
    
    result = benchmark(tester.run_single_test, test_case)
    assert result.passed
```

### Load Testing

```python
def test_parallel_execution_load():
    """Test system under load."""
    import concurrent.futures
    
    # Create many test cases
    test_cases = [
        {"parameters": {"n": i}, "expected": factorial(i)}
        for i in range(100)
    ]
    
    tester = CodeTester(
        code="def factorial(n): ...",  # Implementation
        test_cases=test_cases,
        function_name="factorial"
    )
    
    # Run with different worker counts
    for workers in [1, 4, 8, 16]:
        start = time.time()
        results = tester.run_tests(parallel=True, max_workers=workers)
        duration = time.time() - start
        
        print(f"Workers: {workers}, Time: {duration:.2f}s")
        assert results.all_passed
```

## Test Coverage

### Coverage Configuration

```ini
# .coveragerc
[run]
source = eiplgrader
omit = 
    */tests/*
    */test_*
    */__pycache__/*
    */venv/*

[report]
exclude_lines =
    pragma: no cover
    def __repr__
    raise AssertionError
    raise NotImplementedError
    if __name__ == .__main__.:
    @abstractmethod
```

### Coverage Commands

```bash
# Run with coverage
python -m pytest --cov=eiplgrader --cov-report=html

# View coverage report
open htmlcov/index.html

# Check coverage threshold
python -m pytest --cov=eiplgrader --cov-fail-under=80
```

## Continuous Integration

### GitHub Actions Example

```yaml
# .github/workflows/tests.yml
name: Tests

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        python-version: [3.8, 3.9, "3.10", "3.11"]
        
    steps:
    - uses: actions/checkout@v3
    
    - name: Set up Python
      uses: actions/setup-python@v4
      with:
        python-version: ${{ matrix.python-version }}
    
    - name: Install dependencies
      run: |
        pip install -e ".[dev]"
        
    - name: Run linting
      run: |
        black --check eiplgrader tests
        pylint eiplgrader
        mypy eiplgrader
    
    - name: Run tests
      run: |
        python -m pytest --cov=eiplgrader --cov-report=xml
    
    - name: Upload coverage
      uses: codecov/codecov-action@v3
```

## Test Best Practices

### 1. Test Organization
- One test class per component
- Group related tests together
- Use descriptive test names
- Keep tests focused and simple

### 2. Test Independence
- Each test should be independent
- Use fixtures for setup/teardown
- Don't rely on test execution order
- Clean up resources after tests

### 3. Mock External Dependencies
```python
@patch('requests.post')
def test_api_call(mock_post):
    mock_post.return_value.json.return_value = {"result": "success"}
    # Test code that uses requests.post
```

### 4. Parametrized Tests
```python
@pytest.mark.parametrize("input,expected", [
    (0, 1),
    (1, 1),
    (5, 120),
    (10, 3628800)
])
def test_factorial_values(input, expected):
    assert factorial(input) == expected
```

### 5. Error Testing
```python
def test_error_handling():
    with pytest.raises(ValueError, match="negative"):
        factorial(-1)
```

## Debugging Tests

### Pytest Options

```bash
# Show print statements
python -m pytest -s

# Drop into debugger on failure
python -m pytest --pdb

# Show local variables on failure
python -m pytest -l

# Run last failed tests
python -m pytest --lf

# Run specific test by name pattern
python -m pytest -k "test_generate"
```

### Debugging Techniques

```python
def test_with_debugging():
    """Test with debugging helpers."""
    # Add breakpoint
    import pdb; pdb.set_trace()
    
    # Print debug info
    print(f"Debug: {variable}")
    
    # Use pytest's capsys
    def test_output(capsys):
        print("Hello")
        captured = capsys.readouterr()
        assert captured.out == "Hello\n"
```

## Next Steps

- Review [Contributing](contributing.html) guide for development workflow
- Check existing tests in the repository
- Run the test suite locally
- Add tests for new features
