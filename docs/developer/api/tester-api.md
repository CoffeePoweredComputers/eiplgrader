---
layout: default
title: CodeTester API
parent: API Reference
grand_parent: Developer Documentation
nav_order: 2
permalink: /developer/api/tester-api
---

# CodeTester API Reference

Complete API documentation for the CodeTester class.

## Class: CodeTester

```python
from eiplgrader.tester import CodeTester
```

### Constructor

```python
def __init__(
    self,
    code: str,
    test_cases: List[Dict[str, Any]],
    function_name: str,
    language: str = "python",
    timeout: int = 5,
    executor: Optional[LanguageExecutor] = None,
    **kwargs
)
```

#### Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `code` | `str` | required | Code to test |
| `test_cases` | `List[Dict]` | required | List of test case dictionaries |
| `function_name` | `str` | required | Name of function to test |
| `language` | `str` | `"python"` | Programming language |
| `timeout` | `int` | `5` | Execution timeout in seconds |
| `executor` | `LanguageExecutor` | `None` | Optional custom executor |
| `**kwargs` | `dict` | `{}` | Additional executor options |

#### Example

```python
# Basic initialization
tester = CodeTester(
    code=generated_code,
    test_cases=[
        {"parameters": {"n": 5}, "expected": 120},
        {"parameters": {"n": 0}, "expected": 1}
    ],
    function_name="factorial",
    language="python"
)

# With custom executor
from eiplgrader.languages.executors import PythonExecutor
executor = PythonExecutor(sandbox_mode=True)

tester = CodeTester(
    code=code,
    test_cases=tests,
    function_name="solve",
    executor=executor
)
```

### Methods

#### run_tests

```python
def run_tests(
    self,
    parallel: bool = False,
    max_workers: int = 4,
    stop_on_failure: bool = False,
    verbose: bool = False
) -> TestResults
```

Execute all test cases against the code.

##### Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `parallel` | `bool` | `False` | Run tests in parallel |
| `max_workers` | `int` | `4` | Maximum parallel workers |
| `stop_on_failure` | `bool` | `False` | Stop after first failure |
| `verbose` | `bool` | `False` | Print detailed output |

##### Returns

`TestResults` object containing:

```python
class TestResults:
    all_passed: bool              # True if all tests passed
    passed_count: int             # Number of passed tests
    failed_count: int             # Number of failed tests
    total_count: int              # Total number of tests
    results: List[TestResult]     # Individual test results
    execution_time: float         # Total execution time
    
    @property
    def pass_rate(self) -> float:
        """Percentage of tests passed."""
        return (self.passed_count / self.total_count) * 100
    
    @property
    def failures(self) -> List[TestResult]:
        """List of failed test results."""
        return [r for r in self.results if not r.passed]
```

##### Example

```python
# Sequential execution
results = tester.run_tests()

# Parallel execution
results = tester.run_tests(parallel=True, max_workers=8)

# Stop on first failure
results = tester.run_tests(stop_on_failure=True)

# Verbose output
results = tester.run_tests(verbose=True)
```

#### run_single_test

```python
def run_single_test(
    self,
    test_case: Dict[str, Any],
    timeout: Optional[int] = None
) -> TestResult
```

Run a single test case.

##### Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `test_case` | `Dict` | required | Single test case dictionary |
| `timeout` | `int` | `None` | Override default timeout |

##### Returns

`TestResult` object:

```python
class TestResult:
    passed: bool                  # Whether test passed
    test_case: Dict[str, Any]     # Original test case
    expected: Any                 # Expected value
    actual: Any                   # Actual value (if available)
    error: Optional[str]          # Error message if failed
    error_type: Optional[str]     # Error category
    execution_time: float         # Execution time in seconds
    stdout: Optional[str]         # Captured stdout
    stderr: Optional[str]         # Captured stderr
```

##### Example

```python
test = {"parameters": {"x": 10}, "expected": 100}
result = tester.run_single_test(test)

if result.passed:
    print(f"Test passed in {result.execution_time:.3f}s")
else:
    print(f"Test failed: {result.error}")
```

#### validate_test_cases

```python
def validate_test_cases(
    self,
    test_cases: Optional[List[Dict]] = None
) -> Tuple[bool, List[str]]
```

Validate test case format and types.

##### Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `test_cases` | `List[Dict]` | `None` | Test cases to validate (uses instance test_cases if None) |

##### Returns

Tuple of (is_valid, error_messages).

##### Example

```python
is_valid, errors = tester.validate_test_cases()

if not is_valid:
    for error in errors:
        print(f"Validation error: {error}")
```

#### get_test_summary

```python
def get_test_summary(
    self,
    results: Optional[TestResults] = None
) -> Dict[str, Any]
```

Get summary statistics for test results.

##### Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `results` | `TestResults` | `None` | Results to summarize (runs tests if None) |

##### Returns

Dictionary containing:

```python
{
    "total_tests": int,
    "passed": int,
    "failed": int,
    "pass_rate": float,
    "average_execution_time": float,
    "error_breakdown": {
        "compilation": int,
        "runtime": int,
        "timeout": int,
        "assertion": int
    }
}
```

##### Example

```python
summary = tester.get_test_summary()
print(f"Pass rate: {summary['pass_rate']:.1f}%")
print(f"Average time: {summary['average_execution_time']:.3f}s")
```

### Properties

#### test_count

```python
@property
def test_count(self) -> int
```

Number of test cases.

#### language_config

```python
@property  
def language_config(self) -> LanguageConfig
```

Language configuration details.

```python
config = tester.language_config
print(f"Language: {config.display_name}")
print(f"Extension: {config.file_extension}")
```

### Test Case Format

#### Basic Format (Dynamic Languages)

```python
test_case = {
    "parameters": {
        "param1": value1,
        "param2": value2
    },
    "expected": expected_value
}
```

#### Extended Format (Static Languages)

```python
test_case = {
    "parameters": {
        "x": 5,
        "y": [1, 2, 3]
    },
    "parameter_types": {
        "x": "int",
        "y": "int[]"  # or List<Integer> for Java
    },
    "expected": 15,
    "expected_type": "int"
}
```

#### In-Place Modification Testing

```python
# Mode 1: Test in-place modification only
test_case = {
    "parameters": {"arr": [3, 1, 4, 1, 5]},
    "expected": [1, 1, 3, 4, 5],  # Expected state after modification
    "inplace": "1"
}

# Mode 2: Test both modification and return value
test_case = {
    "parameters": {"arr": [3, 1, 4]},
    "expected": {
        "state": [1, 3, 4],      # Expected state
        "return": 3              # Expected return value
    },
    "inplace": "2"
}
```

### Error Types

| Error Type | Description | Common Causes |
|------------|-------------|---------------|
| `compilation` | Code failed to compile | Syntax errors, type errors |
| `runtime` | Code crashed during execution | Logic errors, exceptions |
| `timeout` | Execution exceeded time limit | Infinite loops, complexity |
| `assertion` | Output doesn't match expected | Logic errors, edge cases |
| `system` | Infrastructure error | Missing executor, file I/O |

### Advanced Usage

#### Custom Test Runners

```python
class CustomTester(CodeTester):
    def run_tests(self, **kwargs) -> TestResults:
        """Add custom test logic."""
        # Pre-test setup
        self.setup_environment()
        
        # Run tests with monitoring
        results = super().run_tests(**kwargs)
        
        # Post-test analysis
        self.analyze_performance(results)
        
        return results
    
    def setup_environment(self):
        """Custom setup logic."""
        pass
    
    def analyze_performance(self, results: TestResults):
        """Analyze test performance."""
        slow_tests = [
            r for r in results.results 
            if r.execution_time > 1.0
        ]
        if slow_tests:
            print(f"Warning: {len(slow_tests)} slow tests detected")
```

#### Test Case Generators

```python
def generate_test_cases(start: int, end: int) -> List[Dict]:
    """Generate range-based test cases."""
    test_cases = []
    
    for i in range(start, end + 1):
        test_cases.append({
            "parameters": {"n": i},
            "expected": factorial(i),  # Reference implementation
            "description": f"Test factorial({i})"
        })
    
    return test_cases

# Use generated tests
tester = CodeTester(
    code=code,
    test_cases=generate_test_cases(0, 10),
    function_name="factorial"
)
```

#### Parallel Testing with Progress

```python
from concurrent.futures import as_completed
from tqdm import tqdm

def run_tests_with_progress(tester: CodeTester) -> TestResults:
    """Run tests in parallel with progress bar."""
    results = []
    
    with tqdm(total=tester.test_count) as pbar:
        # Submit all tests
        futures = []
        with ThreadPoolExecutor(max_workers=8) as executor:
            for test_case in tester.test_cases:
                future = executor.submit(
                    tester.run_single_test, 
                    test_case
                )
                futures.append(future)
            
            # Collect results
            for future in as_completed(futures):
                result = future.result()
                results.append(result)
                pbar.update(1)
    
    return TestResults(results)
```

#### Test Result Analysis

```python
def analyze_failures(results: TestResults):
    """Analyze test failures for patterns."""
    failures = results.failures
    
    # Group by error type
    error_groups = {}
    for failure in failures:
        error_type = failure.error_type or "unknown"
        if error_type not in error_groups:
            error_groups[error_type] = []
        error_groups[error_type].append(failure)
    
    # Report findings
    for error_type, group in error_groups.items():
        print(f"\n{error_type.upper()} errors ({len(group)}):")
        for failure in group[:3]:  # Show first 3
            print(f"  - Test: {failure.test_case}")
            print(f"    Error: {failure.error}")
```

### Performance Optimization

#### Executor Pooling

```python
from eiplgrader.utils import ExecutorPool

# Create executor pool
pool = ExecutorPool(max_size=10)

# Run multiple tests with pooled executors
for code_variant in code_variants:
    executor = pool.acquire("python")
    try:
        tester = CodeTester(
            code=code_variant,
            test_cases=test_cases,
            function_name="solve",
            executor=executor
        )
        results = tester.run_tests()
    finally:
        pool.release("python", executor)
```

#### Result Caching

```python
class CachedTester(CodeTester):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self._cache = {}
    
    def run_single_test(self, test_case: Dict) -> TestResult:
        # Create cache key
        cache_key = f"{self.code_hash}:{hash(str(test_case))}"
        
        # Check cache
        if cache_key in self._cache:
            return self._cache[cache_key]
        
        # Run test
        result = super().run_single_test(test_case)
        
        # Cache result
        self._cache[cache_key] = result
        return result
    
    @property
    def code_hash(self) -> str:
        import hashlib
        return hashlib.md5(self.code.encode()).hexdigest()
```

### Security Considerations

#### Sandboxed Execution

```python
# Use Docker executor for untrusted code
tester = CodeTester(
    code=untrusted_code,
    test_cases=test_cases,
    function_name="solve",
    language="python",
    use_docker=True,
    docker_image="python:3.9-slim",
    memory_limit="256m",
    cpu_limit="0.5"
)
```

#### Resource Limits

```python
# Set strict resource limits
tester = CodeTester(
    code=code,
    test_cases=test_cases,
    function_name="solve",
    timeout=2,  # 2 second timeout
    max_memory_mb=128,  # 128MB memory limit
    max_output_size=1024 * 1024  # 1MB output limit
)
```

### Debugging

#### Enable Debug Mode

```python
import logging

# Enable debug logging
logging.basicConfig(level=logging.DEBUG)

# Run with verbose output
results = tester.run_tests(verbose=True)

# Access detailed execution info
for result in results.results:
    if not result.passed:
        print(f"Failed test: {result.test_case}")
        print(f"Stdout: {result.stdout}")
        print(f"Stderr: {result.stderr}")
        print(f"Error: {result.error}")
```

#### Test Isolation

```python
def debug_single_test(tester: CodeTester, test_index: int):
    """Debug a specific test case."""
    test_case = tester.test_cases[test_index]
    
    print(f"Running test: {test_case}")
    print(f"Code:\n{tester.code}")
    
    result = tester.run_single_test(test_case)
    
    print(f"Result: {'PASS' if result.passed else 'FAIL'}")
    print(f"Expected: {result.expected}")
    print(f"Actual: {result.actual}")
    print(f"Execution time: {result.execution_time:.3f}s")
    
    return result
```

## See Also

- [CodeGenerator API](codegen-api.html) - For generating code to test
- [Language System API](language-api.html) - For language-specific details
- [Test Case Format](../guide/test-cases.html) - Detailed test case documentation