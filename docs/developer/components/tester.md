---
layout: default
title: CodeTester
parent: Core Components
grand_parent: Developer Documentation
nav_order: 2
---

# CodeTester Component

Deep dive into the CodeTester class implementation and extension.

## Overview

The `CodeTester` class executes generated code against test cases and provides detailed results about test success or failure.

## Class Structure

```python
class CodeTester:
    """Test generated code against predefined test cases."""
    
    def __init__(
        self,
        code: str,
        test_cases: List[Dict[str, Any]],
        function_name: str = "foo",
        language: str = "python",
        timeout: int = 30
    ):
        """
        Initialize the code tester.
        
        Args:
            code: The code to test
            test_cases: List of test case dictionaries
            function_name: Name of the function to test
            language: Programming language
            timeout: Default timeout for tests in seconds
        """
        self.code = code
        self.test_cases = test_cases
        self.function_name = function_name
        self.language = language
        self.timeout = timeout
        self._load_executor()
```

## Key Components

### TestResult Class

```python
@dataclass
class TestResult:
    """Result of a single test case."""
    test_case: Dict[str, Any]
    passed: bool
    actual: Any
    expected: Any
    error: Optional[str] = None
    execution_time: Optional[float] = None
    
    @property
    def function_call(self) -> str:
        """String representation of the function call."""
        params = self.test_case.get("parameters", {})
        args = ", ".join(f"{k}={repr(v)}" for k, v in params.items())
        return f"{self.test_case.get('function_name', 'foo')}({args})"
```

### TestResults Class

```python
@dataclass
class TestResults:
    """Aggregate results of all test cases."""
    results: List[TestResult]
    
    @property
    def testsRun(self) -> int:
        return len(self.results)
    
    @property
    def successes(self) -> int:
        return sum(1 for r in self.results if r.passed)
    
    @property
    def failures(self) -> List[TestResult]:
        return [r for r in self.results if not r.passed]
    
    @property
    def allPassed(self) -> bool:
        return all(r.passed for r in self.results)
    
    @property
    def successRate(self) -> float:
        return self.successes / self.testsRun if self.testsRun > 0 else 0.0
```

## Core Methods

### `run_tests()`

The main method for running all test cases:

```python
def run_tests(self) -> TestResults:
    """
    Run all test cases against the code.
    
    Returns:
        TestResults object with detailed results
    """
    # Validate test cases before execution
    self._validate_test_cases()
    
    results = []
    for test_case in self.test_cases:
        try:
            result = self._run_single_test(test_case)
            results.append(result)
        except Exception as e:
            # Handle unexpected errors
            result = TestResult(
                test_case=test_case,
                passed=False,
                actual=None,
                expected=test_case.get("expected"),
                error=str(e)
            )
            results.append(result)
    
    # Clean up resources
    self.executor.cleanup()
    
    return TestResults(results)
```

### `_run_single_test()`

Execute a single test case:

```python
def _run_single_test(self, test_case: Dict[str, Any]) -> TestResult:
    """Run a single test case."""
    start_time = time.time()
    
    try:
        # Execute the test
        execution_result = self.executor.execute_test(
            code=self.code,
            test_case=test_case
        )
        
        execution_time = time.time() - start_time
        
        # Create test result
        return TestResult(
            test_case=test_case,
            passed=execution_result.get("passed", False),
            actual=execution_result.get("actual"),
            expected=test_case.get("expected"),
            error=execution_result.get("error"),
            execution_time=execution_time
        )
        
    except TimeoutError:
        return TestResult(
            test_case=test_case,
            passed=False,
            actual=None,
            expected=test_case.get("expected"),
            error=f"Test timed out after {test_case.get('timeout', self.timeout)} seconds"
        )
```

## Test Case Validation

### Dynamic Language Validation

```python
def _validate_dynamic_language_test_case(self, test_case: Dict[str, Any]) -> None:
    """Validate test case for dynamic languages (Python, JavaScript)."""
    required_fields = ["parameters", "expected"]
    
    for field in required_fields:
        if field not in test_case:
            raise ValueError(f"Test case missing required field: {field}")
    
    # Validate parameter structure
    if not isinstance(test_case["parameters"], dict):
        raise ValueError("Parameters must be a dictionary")
    
    # Optional fields with defaults
    test_case.setdefault("function_name", self.function_name)
    test_case.setdefault("timeout", self.timeout)
    test_case.setdefault("inplace", "0")
```

### Static Language Validation

```python
def _validate_static_language_test_case(self, test_case: Dict[str, Any]) -> None:
    """Validate test case for static languages (Java, C++, etc)."""
    # All dynamic language requirements plus type information
    self._validate_dynamic_language_test_case(test_case)
    
    # Additional required fields for static languages
    type_fields = ["parameter_types", "expected_type"]
    
    for field in type_fields:
        if field not in test_case:
            raise ValueError(
                f"Test case missing required type field: {field}. "
                f"Static languages require explicit type annotations."
            )
    
    # Validate type completeness
    params = test_case["parameters"]
    param_types = test_case["parameter_types"]
    
    for param_name in params:
        if param_name not in param_types:
            raise ValueError(
                f"Missing type for parameter '{param_name}' in parameter_types"
            )
```

## Executor Integration

### Loading Language Executors

```python
def _load_executor(self) -> None:
    """Load the appropriate language executor."""
    from eiplgrader.languages.registry import LanguageRegistry
    
    registry = LanguageRegistry()
    executor_class = registry.get_executor(self.language)
    self.executor = executor_class()
```

### Executor Interface

```python
class LanguageExecutor(ABC):
    """Abstract base for language executors."""
    
    @abstractmethod
    def execute_test(
        self, 
        code: str, 
        test_case: Dict[str, Any]
    ) -> Dict[str, Any]:
        """
        Execute code with test case.
        
        Returns:
            Dict with keys: passed, actual, expected, error, function_call
        """
        pass
```

## In-Place Operation Handling

Support for functions that modify arguments:

```python
def _handle_inplace_mode(
    self, 
    test_case: Dict[str, Any], 
    execution_result: Dict[str, Any]
) -> Dict[str, Any]:
    """Handle different in-place modification modes."""
    inplace_mode = test_case.get("inplace", "0")
    
    if inplace_mode == "0":
        # Standard mode - check return value
        return execution_result
    
    elif inplace_mode == "1":
        # In-place only - check first parameter modification
        params = list(test_case["parameters"].values())
        if params:
            execution_result["actual"] = execution_result.get("modified_params", [None])[0]
        return execution_result
    
    elif inplace_mode == "2":
        # Both in-place and return - check both
        # Executor should handle this appropriately
        return execution_result
    
    else:
        raise ValueError(f"Invalid inplace mode: {inplace_mode}")
```

## Error Classification

### Structural Errors

Errors that prevent code from being tested:

```python
class StructuralError(Exception):
    """Code structure prevents testing."""
    pass

# Examples:
# - Missing function definition
# - Syntax errors
# - Import errors
# - Wrong function name
```

### Runtime Errors

Errors during test execution:

```python
class RuntimeError(Exception):
    """Error during test execution."""
    pass

# Examples:
# - Division by zero
# - Index out of bounds
# - Type errors
# - Null pointer exceptions
```

### Error Handler

```python
def _classify_error(self, error: Exception) -> str:
    """Classify error type for better reporting."""
    error_str = str(error)
    
    # Structural errors
    if any(marker in error_str for marker in [
        "SyntaxError", "IndentationError", "NameError",
        "ImportError", "ModuleNotFoundError"
    ]):
        return "structural"
    
    # Compilation errors (static languages)
    if "Compilation failed" in error_str:
        return "compilation"
    
    # Timeout errors
    if "timeout" in error_str.lower():
        return "timeout"
    
    # Default to runtime error
    return "runtime"
```

## Result Analysis

### Statistical Analysis

```python
class ResultAnalyzer:
    """Analyze test results for patterns."""
    
    def __init__(self, results: TestResults):
        self.results = results
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get statistical summary of results."""
        return {
            "total_tests": self.results.testsRun,
            "passed": self.results.successes,
            "failed": len(self.results.failures),
            "success_rate": self.results.successRate,
            "avg_execution_time": self._avg_execution_time(),
            "error_types": self._error_distribution()
        }
    
    def _avg_execution_time(self) -> float:
        """Calculate average execution time."""
        times = [r.execution_time for r in self.results.results 
                 if r.execution_time is not None]
        return sum(times) / len(times) if times else 0.0
    
    def _error_distribution(self) -> Dict[str, int]:
        """Analyze error type distribution."""
        errors = {}
        for failure in self.results.failures:
            if failure.error:
                error_type = self._classify_error_type(failure.error)
                errors[error_type] = errors.get(error_type, 0) + 1
        return errors
```

### Failure Pattern Detection

```python
def analyze_failure_patterns(results: TestResults) -> List[str]:
    """Detect common failure patterns."""
    patterns = []
    
    # Check for consistent type errors
    type_errors = [f for f in results.failures 
                   if "TypeError" in str(f.error)]
    if len(type_errors) > len(results.failures) * 0.5:
        patterns.append("Frequent type errors - check parameter types")
    
    # Check for edge case failures
    edge_failures = [f for f in results.failures 
                     if _is_edge_case(f.test_case)]
    if edge_failures:
        patterns.append("Edge case handling issues")
    
    # Check for timeout patterns
    timeout_failures = [f for f in results.failures 
                        if "timeout" in str(f.error).lower()]
    if timeout_failures:
        patterns.append("Performance issues - algorithm may be inefficient")
    
    return patterns
```

## Performance Optimization

### Parallel Test Execution

```python
class ParallelCodeTester(CodeTester):
    """Execute tests in parallel for performance."""
    
    def run_tests(self) -> TestResults:
        """Run tests in parallel."""
        import concurrent.futures
        
        with concurrent.futures.ThreadPoolExecutor(max_workers=4) as executor:
            # Submit all tests
            futures = []
            for test_case in self.test_cases:
                future = executor.submit(self._run_single_test, test_case)
                futures.append((future, test_case))
            
            # Collect results
            results = []
            for future, test_case in futures:
                try:
                    result = future.result(timeout=test_case.get('timeout', 30))
                    results.append(result)
                except concurrent.futures.TimeoutError:
                    result = TestResult(
                        test_case=test_case,
                        passed=False,
                        actual=None,
                        expected=test_case.get("expected"),
                        error="Test execution timed out"
                    )
                    results.append(result)
        
        return TestResults(results)
```

### Caching Compiled Code

For compiled languages, cache binaries:

```python
class CachedCompiledTester(CodeTester):
    """Cache compiled binaries for repeated tests."""
    
    _compilation_cache = {}
    
    def _get_cached_binary(self, code_hash: str) -> Optional[str]:
        """Get cached binary path if available."""
        return self._compilation_cache.get(code_hash)
    
    def _cache_binary(self, code_hash: str, binary_path: str) -> None:
        """Cache compiled binary."""
        self._compilation_cache[code_hash] = binary_path
```

## Custom Test Runners

### Benchmarking Test Runner

```python
class BenchmarkTester(CodeTester):
    """Test runner with performance benchmarking."""
    
    def run_tests(self) -> TestResults:
        """Run tests with benchmarking."""
        results = super().run_tests()
        
        # Add benchmark results
        for i, result in enumerate(results.results):
            if result.passed:
                # Run performance benchmark
                benchmark = self._benchmark_test(result.test_case)
                result.benchmark = benchmark
        
        return results
    
    def _benchmark_test(
        self, 
        test_case: Dict[str, Any], 
        iterations: int = 100
    ) -> Dict[str, float]:
        """Benchmark a single test case."""
        times = []
        
        for _ in range(iterations):
            start = time.perf_counter()
            self._run_single_test(test_case)
            times.append(time.perf_counter() - start)
        
        return {
            "min": min(times),
            "max": max(times),
            "avg": sum(times) / len(times),
            "median": sorted(times)[len(times) // 2]
        }
```

### Security-Enhanced Tester

```python
class SecureTester(CodeTester):
    """Enhanced security for code testing."""
    
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.sandbox = self._create_sandbox()
    
    def _create_sandbox(self) -> Sandbox:
        """Create isolated execution environment."""
        return Sandbox(
            memory_limit="256MB",
            cpu_limit=1.0,
            network_access=False,
            filesystem_access="readonly"
        )
    
    def _run_single_test(self, test_case: Dict[str, Any]) -> TestResult:
        """Run test in sandbox."""
        with self.sandbox:
            return super()._run_single_test(test_case)
```

## Integration with CI/CD

### GitHub Actions Integration

```python
class GitHubActionsTester(CodeTester):
    """CodeTester with GitHub Actions output format."""
    
    def run_tests(self) -> TestResults:
        """Run tests with GitHub Actions annotations."""
        results = super().run_tests()
        
        # Output GitHub Actions annotations
        for failure in results.failures:
            print(f"::error::Test failed: {failure.function_call}")
            print(f"::error::Expected: {failure.expected}")
            print(f"::error::Actual: {failure.actual}")
            if failure.error:
                print(f"::error::Error: {failure.error}")
        
        # Set output variables
        print(f"::set-output name=tests_run::{results.testsRun}")
        print(f"::set-output name=tests_passed::{results.successes}")
        print(f"::set-output name=success_rate::{results.successRate:.2%}")
        
        return results
```

## Testing the Tester

### Unit Tests

```python
class TestCodeTester(unittest.TestCase):
    """Test cases for CodeTester."""
    
    def test_simple_function(self):
        """Test basic function testing."""
        code = "def add(a, b): return a + b"
        test_cases = [
            {"parameters": {"a": 1, "b": 2}, "expected": 3},
            {"parameters": {"a": -1, "b": 1}, "expected": 0}
        ]
        
        tester = CodeTester(
            code=code,
            test_cases=test_cases,
            function_name="add",
            language="python"
        )
        
        results = tester.run_tests()
        
        self.assertTrue(results.allPassed)
        self.assertEqual(results.testsRun, 2)
        self.assertEqual(results.successes, 2)
    
    def test_failing_tests(self):
        """Test handling of test failures."""
        code = "def add(a, b): return a + b + 1"  # Wrong implementation
        test_cases = [
            {"parameters": {"a": 1, "b": 2}, "expected": 3}
        ]
        
        tester = CodeTester(code=code, test_cases=test_cases)
        results = tester.run_tests()
        
        self.assertFalse(results.allPassed)
        self.assertEqual(len(results.failures), 1)
        self.assertEqual(results.failures[0].actual, 4)
```

### Mock Testing

```python
class TestWithMocks(unittest.TestCase):
    """Test CodeTester with mocked executors."""
    
    @patch('eiplgrader.languages.registry.LanguageRegistry')
    def test_executor_loading(self, mock_registry):
        """Test that correct executor is loaded."""
        mock_executor = Mock()
        mock_registry.return_value.get_executor.return_value = mock_executor
        
        tester = CodeTester(
            code="code",
            test_cases=[],
            language="python"
        )
        
        mock_registry.return_value.get_executor.assert_called_with("python")
```

## Configuration

### Tester Configuration

```python
@dataclass
class TesterConfig:
    """Configuration for CodeTester."""
    default_timeout: int = 30
    max_output_length: int = 10000
    parallel_execution: bool = False
    sandbox_mode: bool = True
    collect_metrics: bool = False
    
    @classmethod
    def from_file(cls, path: str) -> 'TesterConfig':
        """Load configuration from file."""
        with open(path) as f:
            data = json.load(f)
        return cls(**data)
```

### Environment Variables

```python
# Load configuration from environment
TESTER_CONFIG = {
    "timeout": int(os.getenv("EIPLGRADER_TIMEOUT", "30")),
    "parallel": os.getenv("EIPLGRADER_PARALLEL", "false").lower() == "true",
    "sandbox": os.getenv("EIPLGRADER_SANDBOX", "true").lower() == "true"
}
```

## Next Steps

- Learn about [Language Executors](../languages/executors.md)
- Explore [Test Case Format](../../guide/test-cases.md)
- See [API Reference](../api/) for complete documentation
- Review [Testing Strategies](../testing.md)