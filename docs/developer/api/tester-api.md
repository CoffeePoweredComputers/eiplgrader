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
    code: Union[str, List[str]],
    test_cases: List[Dict[str, Any]],
    inplace: str = "0",
    function_name: str = "foo",
    language: str = "python"
)
```

#### Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `code` | `Union[str, List[str]]` | required | Code to test (can be single string or list of strings) |
| `test_cases` | `List[Dict]` | required | List of test case dictionaries |
| `inplace` | `str` | `"0"` | Test mode for in-place modifications |
| `function_name` | `str` | `"foo"` | Name of function to test |
| `language` | `str` | `"python"` | Programming language |

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

# With multiple code variants
tester = CodeTester(
    code=[code_variant1, code_variant2, code_variant3],
    test_cases=tests,
    function_name="solve",
    language="python"
)
```

### Methods

#### run_tests

```python
def run_tests(self) -> Union[CodeTestResult, List[CodeTestResult]]
```

Execute all test cases against the code.

##### Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|





##### Returns

`CodeTestResult` object (or list of them if code is a list) containing:

```python
class CodeTestResult:
    test_results: List[Dict[str, Any]]  # List of test result dictionaries
    successes: int                      # Number of successful tests
    failures: int                       # Number of failed tests  
    errors: int                         # Number of tests with errors
    
    def was_successful(self) -> bool:
        """Check if all tests passed."""
        return self.failures == 0 and self.errors == 0
    
    @property
    def testsRun(self) -> int:
        """Total number of tests run."""
        return len(self.test_results)
```

##### Example

```python
# Run tests
results = tester.run_tests()

# Check results
if results.was_successful():
    print("All tests passed!")
else:
    print(f"Tests run: {results.testsRun}")
    print(f"Successes: {results.successes}")
    print(f"Failures: {results.failures}")
    print(f"Errors: {results.errors}")
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
    def run_tests(self) -> Union[CodeTestResult, List[CodeTestResult]]:
        """Add custom test logic."""
        # Pre-test setup
        self.setup_environment()
        
        # Run tests with monitoring
        results = super().run_tests()
        
        # Post-test analysis
        self.analyze_performance(results)
        
        return results
    
    def setup_environment(self):
        """Custom setup logic."""
        pass
    
    def analyze_performance(self, results: CodeTestResult):
        """Analyze test performance."""
        slow_tests = [
            r for r in results.test_results 
            if r.get("execution_time", 0) > 1.0
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



#### Test Result Analysis

```python
def analyze_failures(results: CodeTestResult):
    """Analyze test failures for patterns."""
    failures = [r for r in results.test_results if not r["pass"]]
    
    # Group by error type
    error_groups = {}
    for failure in failures:
        error_type = failure.get("error_type", "unknown")
        if error_type not in error_groups:
            error_groups[error_type] = []
        error_groups[error_type].append(failure)
    
    # Report findings
    for error_type, group in error_groups.items():
        print(f"\n{error_type.upper()} errors ({len(group)}):")
        for failure in group[:3]:  # Show first 3
            print(f"  - Test: {failure['function_call']}")
            print(f"    Error: {failure.get('error', 'No error message')}")
```



### Security Considerations

The CodeTester provides built-in security through language-specific executors that run code in temporary directories with appropriate timeouts. Each language executor handles resource management and cleanup automatically.

### Debugging

#### Enable Debug Mode

```python
import logging

# Enable debug logging
logging.basicConfig(level=logging.DEBUG)

# Run with verbose output
results = tester.run_tests()

# Access detailed execution info
for result in results.test_results:
    if not result["pass"]:
        print(f"Failed test: {result['function_call']}")
        print(f"Stdout: {result.get('stdout', '')}")
        print(f"Stderr: {result.get('stderr', '')}")
        print(f"Error: {result.get('error', 'No error message')}")
```

#### Test Isolation

```python
def debug_specific_test(tester: CodeTester, test_index: int):
    """Debug a specific test case by creating a new tester with just that test."""
    if test_index >= len(tester.test_cases):
        print(f"Invalid test index: {test_index}")
        return
    
    test_case = tester.test_cases[test_index]
    
    print(f"Running test: {test_case}")
    print(f"Code:
{tester.code}")
    
    # Create a new tester with just this test case
    debug_tester = CodeTester(
        code=tester.code,
        test_cases=[test_case],
        function_name=tester.function_name,
        language=tester.language
    )
    
    result = debug_tester.run_tests()
    
    if result.test_results:
        test_result = result.test_results[0]
        print(f"Result: {'PASS' if test_result['pass'] else 'FAIL'}")
        print(f"Expected: {test_result.get('expected', 'N/A')}")
        print(f"Actual: {test_result.get('actual', 'N/A')}")
        if 'error' in test_result:
            print(f"Error: {test_result['error']}")
    
    return result
```

## See Also

- [CodeGenerator API](codegen-api.html) - For generating code to test
- [Language System API](language-api.html) - For language-specific details
- [Test Case Format](../guide/test-cases.html) - Detailed test case documentation