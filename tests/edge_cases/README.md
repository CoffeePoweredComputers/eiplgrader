# Edge Case and Error Resilience Testing

This directory contains comprehensive tests for edge cases, error scenarios, and boundary conditions in the EiplGrader system. The tests are designed to ensure the system handles failure modes gracefully and provides meaningful error messages.

## Test Categories

### 1. Error Scenarios (`test_error_scenarios/`)

#### Malformed Code (`test_malformed_code.py`)
- **Syntax errors**: Missing colons, brackets, semicolons
- **Missing functions**: Code without the expected function
- **Indentation errors**: Python-specific indentation issues
- **Runtime errors**: Division by zero, undefined variables
- **Wrong signatures**: Functions with incorrect parameter counts
- **Import errors**: Importing non-existent modules
- **Compilation errors**: Language-specific compilation failures

#### Invalid Inputs (`test_invalid_inputs.py`)
- **None/null inputs**: Handling of None values
- **Invalid data types**: Non-string/non-list code inputs
- **Malformed test cases**: Missing required keys, wrong formats
- **Empty collections**: Empty test case lists
- **Missing type information**: Static languages without type annotations
- **Unsupported languages**: Invalid language specifications
- **Special characters**: Unicode and special characters in function names

#### CGBG-Specific Failures (`test_cgbg_specific_failures.py`)
- **Multiple functions**: Generated code with multiple functions
- **No functions**: Code with no functions at all
- **Class definitions**: Classes instead of functions
- **Wrong return types**: Functions returning unexpected types
- **Side effects**: Functions with print statements or global modifications
- **Hardcoded outputs**: Functions that only work for specific inputs
- **Incomplete implementations**: Functions with TODO comments or pass statements
- **Mixed language syntax**: Code mixing multiple programming languages

#### Boundary Conditions (`test_boundary_conditions.py`)
- **Empty inputs**: Empty strings, lists, dictionaries
- **Zero values**: Division by zero, zero-length collections
- **Negative values**: Negative numbers and edge cases
- **Large values**: Very large numbers and data structures
- **Unicode handling**: International characters and emojis
- **Special float values**: NaN, infinity, negative infinity
- **Nested structures**: Deeply nested lists and dictionaries
- **Mixed data types**: Collections with multiple data types

### 2. Resource Limits (`test_resource_limits/`)

#### Timeout Scenarios (`test_timeout_scenarios.py`)
- **Infinite loops**: `while True` and infinite recursion
- **Long computations**: CPU-intensive calculations
- **Sleep operations**: Functions that sleep for extended periods
- **Nested loops**: Deeply nested iteration structures
- **Blocking I/O**: Network requests and file operations
- **Custom timeouts**: User-specified timeout values
- **Partial computations**: Timeouts during execution

#### Memory Scenarios (`test_memory_scenarios.py`)
- **Memory exhaustion**: Allocating excessive memory
- **Memory leaks**: Creating large numbers of objects
- **String explosion**: Very large string allocations
- **Recursive structures**: Deeply nested data structures
- **Dictionary expansion**: Large dictionary creation
- **File handle exhaustion**: Opening many files simultaneously
- **Thread exhaustion**: Creating too many threads
- **Stack overflow**: Deep recursion causing stack overflow

#### Security Boundaries (`test_security_boundaries.py`)
- **File system access**: Attempting to read/write files
- **Network access**: HTTP requests and socket connections
- **Subprocess execution**: Running system commands
- **Environment variables**: Accessing system environment
- **Module hijacking**: Modifying sys.modules
- **Code injection**: Using eval() and exec()
- **Memory dumps**: Accessing object memory
- **Reflection abuse**: Java reflection security bypasses

## Error Handling Principles

### Graceful Degradation
The system should:
1. **Never crash**: All errors should be caught and handled
2. **Provide meaningful messages**: Clear error descriptions
3. **Maintain state**: System should remain operational after errors
4. **Log appropriately**: Errors should be logged for debugging

### Error Categories
1. **Compilation Errors**: Syntax and compilation failures
2. **Runtime Errors**: Execution-time failures
3. **Timeout Errors**: Resource limit violations
4. **Security Errors**: Attempted security boundary violations
5. **Input Validation Errors**: Invalid input formats or values

### Language-Specific Considerations

#### Python
- Dynamic typing allows more runtime flexibility
- Syntax errors caught during compilation
- Exception handling with try/catch blocks
- Module import system security

#### Java
- Static typing requires explicit type declarations
- Compilation errors for type mismatches
- JVM security sandbox
- Reflection system controls

#### C/C++
- Manual memory management
- Compilation errors for syntax issues
- Buffer overflow protection
- System call restrictions

#### Go
- Garbage collection handles memory
- Compilation errors for unused variables
- Goroutine resource limits
- Package system security

#### JavaScript
- Dynamic typing with type coercion
- Runtime error handling
- Node.js security sandbox
- Module system protections

#### Haskell
- Strong static typing
- Lazy evaluation considerations
- Compilation error verbosity
- Pure functional constraints

## Testing Best Practices

### Test Structure
1. **Arrange**: Set up test data and conditions
2. **Act**: Execute the test scenario
3. **Assert**: Verify expected behavior (usually graceful failure)

### Error Verification
```python
# Test should verify that errors are handled gracefully
result = tester.run_tests()
assert not result.was_successful()  # Expected to fail
assert result.errors > 0  # Should have error messages
assert "timeout" in str(result.test_results[0]["error"]).lower()  # Specific error type
```

### Skip Conditions
Tests use `pytest.mark.skipif` to skip when languages aren't available:
```python
@pytest.mark.skipif(not language_registry.is_available("java"), 
                   reason="Java not available")
def test_java_specific_scenario(self):
    # Test only runs if Java is installed
```

## Running Edge Case Tests

### Run All Edge Case Tests
```bash
python -m pytest tests/edge_cases/ -v
```

### Run Specific Category
```bash
python -m pytest tests/edge_cases/test_error_scenarios/ -v
python -m pytest tests/edge_cases/test_resource_limits/ -v
```

### Run with Coverage
```bash
python -m pytest tests/edge_cases/ --cov=eiplgrader --cov-report=html
```

### Run Specific Test
```bash
python -m pytest tests/edge_cases/test_error_scenarios/test_malformed_code.py::TestMalformedCode::test_syntax_error_python -v
```

## Expected Behavior

### Successful Error Handling
- Tests should complete without crashing the test runner
- Error messages should be informative and actionable
- System should remain in a clean state after errors
- Resource cleanup should occur even after failures

### Performance Considerations
- Timeout tests should respect timeout limits
- Memory tests should not consume excessive system resources
- Resource cleanup should prevent test interference

### Security Assurance
- Security boundary tests should not compromise the system
- Malicious code attempts should be safely contained
- File system and network access should be controlled

## Contributing

When adding new edge case tests:

1. **Identify the failure mode**: What specific error condition are you testing?
2. **Choose the right category**: Error scenario, resource limit, or boundary condition?
3. **Write descriptive test names**: Clear indication of what's being tested
4. **Add language-specific variants**: Test the same scenario across languages
5. **Document expected behavior**: What should happen when this error occurs?
6. **Verify graceful handling**: Ensure the system doesn't crash or leak resources

## Continuous Integration

These tests are particularly important for CI/CD pipelines as they:
- Verify system stability under adverse conditions
- Catch regressions in error handling
- Ensure security boundaries remain intact
- Validate resource management across environments

The edge case test suite provides confidence that the EiplGrader system will handle real-world usage scenarios gracefully, even when students submit malformed code or when system resources are constrained.