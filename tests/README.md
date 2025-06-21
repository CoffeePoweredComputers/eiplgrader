# EiplGrader Comprehensive Test Suite

This directory contains a comprehensive test suite for EiplGrader's language adapters and executors, developed using a specialized three-agent approach.

## Test Suite Architecture

### Three-Agent Development Approach

The test suite was developed by three specialized agents, each with distinct personas and focus areas:

#### 🔬 **Agent 1: Type System Specialist**
- **Focus**: Language-specific type handling and execution patterns
- **Coverage**: Type inference vs explicit type requirements across all 7 languages
- **Location**: `tests/unit/test_executors/`, `tests/fixtures/mock_code_samples/`

#### 🏗️ **Agent 2: Execution Environment Engineer**  
- **Focus**: Infrastructure robustness and compilation/interpretation workflows
- **Coverage**: Registry system, adapter functionality, resource management
- **Location**: `tests/unit/test_adapters/`, `tests/integration/test_registry/`

#### 🛡️ **Agent 3: Edge Case & Error Resilience Tester**
- **Focus**: Failure modes and boundary conditions specific to CGBG workflows
- **Coverage**: Error handling, malformed inputs, resource limits, security boundaries
- **Location**: `tests/edge_cases/test_error_scenarios/`, `tests/edge_cases/test_resource_limits/`

## Directory Structure

```
tests/
├── conftest.py                           # Pytest configuration and fixtures
├── test_comprehensive_suite.py          # Unified test runner
├── README.md                            # This documentation
│
├── unit/                                # Unit tests for individual components
│   ├── test_adapters/
│   │   └── test_adapter_base.py         # All 7 language adapters
│   └── test_executors/
│       ├── test_c_executor.py           # C executor tests
│       ├── test_cpp_executor.py         # C++ executor tests  
│       ├── test_go_executor.py          # Go executor tests
│       ├── test_haskell_executor.py     # Haskell executor tests
│       ├── test_java_executor.py        # Java executor tests
│       ├── test_javascript_executor.py  # JavaScript executor tests
│       ├── test_python_executor.py      # Python executor tests
│       ├── test_base_executors.py       # Base executor infrastructure
│       ├── test_cross_language_consistency.py  # Cross-language validation
│       └── test_type_system_validation.py      # Type system testing
│
├── integration/                         # Integration tests
│   ├── test_registry/
│   │   └── test_language_registry.py    # Registry system tests
│   ├── test_compilation_workflows.py    # Compiled language workflows
│   └── test_interpretation_workflows.py # Interpreted language workflows
│
├── edge_cases/                          # Edge case and error testing
│   ├── test_error_scenarios/
│   │   ├── test_malformed_code.py       # Syntax errors, compilation failures
│   │   ├── test_invalid_inputs.py       # Input validation failures
│   │   ├── test_cgbg_specific_failures.py # CGBG workflow failures
│   │   ├── test_boundary_conditions.py  # Edge cases and limits
│   │   └── test_error_propagation.py    # Error handling consistency
│   └── test_resource_limits/
│       ├── test_timeout_scenarios.py    # Timeout and infinite loops
│       ├── test_memory_scenarios.py     # Memory exhaustion
│       ├── test_security_boundaries.py  # Security boundary testing
│       └── test_resource_management.py  # Resource cleanup and safety
│
└── fixtures/                            # Test data and mock samples
    ├── mock_code_samples/
    │   ├── c_samples.py                 # C code samples
    │   ├── cpp_samples.py               # C++ code samples
    │   ├── go_samples.py                # Go code samples
    │   ├── haskell_samples.py           # Haskell code samples
    │   ├── java_samples.py              # Java code samples
    │   ├── javascript_samples.py        # JavaScript code samples
    │   └── python_samples.py            # Python code samples
    └── test_case_templates.py           # Standard test case formats
```

## Language Coverage

### Type System Categories

#### 🔍 **Type Inference Languages** (Agent 1 Focus)
- **Python**: Full type inference, optional explicit types
- **JavaScript**: Full type inference, optional explicit types  
- **Go**: Type inference with JSON marshaling support

#### 📝 **Explicit Type Languages** (Agent 1 Focus)
- **C**: Requires exact C types (`int*`, `char*`, etc.)
- **C++**: Requires exact C++ types (`std::vector<int>`, `std::string`)
- **Java**: Requires exact Java types (`int[]`, `String`, `boolean`)
- **Haskell**: Requires exact Haskell types (`[Int]`, `String`, `Bool`)

## Running Tests

### Comprehensive Test Suite

```bash
# Run all tests
python -m pytest tests/ -v

# Run the unified comprehensive suite
python tests/test_comprehensive_suite.py --full-suite

# Run by test category
python tests/test_comprehensive_suite.py --category=type-system
python tests/test_comprehensive_suite.py --category=infrastructure  
python tests/test_comprehensive_suite.py --category=edge-cases
```

### Individual Test Categories

```bash
# Type system tests (Agent 1)
python -m pytest tests/unit/test_executors/ -m type_system -v

# Infrastructure tests (Agent 2)
python -m pytest tests/unit/test_adapters/ tests/integration/ -m infrastructure -v

# Edge case tests (Agent 3)
python -m pytest tests/edge_cases/ -m edge_cases -v
```

### Language-Specific Testing

```bash
# Test specific language executors
python -m pytest tests/unit/test_executors/test_python_executor.py -v
python -m pytest tests/unit/test_executors/test_java_executor.py -v

# Test cross-language consistency
python -m pytest tests/unit/test_executors/test_cross_language_consistency.py -v
```

### Coverage Reporting

```bash
# Run with coverage
python -m pytest tests/ --cov=eiplgrader --cov-report=html

# Generate coverage report
python -m pytest tests/ --cov=eiplgrader --cov-report=term-missing
```

## Test Categories & Markers

### Pytest Markers

- `@pytest.mark.type_system` - Type system functionality tests
- `@pytest.mark.infrastructure` - Infrastructure and registry tests
- `@pytest.mark.edge_cases` - Edge cases and error handling tests
- `@pytest.mark.integration` - Integration workflow tests
- `@pytest.mark.slow` - Slow-running tests (skippable)

### Test Selection

```bash
# Run only type system tests
python -m pytest -m type_system

# Run only infrastructure tests
python -m pytest -m infrastructure

# Skip slow tests
python -m pytest -m "not slow"

# Run integration tests only
python -m pytest -m integration
```

## Key Test Features

### 🚀 **No API Dependencies**
- All tests use mock code samples and responses
- No expensive LLM API calls required
- Fast execution suitable for CI/CD

### 🔄 **Cross-Language Consistency**
- Same logical functions tested across all 7 languages
- Consistent error handling validation
- Type system behavior verification

### 🛡️ **Comprehensive Error Handling**
- Malformed code scenarios for each language
- Resource limit and timeout testing
- Security boundary validation
- Graceful failure verification

### 📊 **Type System Validation**
- Type inference testing for Python/JS/Go
- Explicit type requirement validation for C/C++/Java/Haskell
- Type mapping consistency across languages
- Parameter vs return type validation

## CI/CD Integration

### GitHub Actions Configuration

The test suite is designed for CI/CD integration with:

```yaml
# Example GitHub Actions workflow
- name: Run Comprehensive Test Suite
  run: |
    python -m pytest tests/ --cov=eiplgrader --cov-report=xml
    python tests/test_comprehensive_suite.py --full-suite
```

### Test Environment Requirements

- **Python 3.10+**: Base runtime
- **Language Compilers**: gcc, g++, javac, ghc, go (for compiled language tests)
- **Node.js**: For JavaScript executor tests
- **Pytest**: Test runner with coverage support

### Continuous Integration Features

- **Parallel Test Execution**: Tests can run in parallel categories
- **Selective Testing**: Run only relevant tests based on changes
- **Coverage Reporting**: Comprehensive coverage metrics
- **Error Aggregation**: Detailed failure reporting and analysis

## Development Guidelines

### Adding New Languages

1. Create adapter in `eiplgrader/languages/adapters/`
2. Create executor in `eiplgrader/languages/executors/`
3. Add mock code samples in `tests/fixtures/mock_code_samples/`
4. Create executor tests in `tests/unit/test_executors/`
5. Update cross-language consistency tests
6. Add to registry system tests

### Test Development Best Practices

1. **Mock First**: Use mock code samples, not real API calls
2. **Cross-Language Thinking**: Consider how tests apply across languages
3. **Error Scenarios**: Always test failure modes
4. **Resource Safety**: Ensure proper cleanup in all tests
5. **Documentation**: Document test purpose and expected behavior

### Performance Considerations

- Individual test files should complete in < 30 seconds
- Full test suite should complete in < 5 minutes
- Use `@pytest.mark.slow` for tests > 5 seconds
- Consider parallelization for CPU-intensive tests

## Troubleshooting

### Common Issues

1. **Compiler Not Found**: Install language compilers (gcc, g++, javac, ghc, go)
2. **Timeout Issues**: Increase timeout values in test configuration
3. **Permission Errors**: Ensure write access to temporary directory
4. **Import Errors**: Check Python path and module imports

### Debug Mode

```bash
# Run tests in debug mode
python -m pytest tests/ -v -s --tb=long

# Run single test with detailed output
python -m pytest tests/unit/test_executors/test_python_executor.py::test_basic_execution -v -s
```

## Contributing

When contributing to the test suite:

1. Follow the three-agent architecture principles
2. Add tests to appropriate category directories
3. Use existing fixtures and conftest utilities
4. Ensure tests work without external API dependencies
5. Add comprehensive documentation for new test categories
6. Verify cross-language consistency for new features

## Summary

This comprehensive test suite provides:

- **400+ individual test cases** across all components
- **Complete language coverage** for all 7 supported languages
- **Infrastructure robustness** validation
- **Error resilience** verification
- **Type system** comprehensive testing
- **CI/CD ready** architecture
- **Performance optimized** execution

The three-agent development approach ensures comprehensive coverage while maintaining focused expertise in each testing domain.