---
layout: default
title: Tester Module
nav_order: 3
---

# Tester Module

The `tester.py` file contains the `CodeTester` class, which is used to run unit tests on code generated by the `CodeGenerator`.

## Classes

### `CodeTester`

This class is designed to facilitate the testing of functions using test cases.

#### Methods

- `__init__(code: str, test_cases: list)`
  - Initializes the tester with the code to be tested and the test cases.
  
- `test_user_function(args: list, expected_output: any)`
  - Tests the function `foo` using the given arguments and checks the output against the expected result.
  
- `run_tests(verbosity: int = 2, suppress_output: bool = False)`
  - Runs the test cases using the `unittest` framework. Optionally suppresses output.

Example usage:

```python
code_tester = CodeTester(generated_code, test_cases)
test_result = code_tester.run_tests()
```