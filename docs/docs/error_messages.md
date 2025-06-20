---
layout: default
title: Standardized Error Messages
nav_order: 6
---

# Standardized Error Messages

This document defines the standardized error messages used across all language executors in EiplGrader.

## Type Information Errors

### Missing Type Information

When required type information is not provided in test cases, executors should raise a `ValueError` with this standardized message:

```python
ValueError: Missing required type information:
- parameter_types not provided
- expected_type not provided

Test case must include:
{
    "parameters": {...},
    "parameter_types": {"param1": "type1", ...},
    "expected": ...,
    "expected_type": "type"
}
```

### Missing Parameter Type

When a specific parameter lacks type information:

```python
ValueError: Missing required type information:
- parameter_types['param_name'] not provided
```

### Invalid Type String

When an unrecognized type string is provided:

```python
ValueError: Invalid type string: 'unknown_type'
Supported types: int, double, float, string, bool, int[], double[], float[], string[], bool[]
```

## Compilation Errors

### General Compilation Failure

```
Compilation failed: [compiler output]
```

### Missing Compiler

```
Compilation failed: [compiler] not found. Please install [language] development tools.
```

## Runtime Errors

### Function Not Found

```
Function '[function_name]' not found in the code
```

### Execution Timeout

```
Execution timeout
```

### Runtime Exception

```
Runtime error: [exception details]
```

## Test Execution Errors

### Invalid Inplace Mode

```
Invalid inplace mode: [mode]
Valid modes: "0" (return value), "1" (modify in-place), "2" (both)
```

### Parameter Count Mismatch

```
Parameter count mismatch: function expects [expected] parameters, but [actual] provided
```

## Language-Specific Errors

### JavaScript

Missing Node.js:
```
Runtime error: node command not found. Please install Node.js.
```

### Java

Missing Gson (warning, not error):
```
Warning: Gson library not found. Using simplified JSON parsing.
```

### C/C++

Segmentation fault:
```
Runtime error: Segmentation fault (core dumped)
```

### Go

Module initialization:
```
Runtime error: go.mod file not found. Run 'go mod init' in the project directory.
```

### Haskell

Type mismatch:
```
Compilation failed: Couldn't match expected type '[expected]' with actual type '[actual]'
```

## Error Message Guidelines

### 1. Be Specific
- Include the exact parameter or field that's missing
- Show the expected format or value

### 2. Be Helpful
- Provide the correct format in the error message
- Suggest how to fix the problem

### 3. Be Consistent
- Use the same message format across all executors
- Maintain consistent capitalization and punctuation

### 4. Include Context
- Show relevant line numbers for compilation errors
- Include function names and parameter names

## Implementation Example

Here's how to implement standardized error checking in an executor:

```python
def validate_test_case(test_case):
    """Validate test case has required type information."""
    errors = []
    
    # Check for parameter_types
    if "parameter_types" not in test_case:
        errors.append("parameter_types not provided")
    
    # Check for expected_type
    if "expected_type" not in test_case:
        errors.append("expected_type not provided")
    
    # If errors found, raise standardized ValueError
    if errors:
        error_msg = "Missing required type information:\n"
        for error in errors:
            error_msg += f"- {error}\n"
        error_msg += "\nTest case must include:\n"
        error_msg += "{\n"
        error_msg += '    "parameters": {...},\n'
        error_msg += '    "parameter_types": {"param1": "type1", ...},\n'
        error_msg += '    "expected": ...,\n'
        error_msg += '    "expected_type": "type"\n'
        error_msg += "}"
        raise ValueError(error_msg)
    
    # Check each parameter has a type
    if "parameters" in test_case and "parameter_types" in test_case:
        for param_name in test_case["parameters"]:
            if param_name not in test_case["parameter_types"]:
                raise ValueError(
                    f"Missing required type information:\n"
                    f"- parameter_types['{param_name}'] not provided"
                )
```

## Error Response Format

When returning errors in test results, use this consistent format:

```python
{
    "passed": False,
    "error": "Standardized error message here",
    "actual": None,
    "expected": test_case.get("expected"),
    "function_call": "function_name(args)" if available else None
}
```

## Logging Errors

For debugging purposes, executors should log errors with appropriate detail:

```python
import logging

logger = logging.getLogger(__name__)

try:
    # Execute test
    result = execute_function(code, test_case)
except ValueError as e:
    # User error - log at INFO level
    logger.info(f"Test case validation error: {e}")
    raise
except Exception as e:
    # System error - log at ERROR level with traceback
    logger.error(f"Unexpected error during execution: {e}", exc_info=True)
    raise
```