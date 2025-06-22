---
layout: default
title: Error Handling
parent: User Guide
nav_order: 6
---

# Error Handling Guide

Understand and resolve common errors in EiplGrader across all supported languages.

## Error Categories

### 1. Generation Errors
Errors that occur during code generation from natural language.

### 2. Compilation Errors
Errors during compilation of generated code (static languages).

### 3. Runtime Errors
Errors during code execution and testing.

### 4. Configuration Errors
Errors related to setup and configuration.

## Generation Errors

### API Key Issues

**Error:**
```
OpenAI API key not found or invalid
```

**Solutions:**
```python
# Set via environment variable
import os
os.environ['OPENAI_API_KEY'] = 'your-key-here'

# Or pass directly
generator = CodeGenerator(api_key='your-key-here')
```

### Rate Limiting

**Error:**
```
Rate limit exceeded. Please try again in X seconds.
```

**Solutions:**
```python
import time

def generate_with_retry(generator, prompt, max_retries=3):
    for attempt in range(max_retries):
        try:
            return generator.generate_code(prompt)
        except RateLimitError as e:
            if attempt < max_retries - 1:
                wait_time = e.retry_after or 60
                print(f"Rate limited. Waiting {wait_time} seconds...")
                time.sleep(wait_time)
            else:
                raise
```

### Invalid Generation Type

**Error:**
```
ValueError: Unsupported generation type: 'invalid'. Supported types are 'cgbg' and 'redef'.
```

**Solution:**
```python
# Use correct generation type
result = generator.generate_code(
    student_response="description",
    gen_type="cgbg"  # or "redef"
)
```

### Model Not Available

**Error:**
```
Model 'gpt-5' not found. Available models: ['gpt-4o', 'gpt-3.5-turbo']
```

**Solution:**
```python
# Use available model
result = generator.generate_code(
    student_response="description",
    model="gpt-4o"  # Use available model
)
```

## Compilation Errors (Static Languages)

### Java Compilation Errors

**Error:**
```
Compilation failed: error: cannot find symbol
    symbol: class Scanner
    location: class Solution
```

**Solution:**
```python
# Ensure imports are included in generated code
result = generator.generate_code(
    student_response="reads input from user using Scanner",
    language="java"
)
# Generated code should include: import java.util.Scanner;
```

### C++ Template Errors

**Error:**
```
Compilation failed: error: no matching function for call to 'sort()'
```

**Solution:**
```python
# C++ needs proper includes
# Generated code should have:
# #include <algorithm>
# #include <vector>
```

### Missing Type Information

**Error:**
```
ValueError: Missing required type information:
- parameter_types not provided
- expected_type not provided
```

**Solution:**
```python
# For static languages, always provide types
test_case = {
    "parameters": {"x": 5, "y": 3},
    "parameter_types": {"x": "int", "y": "int"},  # Required!
    "expected": 8,
    "expected_type": "int"  # Required!
}
```

## Runtime Errors

### Timeout Errors

**Error:**
```
Execution timeout after 30 seconds
```

**Solutions:**
```python
# 1. Increase timeout for specific test
test_case = {
    "parameters": {"n": 1000000},
    "expected": "result",
    "timeout": 60  # 60 seconds
}

# 2. Optimize the algorithm
# Request more efficient implementation
result = generator.generate_code(
    "implements efficient binary search with O(log n) complexity"
)
```

### Memory Errors

**Error (C/C++):**
```
Runtime error: Segmentation fault (core dumped)
```

**Common Causes and Solutions:**
```c
// Problem: Array access out of bounds
int arr[5];
arr[10] = 0;  // Segmentation fault

// Solution: Check bounds
if (index >= 0 && index < size) {
    arr[index] = value;
}

// Problem: Null pointer dereference
char* str = NULL;
printf("%s", str);  // Segmentation fault

// Solution: Check for NULL
if (str != NULL) {
    printf("%s", str);
}
```

### Type Mismatch Errors

**Error (Python):**
```
TypeError: unsupported operand type(s) for +: 'int' and 'str'
```

**Solution:**
```python
# Ensure consistent types in test cases
test_cases = [
    {"parameters": {"x": 5, "y": 3}, "expected": 8},     # All integers
    {"parameters": {"x": "5", "y": "3"}, "expected": "53"}  # All strings
]
```

### Division by Zero

**Error:**
```
ZeroDivisionError: division by zero
RuntimeError: Arithmetic exception
```

**Solution:**
```python
# Include edge case tests
test_cases = [
    {"parameters": {"a": 10, "b": 2}, "expected": 5},
    {"parameters": {"a": 10, "b": 0}, "expected": None}  # Handle gracefully
]

# Request safe implementation
result = generator.generate_code(
    "divides a by b, returning None if b is zero"
)
```

## Language-Specific Errors

### Python Errors

**Import Errors:**
```
ImportError: No module named 'numpy'
```
- Solution: Standard library only, no external dependencies

**Indentation Errors:**
```
IndentationError: unexpected indent
```
- Solution: Generated code should use consistent 4-space indentation

### JavaScript Errors

**Async/Promise Errors:**
```
UnhandledPromiseRejectionWarning: TypeError: Cannot read property 'x' of undefined
```

**Solution:**
```javascript
// Ensure proper async handling
async function processData(data) {
    if (!data) return null;  // Guard clause
    return await someAsyncOperation(data);
}
```

### Java Errors

**Null Pointer Exception:**
```
java.lang.NullPointerException at Solution.processString
```

**Solution:**
```java
// Add null checks
public static String processString(String input) {
    if (input == null) return "";
    return input.toUpperCase();
}
```

### C/C++ Errors

**Array Size Issues:**
```c
// Error: Variable length array
int n = getUserInput();
int arr[n];  // Not allowed in C90

// Solution: Use dynamic allocation or fixed size
int* arr = malloc(n * sizeof(int));
// Remember to free(arr) later
```

### Go Errors

**Slice Bounds Error:**
```
panic: runtime error: index out of range [5] with length 3
```

**Solution:**
```go
// Check bounds before access
if index < len(slice) {
    value := slice[index]
}
```

### Haskell Errors

**Pattern Match Failure:**
```
*** Exception: Non-exhaustive patterns in function processList
```

**Solution:**
```haskell
-- Handle all cases
processList :: [Int] -> Int
processList [] = 0          -- Empty list case
processList (x:xs) = x + processList xs
```

## Test Case Errors

### Invalid Test Case Format

**Error:**
```
KeyError: 'parameters'
ValueError: Test case missing required field 'parameters'
```

**Solution:**
```python
# Ensure all required fields
test_case = {
    "parameters": {"x": 5},  # Required
    "expected": 10           # Required
}
```

### Type Annotation Errors

**Error (Java):**
```
ValueError: Invalid type 'string' for Java. Use 'String' instead.
```

**Solution:**
```python
# Use language-specific type names
java_types = {
    "parameter_types": {
        "name": "String",    # Not "string"
        "age": "int",        # Not "integer"
        "active": "boolean"  # Not "bool"
    }
}
```

## Debugging Strategies

### 1. Enable Verbose Output

```python
import logging
logging.basicConfig(level=logging.DEBUG)

# Now you'll see detailed execution logs
result = generator.generate_code(prompt)
```

### 2. Test Generated Code Separately

```python
# Extract and examine generated code
code = result["code"][0]
print("Generated code:")
print(code)
print("-" * 40)

# Test with simple case first
simple_test = {
    "parameters": {"x": 1},
    "expected": 2
}

tester = CodeTester(code=code, test_cases=[simple_test], ...)
```

### 3. Use Print Debugging

```python
# Request code with debug output
result = generator.generate_code(
    "that sorts a list and prints each step for debugging"
)
```

### 4. Incremental Testing

```python
# Start with simplest test case
test_cases = [
    {"parameters": {"x": 0}, "expected": 0},  # Edge case
]

# Add complexity gradually
if basic_test_passes:
    test_cases.append({
        "parameters": {"x": [1, 2, 3]}, 
        "expected": 6
    })
```

## Error Recovery Patterns

### Try Multiple Variants

```python
def try_variants(generator, prompt, test_cases, num_variants=5):
    """Try multiple code variants until one passes all tests."""
    
    result = generator.generate_code(prompt, num_to_gen=num_variants)
    
    for i, code in enumerate(result["code"]):
        try:
            tester = CodeTester(code=code, test_cases=test_cases)
            test_result = tester.run_tests()
            
            if test_result.allPassed:
                return {
                    "success": True,
                    "variant": i,
                    "code": code,
                    "results": test_result
                }
        except Exception as e:
            print(f"Variant {i} failed: {e}")
            continue
    
    return {"success": False, "error": "All variants failed"}
```

### Provide Better Context

```python
def enhance_prompt(original_prompt, error_info):
    """Enhance prompt based on previous errors."""
    
    enhanced = original_prompt
    
    if "division by zero" in str(error_info):
        enhanced += " (handle division by zero gracefully)"
    elif "null pointer" in str(error_info):
        enhanced += " (check for null/None inputs)"
    elif "index out of range" in str(error_info):
        enhanced += " (validate array indices)"
    
    return enhanced
```

### Fallback Strategies

```python
class GradingStrategy:
    def __init__(self, generator):
        self.generator = generator
    
    def grade_with_fallback(self, prompt, test_cases):
        strategies = [
            lambda: self._try_direct(prompt, test_cases),
            lambda: self._try_simplified(prompt, test_cases),
            lambda: self._try_with_hints(prompt, test_cases),
            lambda: self._try_different_model(prompt, test_cases)
        ]
        
        for strategy in strategies:
            try:
                result = strategy()
                if result["success"]:
                    return result
            except Exception as e:
                print(f"Strategy failed: {e}")
                continue
        
        return {"success": False, "error": "All strategies failed"}
```

## Common Solutions Reference

| Error Type | Common Cause | Quick Fix |
|------------|--------------|-----------|
| `ImportError` | Missing module | Use standard library only |
| `TypeError` | Type mismatch | Check test case types |
| `ValueError` | Invalid value | Add input validation |
| `IndexError` | Out of bounds | Check array/list size |
| `KeyError` | Missing key | Verify dict keys exist |
| `AttributeError` | Wrong method | Check object type |
| `SyntaxError` | Invalid syntax | Regenerate code |
| `TimeoutError` | Infinite loop | Add iteration limits |
| `MemoryError` | Large data | Optimize algorithm |
| `SegmentationFault` | Memory access | Check pointers/bounds |

## Getting Help

If you encounter persistent errors:

1. **Check the logs** - Enable debug logging
2. **Simplify the problem** - Test with minimal case
3. **Review examples** - See language quickstart guides
4. **File an issue** - GitHub issues with reproduction steps

## Next Steps

- Review [Test Case Format](test-cases.md) to avoid format errors
- See [Language Support](languages.md) for language-specific issues
- Explore [Advanced Features](advanced-features.md) for error recovery patterns